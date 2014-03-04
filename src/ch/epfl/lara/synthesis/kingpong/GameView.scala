package ch.epfl.lara.synthesis.kingpong

import scala.util.Try
import scala.collection.mutable.ConcurrentMap
import android.app.Activity
import android.view.SurfaceView
import android.view.MotionEvent
import android.view.SurfaceHolder
import android.view.Surface
import android.widget.SeekBar
import android.graphics.Canvas
import android.content.Context
import android.graphics.Bitmap
import android.graphics.Rect
import android.graphics.RectF
import android.graphics.Paint
import android.graphics.Matrix
import android.graphics.drawable.BitmapDrawable
import android.graphics.PorterDuffColorFilter
import android.graphics.PorterDuff
import android.graphics.PorterDuff.Mode
import android.graphics.drawable.Drawable
import android.util.Log
import android.util.AttributeSet
import android.os.Handler
import android.os.Vibrator
import android.hardware.SensorManager
import android.hardware.Sensor
import android.hardware.SensorEventListener
import android.hardware.SensorEvent
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.rules.Events._
import org.jbox2d.common.MathUtils
import android.widget.TextView
import android.text.style.BackgroundColorSpan
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import android.text.style.StyleSpan
import android.graphics.Typeface
import android.text.style.ForegroundColorSpan
import android.widget.ExpandableListView
import android.widget.ExpandableListAdapter

object GameView {
  sealed trait GameState
  case object Running extends GameState
  case object Editing extends GameState

  // 1 meter is equivalent to 100 pixels (with default zoom)
  val BOX2D_RATIO = 100

  val FINGERS = 10
  
  object V {
    def unapply(v: Vec2): Option[(Float, Float)] = Some((v.x, v.y))
  }
}

/**
 * Handler for the time bar to change the time displayed on it.
 */
trait ProgressBarHandler extends SeekBar.OnSeekBarChangeListener  {
  private var progressBar: SeekBar = null
  def setProgressBar(progressBar: SeekBar): Unit = {
    this.progressBar = progressBar
    if(progressBar != null) {
      progressBar.setMax(common.History.MAX_HISTORY_SIZE)
      progressBar.setProgress(0)
      progressBar.setSecondaryProgress(0)
      progressBar.setOnSeekBarChangeListener(this)
    }
  }
  def setProgressTime(t: Int) = {
    progressBar.setProgress(t)
    progressBar.setSecondaryProgress(t)
  }
  
  def onStartTrackingTouch(seekBar: SeekBar):Unit = {
  }
  def onStopTrackingTouch(seekBar: SeekBar):Unit = {
  }
}

/**
 * Handler for the action bar to add shapes to the world.
 */
trait ActionBarHandler extends common.ContextUtils {
  private var actionBar: ExpandableListView = _
  private var actionBarAdapter: ExpandableListAdapter = _
  def menuCallBacks: String => Unit
  
  var submenusLabels: IndexedSeq[IndexedSeq[String]] = _
  def setActionBar(actionBar: ExpandableListView): Unit = {
    this.actionBar = actionBar
    if(actionBar != null) {
      
      val menuLabels: IndexedSeq[String] = getStringArray(R.array.menu_arrays_hint)
      val menuDrawables = getArray(R.array.menu_arrays_drawable) map getDrawable
      val submenusDrawables = getArray(R.array.menu_arrays) map getDrawableArray
      submenusLabels = getArray(R.array.menu_arrays_strings) map getStringArray map {u: Array[String] => u:IndexedSeq[String]}
      
      val actions: IndexedSeq[String] = menuLabels
      val actionsCollection: Map[String, IndexedSeq[String]] = (menuLabels zip submenusLabels).toMap
      val bitmaps: Map[String, Drawable] = ((menuLabels ++ submenusLabels.flatten) zip (menuDrawables ++ submenusDrawables.flatten)).toMap
      val callbacks: String => Unit = menuCallBacks
      actionBarAdapter = new adapters.ActionsAdapter(context, actions, actionsCollection, bitmaps, callbacks)
      actionBar.setAdapter(actionBarAdapter)
    }
  }
}

class GameView(val context: Context, attrs: AttributeSet)
  extends SurfaceView(context, attrs) 
  with SurfaceHolder.Callback
  with ProgressBarHandler
  with ActionBarHandler
  with common.ContextUtils
  with Game
  {
  import GameView._
  import expression.Types._
  import common.Implicits._

  private var activity: Activity = null
  private var codeview: EditTextCursorWatcher = null
  private var grid: Grid = new Grid(step=1, offset=0, stroke_width=1, color=0x88000000)
  implicit val self: Game = this
  
  def menuCallBacks: String => Unit = { s =>
    s match {
      case Str(R.string.add_rectangle_hint) =>
        game.rectangle(DefaultCategory("rectangle", game))(name="rectangle", x=0, y=0, width=2*grid.step, height=grid.step)
      case Str(R.string.add_circle_hint) =>
        game.circle(DefaultCategory("circle", game))(name="circle", x=0, y=0, radius=grid.step)
      case _ =>
    }
  }
  
  EventHistory.addMethod("toGame", MethodDecl(TVec2, Val("toGame"), List(Formal(TVec2, Val("pos"))), stats=NOP, retExpr=List()).withFastImplementation(
      (l: List[Value]) =>
        mapVectorToGame(Vec2V(l))
  ))
  
  EventHistory.addMethod("fromGame", MethodDecl(TVec2, Val("fromGame"), List(Formal(TVec2, Val("pos"))), stats=NOP, retExpr=List()).withFastImplementation(
      (l: List[Value]) =>
        mapVectorFromGame(Vec2V(l))
  ))
  EventHistory.addMethod("snap", MethodDecl(TVec2, Val("snap"), List(Formal(TVec2, Val("pos"))), stats=NOP, retExpr=List()).withFastImplementation(
      (l: List[Value]) =>
        l match {
          case NumericV(i)::Nil => FloatV(grid.snap(i))
          case (v@Vec2V(x, y))::Nil => grid.snap(v)
          case NumericV(i)::NumericV(j)::Nil => grid.snap(Vec2V(i, j))
          case _ => throw new InterpreterException(s"Unable to snap value $l to grid. Should be a FloatV x1 or x2, or a Vec2V")
        }
  ))
  
  def toGame(e: Expr): Expr = {
    MethodCall("toGame", List(e))
  }
  
  def fromGame(e: Expr): Expr = {
    MethodCall("fromGame", List(e))
  }
  def snap(e: Expr): Expr = {
    MethodCall("snap", List(e))
  }
  
  /** All game stuff from the Game trait */
  val world = new PhysicalWorld(Vec2(0, 0))
  
  val menus = Category("Menus")()
  val FingerUps = CategoryInput("FingerUps", { case e: FingerUp => true case _ => false} )
  val FingerMoves = CategoryInput("FingerMoves", { case e: FingerMove => true case _ => false} )
  
  val moveMenu = activeBox(menus)(name="Move", x=0, y=0, radius=42, visible=false, picture="cross_move")
  //val moveRule1 = Block(// need to disambiguate moveMenu("obj") := ObjectLiteral(null),
  //    foreach(FingerUps)("fingerUp") {
  //  moveMenu("obj") := obj("fingerUp")("obj")  // Objects of the GameView, that is none.
  //})
  val moveRule2 = whenever(FingerMoveOver(moveMenu))(
    List(moveMenu("x"), moveMenu("y")) := List(moveMenu("x"), moveMenu("y")) + VecExpr(List(Val("dx"), Val("dy")))
  )
  // TODO : it currently updates their next state, not their current.
  val moveRule2bis = whenever(moveMenu("obj") =!= NULL)(
     List(moveMenu("obj")("x"), moveMenu("obj")("y")) := snap(toGame(List(moveMenu("x"), moveMenu("y"))))
  )
  
  //register(moveRule1)
  register(moveRule2)
  register(moveRule2bis)

  /** The game model currently rendered. */
  private var game: Game = null
  def setGame(g: Game) = {
    game = g
    game.setInstantProperties(state == Editing)
  }
  def getGame() = game
  def hasGame(): Boolean = game != null
  private var mWidth = 0
  private var mHeight = 0
  def initialize() = {
    // Find lower and upper bounds of the game, and set the viewing matrix to it.
    layoutResize()
    //updateCodeView(game.rules, game.objects)
    codeview.setText("")
  }
  
  def layoutResize() = {
    if(game != null) {
      val a = (Array(0f, 0f, 0f, 0f) /: game.objects) { case (a, obj) =>
        val aabb = obj.getAABB()
        val V(xmin, ymin) = aabb.lowerBound
        val V(xmax, ymax) = aabb.upperBound
        if(a(0) > xmin) a(0) = xmin
        if(a(1) > ymin) a(1) = ymin
        if(a(2) < xmax) a(2) = xmax
        if(a(3) < ymax) a(3) = ymax
        a
      }
      val before = new RectF(a(0), a(1), a(2), a(3))
      val after = new RectF(0, 0, mWidth, mHeight)
      matrix.setRectToRect (before, after, Matrix.ScaleToFit.CENTER)
      push(matrix)
    }
  }

  /** The main game loop that calls `update()` and `render()`. */
  private var gameLoop: Option[GameLoop] = None

  /** The transformation applied to the canvas. 
   *  Transforms from Box2D units (meters) to pixels.
   */
  private val matrix = new Matrix()

  /** The inverse transformation matrix from pixels to meters. */
  private val matrixI = new Matrix()

  /** The current game state. */
  private var _state: GameState = Editing
  private def state_=(s: GameState) = _state = s
  def state = _state

  /** Flag to know if the canvas is ready to draw on it. */
  private var isSurfaceCreated = false

  // Register to intercept events
  getHolder().addCallback(this)

  /** Called one time at the initialization phase by the Activity.
   */
  def setActivity(activity: Activity): Unit = {
    this.activity = activity
    EventHolder.setSensorManager(activity.getSystemService(Context.SENSOR_SERVICE).asInstanceOf[SensorManager])
  }
  
  // Testing section
  var obj_to_highlight: Set[GameObject] = Set.empty
  var codeMapping = Map[Int, Category]()
  var propMapping = Map[Int, Property[_]]()
  def setCodeDisplay(code: EditTextCursorWatcher): Unit = {
    this.codeview = code
    codeview.setOnSelectionChangedListener({ case (start, end) =>
      if(codeMapping != null) {
        codeMapping.get(start) match {
          case Some(category) if category != null =>
            obj_to_highlight = category.objects.toSet
          case Some(_) =>
            obj_to_highlight = Set.empty
          case None =>
            obj_to_highlight = Set.empty
            
        }
      }
      if(propMapping != null) {
        propMapping.get(start) match {
          case Some(p) => // Activate the menu corresponding to this kind of property;
            p.name match {
              case "x" | "y" => evaluate(Block(moveMenu("obj") := ObjectLiteral(p.parent),
                  List(moveMenu("x"), moveMenu("y")) := fromGame(List(p.parent.x.expr, p.parent.y.expr)),
                  moveMenu("visible") := true))
              case _ =>
            }
          case _ =>
        }
      }
    })
  }
  
  /** When the progress changes from the user. */
  def onProgressChanged(seekBar: SeekBar, progress: Int, fromUser: Boolean) = {
    if(fromUser && state == Editing) {
      if(seekBar.getProgress() > seekBar.getSecondaryProgress()) {
        seekBar.setProgress(seekBar.getSecondaryProgress())
      }
      val t = game.maxTime + progress - seekBar.getSecondaryProgress()
      Log.d("GameView",s"OnProgress up to $t")
      game.restore(t)
      //game.returnToTime(game.maxTime + seekBar.getProgress() - seekBar.getSecondaryProgress())
    }
  }
  

  /** Called by the activity when the game has to sleep deeply. 
   *  The state is changed to `Editing` and the game loop is stoped.
   */
  def onPause(): Unit = {
    Log.d("kingpong", "onPause()")
    state = Editing
    EventHolder.disableAccelerometer()
    stopLoop()
  }

  /** Called by the activity after a deep sleep. 
   *  The state is kept to `Editing` and the game loop is started.
   */
  def onResume(): Unit = {
    Log.d("kingpong", "onResume()")
    state = Editing
    EventHolder.enableAccelerometer()
    if (isSurfaceCreated) {   
      startLoop()
    }
  }

  /** Called by the activity when to progress bar is modified by the user. */
  def onProgressBarChanged(progress: Int): Unit = {
    
  }

  /** Change the current state to Editing. */
  def toEditing(): Unit = if (state == Running) {
    Log.d("kingpong", "toEditing()")
    state = Editing
    game.setInstantProperties(true)
    layoutResize()
  }

  /** Change the current state to Running. */
  def toRunning(): Unit = if (state == Editing) {
    Log.d("kingpong", "toRunning()")
    // Remove objects that have been created after the date.
    game.gc()
    game.setInstantProperties(false)
    state = Running
  }

  /** Reset the game to its initial state. */
  def backToBeginning(): Unit = {
    toEditing()
    game.reset()
  }

  def reset(newGame: Game): Unit = {
    game = newGame
  }

  override def update(): Unit = {
    super.update()
    state match {
      case Running =>
        game.update()
        setProgressTime(game.time.toInt)
      case Editing =>
        //TODO
    }
  }


  private val rectF = new RectF()
  private val paint = new Paint()
  paint.setAntiAlias(true)
  private val paintSelected = new Paint(paint)
  paintSelected.setStyle(Paint.Style.STROKE)
  paintSelected.setColor(color(R.color.selection))
  def render(canvas: Canvas): Unit = {
    canvas.drawRGB(0xFF, 0xFF, 0xFF)
    if(state == Editing) grid.drawOn(matrix, matrixI, canvas)
    canvas.save()
    canvas.setMatrix(matrix)
    
    paint.setStrokeWidth(mapRadiusI(1))
    paintSelected.setStrokeWidth(mapRadiusI(3))
    
    def drawObject(o: GameObject): Unit = {
      if(o.existsAt(game.time)) {
      o match {
      case r: Rectangle =>
        paint.setColor(r.color.get)
        if(!r.visible.get)
          paint.setAlpha(0x00)

        canvas.save()
        canvas.rotate(radToDegree(r.angle.get), r.x.get, r.y.get)
        canvas.drawRect(r.x.get - r.width.get/2, r.y.get - r.height.get/2, r.x.get + r.width.get/2, r.y.get + r.height.get/2, paint)
        if(obj_to_highlight contains r) canvas.drawRect(r.x.get - r.width.get/2, r.y.get - r.height.get/2, r.x.get + r.width.get/2, r.y.get + r.height.get/2, paintSelected)
        canvas.restore()

      case c: Circle => 
        paint.setColor(c.color.get)
        if(!c.visible.get)
          paint.setAlpha(0x00)
        canvas.drawCircle(c.x.get, c.y.get, c.radius.get, paint)
        if(obj_to_highlight contains c) canvas.drawCircle(c.x.get, c.y.get, c.radius.get, paintSelected)
        
      case b: Box[_] => 
        paint.setColor(b.color.get)
        //if(b == obj_to_highlight) paint.setAlpha(0x88)
        paint.setTextSize(b.height.get)
        if(b.className == "Box[Boolean]") {
          val c = b.value.get.asInstanceOf[Boolean]
          canvas.drawText(b.name.get, b.x.get + b.height.get*3/2, b.y.get, paint)
          canvas.drawRect(b.x.get, b.y.get, b.x.get + b.height.get, b.y.get + b.height.get, paint)
          if(c) {
            paint.setColor(0xFF00FF00)
          } else {
            paint.setColor(0xFFFF0000)
          }
          canvas.drawCircle(b.x.get - b.height.get/2, b.y.get - b.height.get/2, b.height.get/2, paint)
        } else {
          val value = b.name.get + ":" + b.value.get.toString
          canvas.drawText(value, b.x.get, b.y.get, paint)
          if(obj_to_highlight contains b) canvas.drawText(value, b.x.get, b.y.get, paint)
        }
      case j: Joystick =>
        paint.setColor(j.color.get) 
        paint.setAlpha(0x20)
        canvas.drawCircle(j.x.get, j.y.get, j.radius.get, paint)
        paint.setAlpha(0x40)
        canvas.drawCircle(j.x.get + j.relative_x.get, j.y.get + j.relative_y.get, j.radius.get/2, paint)
        if(obj_to_highlight contains j) canvas.drawCircle(j.x.get, j.y.get, j.radius.get, paintSelected)
        
      case r: Character =>
        paint.setColor(r.color.get)
        if(!r.visible.get)
          paint.setAlpha(0x00)
        canvas.save()
        canvas.rotate(radToDegree(r.angle.get), r.x.get, r.y.get)
        canvas.drawRect(r.x.get - r.width.get/2, r.y.get - r.height.get/2, r.x.get + r.width.get/2, r.y.get + r.height.get/2, paint)
        if(obj_to_highlight contains r) canvas.drawRect(r.x.get - r.width.get/2, r.y.get - r.height.get/2, r.x.get + r.width.get/2, r.y.get + r.height.get/2, paintSelected)
        canvas.restore()
        
      case r: ActiveBox =>
        if(r.visible.get) {
          canvas.save()
          canvas.rotate(radToDegree(r.angle.get), r.x.get, r.y.get)
          retrieveDrawable(r.picture.get) match {
            case Some(drawable) =>
              drawable.setBounds((r.x.get - r.radius.get).toInt, (r.y.get - r.radius.get).toInt, (r.x.get + r.radius.get).toInt, (r.y.get + r.radius.get).toInt)
              drawable.draw(canvas)
            case None =>
          }
          canvas.restore()
        }
    }}}
    if(game == null) return;
    game.objects foreach drawObject
    canvas.restore()
    this.objects foreach drawObject
    
    if(fingerIsDown) {
      paint.setColor(0xAAFF0000)
      canvas.drawCircle(currentFingerPos.x, currentFingerPos.y, game.FINGER_SIZE, paint)
    }
    /*
    game.world.beginContacts foreach { c =>
      paint.setColor(0xFFFF0000)
      canvas.drawCircle(c.point.x, c.point.y, mapRadiusI(10), paint)
    }

    game.world.currentContacts foreach { c =>
      paint.setColor(0xFF00FF00)
      canvas.drawCircle(c.point.x, c.point.y, mapRadiusI(10), paint)
    }

    game.world.endContacts foreach { c =>
      paint.setColor(0xFF0000FF)
      canvas.drawCircle(c.point.x, c.point.y, mapRadiusI(10), paint)
    }

    game.events.find(_.isInstanceOf[AccelerometerChanged]) match {
      case Some(e @ AccelerometerChanged(v)) =>
        paint.setStrokeWidth(mapRadiusI(2))
        paint.setColor(0xFFFF00FF)
        val pos = mapVectorToGame(Vec2(100, 100))
        canvas.drawLine(pos.x, pos.y, pos.x + v.x*5, pos.y + v.y*5, paint)
      case _ => //Do nothing
    }
    */
  }

  def surfaceChanged(holder: SurfaceHolder, format: Int, width: Int, height: Int): Unit = {
    Log.d("kingpong", "surfaceChanged()")
    mWidth = width
    mHeight = height
    computeTransformationMatrices()
  }

  def surfaceCreated(holder: SurfaceHolder): Unit = {
    Log.d("kingpong", "surfaceCreated()")
    isSurfaceCreated = true
    startLoop()
  }

  def surfaceDestroyed(holder: SurfaceHolder): Unit = {
    Log.d("kingpong", "surfaceDestroyed()")
    isSurfaceCreated = false
    stopLoop()
  }

  override def onAccelerometerChanged(vector: Vec2): Unit = {
    // do NOT touch the given vector! 
    // It is mutable for performance purposes.
    state match {
      case Running => 
        game.onAccelerometerChanged(vector.clone)
      case Editing =>
        super.onAccelerometerChanged(vector.clone)
    } 
  }
  
  var currentFingerPos: Vec2 = null
  var fingerIsDown = false

  override def onFingerDown(pos: Vec2): Unit = {
    state match {
      case Running => 
        val res = mapVectorToGame(pos)
        game.onFingerDown(res)
        currentFingerPos = res
        fingerIsDown = true
      case Editing =>
        super.onFingerDown(pos)
        //TODO: Add menus handling ?
    }
  }

  override def onFingerUp(pos: Vec2): Unit = state match {
    case Running => 
      val res = mapVectorToGame(pos)
      game.onFingerUp(res)
      fingerIsDown = false
      currentFingerPos = res
    case Editing =>
      // Select an object below if any and display the corresponding code
      val res = mapVectorToGame(pos)
      val objectsTouched = game.objectFingerAt(res)
      obj_to_highlight = objectsTouched.toSet
      val rulesConcerned = game.getRulesbyObject(objectsTouched)
      updateCodeView(rulesConcerned, objectsTouched)
      super.onFingerUp(pos)
  }
  
  def updateCodeView(rules: Iterable[Stat], objects: Iterable[GameObject]) = {
    val header = PrettyPrinterExtended.printGameObjectDef(objects)
    val all = PrettyPrinterExtended.print(rules, header + "\n")
    val r: CharSequence = all.c
    val mapping = all.map
    val mObjects = mapping.mObjects
    codeMapping = mapping.mPosCategories
    propMapping = mapping.mPropertyPos
    var rulesString = SyntaxColoring.setSpanOnKeywords(r, PrettyPrinterExtended.LANGUAGE_SYMBOLS, () => new StyleSpan(Typeface.BOLD), () => new ForegroundColorSpan(0xFF950055))
    objects.foreach { obj =>
      //expression.PrettyPrinterExtended.setSpanOnKeywords(rules, List(obj.name.get),  () => new BackgroundColorSpan(0xFF00FFFF))
      mObjects.get(obj.category) match {
        case Some(l) => l foreach { case (start, end) => rulesString = SyntaxColoring.setSpanOnBounds(rulesString, start, end, () => new BackgroundColorSpan(color(R.color.selection))) }
        case None => // No objects to color
      }
    }
    codeview.setText(rulesString)
  }
  
  def push(m: Matrix) = {
    m.invert(matrixI)
    
    grid = Grid(matrixI, width=mWidth, numSteps=15, stroke_width=1, color=0x88000000)
    if(game != null) game.FINGER_SIZE = matrixI.mapRadius(35)
  }

  override def onOneFingerMove(from: Vec2, to: Vec2): Unit = state match {
    case Running => 
      val res = mapVectorToGame(to)
      game.onOneFingerMove(mapVectorToGame(from), res)
      currentFingerPos = res
    case Editing =>
      //matrix.postTranslate(to.x - from.x, to.y - from.y)
      //push(matrix)
      super.onOneFingerMove(from, to)
  }

  def onTwoFingersMove(from1: Vec2, to1: Vec2, from2: Vec2, to2: Vec2): Unit = {

    val lengthFrom = from1.sub(from2).length
    val lengthTo = to1.sub(to2).length
    val scale = if (lengthFrom != 0) lengthTo / lengthFrom else 1f
    
    val d1 = from1.distance(to1)
    val d2 = from2.distance(to2)
    val p = if(d1+d2 != 0) d2/(d1+d2) else 0f

    matrix.postTranslate((to1.x + to2.x)/2 - (from1.x + from2.x)/2, (to1.y + to2.y)/2 - (from1.y + from2.y)/2)
    matrix.postScale(scale, scale, from1.x * p + from2.x * (1-p), from1.y * p + from2.y * (1-p))
    
    push(matrix)
  }

  /** meters to pixels */
  def mapVectorFromGame(p: Vec2): Vec2 = {
    val toMap = Array(p.x, p.y)
    matrix.mapPoints(toMap)
    Vec2(toMap(0), toMap(1))
  }
  
  /** meters to pixels */
  def mapVectorFromGame(p: Vec2V): Vec2V = {
    val toMap = Array(p.x, p.y)
    matrix.mapPoints(toMap)
    Vec2V(toMap(0), toMap(1))
  }
  

  /** pixels to meters */
  def mapVectorToGame(p: Vec2): Vec2 = {
    val toMap = Array(p.x, p.y)
    matrixI.mapPoints(toMap)
    Vec2(toMap(0), toMap(1))
  }
  
  
  /** meters to pixels */
  def mapVectorToGame(p: Vec2V): Vec2V = {
    val toMap = Array(p.x, p.y)
    matrixI.mapPoints(toMap)
    Vec2V(toMap(0), toMap(1))
  }

  /** meters to pixels */
  def mapRadius(r: Float): Float = matrix.mapRadius(r)

  /** pixels to meters */
  def mapRadiusI(r: Float): Float = matrixI.mapRadius(r)

  def radToDegree(r: Float): Float = r * MathUtils.RAD2DEG

  private def computeTransformationMatrices() = {
    if(game != null) layoutResize()
    else {
      matrix.reset() // identity matrix
      //matrix.postScale(1, -1); // upside-down
      matrix.postScale(BOX2D_RATIO, BOX2D_RATIO)
      push(matrix)
    }
  }

  /** Stop the thread loop. */
  private def stopLoop(): Unit = gameLoop match {
    case Some(loop) =>
      loop.requestStop()
      var retry = true
      while (retry) Try { 
        loop.join()
        retry = false
      }
      gameLoop = None

    case _ => //Do nothing
  }

  /** Start a fresh thread loop that will call `update()` and
   *  `render()`. 
   *  If a loop is already running, calling this 
   *  will have no effect. If the thread exists but is either 
   *  dead or not started, a new thread will be created and 
   *  started.
   */
  private def startLoop(): Unit = {
    if (!gameLoop.isDefined || 
        !gameLoop.get.isAlive) {
      gameLoop = Some(new GameLoop(getHolder, this))
      gameLoop.get.start()
    }
  }

  override def onTouchEvent(me: MotionEvent): Boolean = {
    EventHolder.onTouchEvent(me)
    true
  }

  /** Hold all touch events and pre-format them before dispatching them back. */
  private object EventHolder extends SensorEventListener {
    import EventHolder._

    private val accAlpha = 0.8f
    private val accLast = Vec2(0, 0)

    private var sensorManager: SensorManager = null
    private var accelerometer: Sensor = null

    def setSensorManager(manager: SensorManager): Unit = {
      sensorManager = manager
      accelerometer = sensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER)
    }

    def enableAccelerometer(): Unit = if (sensorManager != null && accelerometer != null) {
      Log.d("kingpong", "enableAccelerometer()")
      sensorManager.registerListener(this, accelerometer, SensorManager.SENSOR_DELAY_NORMAL)
    }

    def disableAccelerometer(): Unit = if (sensorManager != null && accelerometer != null) {
      Log.d("kingpong", "disableAccelerometer()")
      sensorManager.unregisterListener(this)
    }

    def onAccuracyChanged(sensor: Sensor, accuracy: Int): Unit = {}

    // Called when the accelerometer change
    def onSensorChanged(event: SensorEvent): Unit = {
      val x = accAlpha * accLast.x + (1 - accAlpha) * event.values(0)
      val y = accAlpha * accLast.y + (1 - accAlpha) * event.values(1)
      
      val rotation = activity.getWindowManager.getDefaultDisplay.getRotation
      /*rotation match {
        case Surface.ROTATION_0 =>
          onAccelerometerChanged(accLast.set(-x, y))
        case Surface.ROTATION_90 =>
          onAccelerometerChanged(accLast.set(-y, x))
        case Surface.ROTATION_180 =>
          onAccelerometerChanged(accLast.set(-x, -y))
        case Surface.ROTATION_270 =>
          onAccelerometerChanged(accLast.set(y, -x))
        case _ =>
      }*/ /// TODO : reuse later for performance reasons.
    }

    private val last = Array.fill(FINGERS)(Vec2(0, 0))
    
    def onTouchEvent(me: MotionEvent): Unit = {
      val action = me.getAction()
      (action & MotionEvent.ACTION_MASK) match {
        
        // A finger gets down.
        case MotionEvent.ACTION_DOWN | MotionEvent.ACTION_POINTER_DOWN =>
          val pointerIndex = (action & MotionEvent.ACTION_POINTER_INDEX_MASK) >> MotionEvent.ACTION_POINTER_INDEX_SHIFT
          val point = Vec2(me.getX(pointerIndex), me.getY(pointerIndex))
          onFingerDown(point)
          last(pointerIndex) = point

        // A finger moves
        case MotionEvent.ACTION_MOVE =>
          if (me.getPointerCount() == 1) {
            val pointerIndex = Math.min(me.getPointerId(0), FINGERS - 1)
            val from = last(pointerIndex)
            val to = Vec2(me.getX(0), me.getY(0))
            //Log.d("GameView", s"Moved from ${from.x}, ${from.y} to ${to.x}, ${to.y}")
            onOneFingerMove(from, to)
            last(pointerIndex) = to
            
          } else if (me.getPointerCount() == 2) {
            val pointerIndex1 = Math.min(me.getPointerId(0), FINGERS - 1)
            val pointerIndex2 = Math.min(me.getPointerId(1), FINGERS - 1)
            val from1 = last(pointerIndex1)
            val from2 = last(pointerIndex2)
            val to1 = Vec2(me.getX(0), me.getY(0))
            val to2 = Vec2(me.getX(1), me.getY(1))
            onTwoFingersMove(from1, to1, from2, to2)
            last(pointerIndex1) = to1
            last(pointerIndex2) = to2
          }

        case MotionEvent.ACTION_UP | MotionEvent.ACTION_POINTER_UP =>
          val pointerIndex = (action & MotionEvent.ACTION_POINTER_INDEX_MASK) >> MotionEvent.ACTION_POINTER_INDEX_SHIFT
          val point = Vec2(me.getX(pointerIndex), me.getY(pointerIndex))
          if(last(pointerIndex).x != point.x || last(pointerIndex).y != point.y)
            onOneFingerMove(last(pointerIndex), point)
          onFingerUp(point)
          last(pointerIndex) = point

        case _ => //Do nothing
      }
    }
  }

}