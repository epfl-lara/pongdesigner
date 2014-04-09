package ch.epfl.lara.synthesis.kingpong

import scala.util.Try
import scala.collection.mutable.ConcurrentMap
import android.app.Activity
import android.view.SurfaceView
import android.view.MotionEvent
import android.view.SurfaceHolder
import android.view.Surface
import android.view.View
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
import android.graphics.Path
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
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import android.text.style.StyleSpan
import android.graphics.Typeface
import android.text.style.ForegroundColorSpan
import android.widget.ExpandableListView
import android.widget.ExpandableListAdapter
import ch.epfl.lara.synthesis.kingpong.menus._
import ch.epfl.lara.synthesis.kingpong.common.History
import android.widget.Toast
import collection.mutable.{Set => MSet, HashMap => MMap}
import android.graphics.DashPathEffect
import android.content.res.Resources
import android.util.DisplayMetrics
import android.view.WindowManager
import android.graphics.LinearGradient
import android.graphics.Shader
import ch.epfl.lara.synthesis.kingpong.common.Messages._

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
  def setTimeProgressAbsolute(i: Long) = {
    progressBar.setProgress(Math.max(0, (i - History.MAX_HISTORY_SIZE).toInt))
  }
  def getGame: Game
}

/**
 * Handler for the action bar to add shapes to the world.
 */
trait ActionBarHandler extends common.ContextUtils {
  private var actionBar: ExpandableListView = _
  private var actionBarAdapter: adapters.ActionsAdapter = _
  def menuCallBacks: String => Boolean
  
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
      val bitmaps: MMap[String, Drawable] = MMap()
      bitmaps ++= ((menuLabels ++ submenusLabels.flatten) zip (menuDrawables ++ submenusDrawables.flatten))
      val callbacks: String => Boolean = menuCallBacks
      actionBarAdapter = new adapters.ActionsAdapter(context, actions, actionsCollection, bitmaps, callbacks)
      actionBar.setAdapter(actionBarAdapter)
      
      actionBar.setOnGroupClickListener(new ExpandableListView.OnGroupClickListener() {
          def onGroupClick(parent: ExpandableListView, v: View, groupPosition: Int, id: Long) = {
            menuCallBacks(actions(groupPosition))
          }
      });
      
    }
  }
  
  def changeMenuIcon(position: String, drawable: Drawable) = {
    actionBarAdapter.bitmaps(position) = drawable
    actionBar.invalidate()
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
  
  def menuCallBacks: String => Boolean = { s =>
    s match {
      case Str(R.string.add_rectangle_hint) =>
        game.rectangle(DefaultCategory("rectangle", game))(name="rectangle", x=0, y=0, width=2*grid.step, height=grid.step)
        true
      case Str(R.string.add_circle_hint) =>
        game.circle(DefaultCategory("circle", game))(name="circle", x=0, y=0, radius=grid.step)
        true
      case Str(R.string.add_drawing_object_hint) =>
        val d = game.drawingObject(DefaultCategory("drawingobjects", game))(name="drawingZone", x=0, y=0, width=4*grid.step, height=4*grid.step)
        game.addRule(d.defaultRule(game))
        true
        
      case Str(R.string.menu_add_constraint_hint) =>
        var menuSelected = false
        val res = context.getResources()
        val game = getGame()
        mRuleState match {
          case STATE_MODIFYING_GAME => 
            setModeSelectEvents()
            

            menuSelected = true
            //mAddRuleButton.text = res.getString(R.string.select_event)
            Toast.makeText(context, res.getString(R.string.select_event_toast), 2000).show()
          case STATE_SELECTING_EVENTS =>
            Toast.makeText(context, res.getString(R.string.rule_canceled), 2000).show()
            setModeModifyGame()
            //mAddRuleButton.text = res.getString(R.string.design_rule)
            //hovered = false
            eventEditor.select(null, 0)
          case STATE_SELECTING_EFFECTS =>
            if(eventEditor.selectedEvent != null) { // Means that the rule has been confirmed.
              val causeEvent =  eventEditor.selectedEvent.getOrElse(null)
              val timestamp = eventEditor.selectedTime
              CodeGenerator.createRule(context, getGame(), Set(causeEvent), Set(), timestamp)
            //}
            /*if(!EditRuleButton.selected) {
              setModeModifyGame(false)
            }*/
          }
          //hovered = false
            true
        case STATE_MODIFYING_CATEGORY =>
          //hovered = false
      }
      menuSelected
      case _ =>
        false
    }
  }
  
//  EventHistory.addMethod("toGame", MethodDecl(TVec2, Val("toGame"), List(Formal(TVec2, Val("pos"))), stats=NOP, retExpr=List()).withFastImplementation(
//      (l: List[Value]) =>
//        mapVectorToGame(Vec2V(l))
//  ))
//  
//  EventHistory.addMethod("fromGame", MethodDecl(TVec2, Val("fromGame"), List(Formal(TVec2, Val("pos"))), stats=NOP, retExpr=List()).withFastImplementation(
//      (l: List[Value]) =>
//        mapVectorFromGame(Vec2V(l))
//  ))
//  EventHistory.addMethod("snap", MethodDecl(TVec2, Val("snap"), List(Formal(TVec2, Val("pos"))), stats=NOP, retExpr=List()).withFastImplementation(
//      (l: List[Value]) =>
//        l match {
//          case NumericV(i)::Nil => FloatV(grid.snap(i))
//          case (v@Vec2V(x, y))::Nil => grid.snap(v)
//          case NumericV(i)::NumericV(j)::Nil => grid.snap(Vec2V(i, j))
//          case _ => throw new InterpreterException(s"Unable to snap value $l to grid. Should be a FloatV x1 or x2, or a Vec2V")
//        }
//  ))
  
  def toGame(e: Expr): Expr = {
    MethodCall("toGame", List(e))
  }
  
  def fromGame(e: Expr): Expr = {
    MethodCall("fromGame", List(e))
  }
  /*def snap(e: Expr): Expr = {
    MethodCall("snap", List(e))
  }*/
  def snapX(i: Float): Float = grid.snap(i)
  def snapY(i: Float): Float = grid.snap(i)
  
  var gameEngineEditors: List[GameEngineEditor] = _
  def addGameEngineEditor(g: GameEngineEditor) = {
    if(gameEngineEditors == null) {
      gameEngineEditors = List(g)
    } else {
      gameEngineEditors = gameEngineEditors ++ List(g)
    }
  }
  /** Selected objects, events and rules */
  var shapeEditor = new ShapeEditor(this)
  var eventEditor = new EventEditor(this)
  /*var categoryEditor = new CategoryEditor(this)
  var ruleEditor = new RuleEditor(this)
  var numberEditor = new NumberEditor(this)
  var systemEditor = new SystemEditor(this)
  var gameEditor = new GameEditor(this)*/
  
  final val STATE_MODIFYING_GAME = 0 // All effects are applied immediately
  final val STATE_SELECTING_EVENTS = 1 // Only events can be selected 
  final val STATE_SELECTING_EFFECTS = 2 // Effects are applied only on the future state, the previous state remains the same.
  final val STATE_MODIFYING_CATEGORY = 3 // Objects are selected to be included to a given category
  var mRuleState = STATE_MODIFYING_GAME
  def setModeSelectCategory() = {
    mRuleState = STATE_MODIFYING_CATEGORY
    MenuOptions.copy_to_prev = true
    MenuOptions.modify_prev = false
  }
  
  /** Switches the current mode to selecting effects */
  def setModeSelectEffects(): Unit = {
    mRuleState = STATE_SELECTING_EFFECTS
    MenuOptions.copy_to_prev = false
    MenuOptions.modify_prev = false
    Toast.makeText(context, Str(R.string.select_effects_toast), 2000).show()
  }
  
  /** Switches the current mode to selecting events. */
  def setModeSelectEvents() = {
    mRuleState = STATE_SELECTING_EVENTS
    gameEngineEditors foreach (_.unselect())
    MenuOptions.modify_prev = false // irrelevant
    MenuOptions.copy_to_prev = true // irrelevant
    changeMenuIcon(Str(R.string.menu_add_constraint_hint), bitmaps(R.drawable.menu_rule_maker))
  }
  
  /** Switches the current mode to the global modification of the game */
  def setModeModifyGame(resetView: Boolean = true) {
    mRuleState = STATE_MODIFYING_GAME
    MenuOptions.modify_prev = false
    MenuOptions.copy_to_prev = true
    changeMenuIcon(Str(R.string.menu_add_constraint_hint), bitmaps(R.drawable.menu_rule_editor))
    //EditRuleButton.selected = false
    //enterEditMode()
    //AddRuleButton.hovered = false
    eventEditor.unselect()
  }

  /** All game stuff from the Game trait */
  val world = new PhysicalWorld(Vec2(0, 0))
  
  val menus = Category("Menus")()
  //val FingerUps = CategoryInput("FingerUps", { case e: FingerUp => true case _ => false} )
  //val FingerMoves = CategoryInput("FingerMoves", { case e: FingerMove => true case _ => false} )
  
  val moveMenu = activeBox(menus)(name="Move", x=0, y=0, radius=42, visible=false, picture="cross_move")
  
  var whitePaint = new Paint()
  whitePaint.setColor(0xFFFFFFFF)
  whitePaint.setStyle(Paint.Style.FILL_AND_STROKE)
  whitePaint.setStrokeWidth(1)
  whitePaint.setAntiAlias(true)
  
//  val moveRule2 = whenever(FingerMoveOver(moveMenu))(
//    List(moveMenu("x"), moveMenu("y")) := List(moveMenu("x"), moveMenu("y")) + VecExpr(List(Val("dx"), Val("dy")))
//  )
//  // TODO : it currently updates their next state, not their current.
//  val moveRule2bis = whenever(moveMenu("obj") =!= ObjectLiteral(null))(
//     List(moveMenu("obj")("x"), moveMenu("obj")("y")) := snap(toGame(List(moveMenu("x"), moveMenu("y"))))
//  )
  
//  register(moveRule2)
//  register(moveRule2bis)

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
  
  def pickImage(): Unit = {
    this.activity.asInstanceOf[KingPong] ! PickImage()
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
//      if(propMapping != null) {
//        propMapping.get(start) match {
//          case Some(p) => // Activate the menu corresponding to this kind of property;
//            p.name match {
//              case "x" | "y" => evaluate(Block(moveMenu.obj := ObjectLiteral(p.parent),
//                  List(moveMenu.x, moveMenu.y) := fromGame(List(p.parent.x.expr, p.parent.y.expr)),
//                  moveMenu("visible") := true))
//              case _ =>
//            }
//          case _ =>
//        }
//      }
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
    //computeMatrix()
    
    gameEngineEditors foreach (_.onExitEditMode())

    //CategoryGroup.unselect()
    setModeModifyGame()
    //mAddRuleButton.text = res.getString(R.string.design_rule)
    MenuCenter.registeredMenusCenters foreach {
      _.menus.foreach { _.hovered = false}
    }
    
    //timeTracker.unpause(game.currentTime - game.maxTime)
    /*if(enteredEditTime != 0) {
      totalNonPlayedTime += System.currentTimeMillis() - enteredEditTime - (game.currentTime - game.maxTime)
      enteredEditTime = 0
    }*/
    state = Running
  }

  /** Reset the game to its initial state. */
  def backToBeginning(): Unit = {
    toEditing()
    setProgressTime(0)
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
    
    paint.setStyle(Paint.Style.FILL)
    paint.setStrokeWidth(mapRadiusI(3))
    paintSelected.setStrokeWidth(mapRadiusI(3))
    
    def drawObject(o: GameObject): Unit = {
      if(o.existsAt(game.time)) {
        
        o match {
          case o: Positionable =>
            val colorPrev = o.color.get
            val colorNext = o.color.next
            val visible_prev = o.visible.get
            val visible_next = o.visible.next
            if(colorPrev != colorNext || visible_prev != visible_next) {
              val cPrev = if(visible_prev) colorPrev else (colorPrev & 0xFFFFFF) + (((colorPrev >>> 24)*0.5).toInt << 24)
              val cNext = if(visible_next) colorNext else (colorNext & 0xFFFFFF) + (((colorNext >>> 24)*0.5).toInt << 24)
              paint.setShader(new LinearGradient(o.left.get, o.y.get, o.right.get, o.y.get, Array(cPrev, cPrev,cNext,cNext), Array(0, 0.4375f,0.5625f,1), Shader.TileMode.MIRROR));
            } else {
              paint.setColor(colorPrev)
              if (!visible_prev)
                paint.setAlpha(0x00)
            }
        }
        

        o match {
        case d : DrawingObject =>
          canvas.save()
          canvas.rotate(radToDegree(d.angle.get), d.x.get, d.y.get)
          paint.setStyle(Paint.Style.STROKE)
          if(state == Editing) {
            canvas.drawRect(d.left.get, d.top.get, d.right.get, d.bottom.get, paint)
          }
          d.drawings foreach { case DrawingElement(time, from, to, width, color) =>
            if(time <= game.time) {
              paint.setColor(color)
              paint.setStrokeWidth(width)
              canvas.drawLine(from.x, from.y, to.x, to.y, paint)
            }
          }
          
          if (obj_to_highlight contains d) 
            canvas.drawRect(d.left.get, d.top.get, d.right.get, d.bottom.get, paintSelected)
          canvas.restore()
          
        case r: Rectangle =>
          canvas.save()
          canvas.rotate(radToDegree(r.angle.get), r.x.get, r.y.get)
          canvas.drawRect(r.left.get, r.top.get, r.right.get, r.bottom.get, paint)
          
          if (obj_to_highlight contains r) 
            canvas.drawRect(r.left.get, r.top.get, r.right.get, r.bottom.get, paintSelected)
          canvas.restore()
  
        case c: Circle => 
          canvas.drawCircle(c.x.get, c.y.get, c.radius.get, paint)
          if (obj_to_highlight contains c) 
            canvas.drawCircle(c.x.get, c.y.get, c.radius.get, paintSelected)
        
        case arr: Array2D =>
          paint.setStyle(Paint.Style.STROKE)
          canvas.drawRect(arr.left.get, arr.top.get, arr.right.get, arr.bottom.get, paint)
          if (obj_to_highlight contains arr) 
            canvas.drawRect(arr.left.get, arr.top.get, arr.right.get, arr.bottom.get, paintSelected)
          
          // reset the paint style
          paint.setStyle(Paint.Style.FILL)
                  
        case cell: Cell =>
          paint.setStyle(Paint.Style.STROKE)   
          canvas.drawRect(cell.left.get, cell.top.get, cell.right.get, cell.bottom.get, paint)
          if (obj_to_highlight contains cell) 
            canvas.drawRect(cell.left.get, cell.top.get, cell.right.get, cell.bottom.get, paintSelected)
          
          // reset the paint style
          paint.setStyle(Paint.Style.FILL)
            
        case b: Box[_] => 
          //if(b == obj_to_highlight) paint.setAlpha(0x88)
          paint.setTextSize(b.height.get)
          if(b.className == "Box[Boolean]") {
            val c = b.value.get.asInstanceOf[Boolean]
            val h = b.height.get
            val x = b.x.get
            val y = b.y.get
            canvas.drawText(b.name.get, x + h*3/2, y, paint)
            canvas.drawRect(x, y - h/2, x + h, y + h/2, paint)
            if(c) {
              paint.setColor(0xFF00FF00)
            } else {
              paint.setColor(0xFFFF0000)
            }
            canvas.drawCircle(x + h/2, y, h/2, paint)
          } else {
            val value = b.name.get + ":" + b.value.get.toString
            canvas.drawText(value, b.x.get, b.y.get, paint)
            if(obj_to_highlight contains b) canvas.drawText(value, b.x.get, b.y.get, paint)
          }
        case j: Joystick =>
          paint.setAlpha(0x20)
          canvas.drawCircle(j.x.get, j.y.get, j.radius.get, paint)
          paint.setAlpha(0x40)
          canvas.drawCircle(j.x.get + j.relative_x.get, j.y.get + j.relative_y.get, j.radius.get/2, paint)
          if(obj_to_highlight contains j) canvas.drawCircle(j.x.get, j.y.get, j.radius.get, paintSelected)
          
        case r: Character =>
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
        }
        
        o match {
          case e: Positionable =>
            val c = e.color.get
            if(c >>> 24 == 0 && (bitmaps contains c))  { // It's a picture
              canvas.restore()
              
              val d = bitmaps(c)
              val leftTop = mapVectorFromGame(Vec2(e.left.get, e.top.get))
              val rightBottom = mapVectorFromGame(Vec2(e.right.get, e.bottom.get))
              
              d.setBounds(leftTop.x.toInt, leftTop.y.toInt, rightBottom.x.toInt, rightBottom.y.toInt)
              d.draw(canvas)
              
              canvas.save()
              canvas.setMatrix(matrix)
            }
        }
        //Draw a small red dot in the (x, y) position of each object
        /*paint.setColor(0xFFFF0000)
        o match {
        case o: Movable =>
          canvas.drawCircle(o.x.get, o.y.get, mapRadiusI(3), paint)
        case _ =>
        }*/
      }
    }
  
    /** Draw a velocity vector from the given point */
    def drawVelocity(o: GameObject, x: Float, y: Float, velocity: Vec2, paint: Paint) {
      o match { //canvas: Canvas, v: Float, x: Float, y: Float, vx: Float, vy: Float, paint: Paint
        case o: Speed with Movable =>
          val middleX = x
          val middleY = y
          val v = velocity.length()
          if(v != 0) {
            val vx = velocity.x
            val vy = velocity.y
            val toX = middleX + vx * 1
            val toY = middleY + vy * 1
            canvas.drawLine(middleX, middleY, toX, toY, paint)
            val unitVectorX = vx / v
            val unitVectorY = vy / v
            val cosRot = -0.87f * 0.5f
            val sinRot = 0.5f * 0.5f
            canvas.drawLine(toX, toY, toX + cosRot*unitVectorX + sinRot*unitVectorY, toY - sinRot*unitVectorX + cosRot*unitVectorY, velocityPaint)
            canvas.drawLine(toX, toY, toX + cosRot*unitVectorX - sinRot*unitVectorY, toY + sinRot*unitVectorX + cosRot*unitVectorY, velocityPaint)
          }
        case _ =>
      }
    }
    
    if(game == null) return;
    game.objects foreach drawObject
    if(state == Editing) game.objects foreach {
      case o: Speed with Movable =>
        drawVelocity(o, o.x.next, o. y.next, o.velocity.next, velocityPaint)
        if(o.velocity.next.x != o.velocity.get.x || o.velocity.next.y != o.velocity.get.y) {
          drawVelocity(o, o.x.get, o.y.get, o.velocity.get, velocityPaintShaded)
        }
      case _ =>
    }
    canvas.restore()
    //this.objects foreach drawObject
    
    if(fingerIsDown) {
      paint.setColor(0xAAFF0000)
      canvas.drawCircle(currentFingerPos.x, currentFingerPos.y, game.FINGER_SIZE, paint)
    }
    if(mRuleState == STATE_SELECTING_EVENTS) {
      game.foreachEvent{ (event, time) => drawEventOn(event, time, canvas) }
      // Divide the luminosity by two
      canvas.drawColor(0xFF808080, PorterDuff.Mode.MULTIPLY)
      // Add a quarter of the luminosity
      canvas.drawColor(0xFF404040, PorterDuff.Mode.ADD)
    }
    drawMenuOn(canvas)
  }
  
  /** Menu drawing */
  private val res: Resources = context.getResources()
  val bitmaps = new MMap[Int, Drawable]()
  val drawables_to_load:List[Int] =
    List(R.drawable.finger,
        R.drawable.bing,
        R.drawable.numbers,
        R.drawable.flat_button,
        R.drawable.flat_button_highlighted,
        R.drawable.flat_button_selected,
        R.drawable.flat_button_selected_highlighted,
        R.drawable.flat_button_m1,
        R.drawable.flat_button_p1,
        R.drawable.cross_move,
        R.drawable.move_velocity,
        R.drawable.move_size,
        R.drawable.nail,
        R.drawable.nail_big,
        R.drawable.trashcan,
        R.drawable.eye,
        R.drawable.menu_paint,
        R.drawable.none,
        R.drawable.menu_add_rect,
        R.drawable.menu_add_circle,
        R.drawable.menu_add_digit,
        R.drawable.menu_add_text,
        R.drawable.modify_text,
        R.drawable.plus,
        R.drawable.menu_add_accelerometer,
        R.drawable.menu_add_force_field,
        R.drawable.menu_rule_editor,
        R.drawable.menu_rule_maker,
        R.drawable.prev_effects,
        R.drawable.next_effects,
        R.drawable.menu_camera,
        R.drawable.existing_rules,
        R.drawable.flat_button_resizable,
        R.drawable.flat_button_resizable_highlighted,
        //R.drawable.cursor_square,
        R.drawable.fingerdown_button,
        R.drawable.fingermove_button,
        R.drawable.fingerup_button,
        //R.drawable.menu_rule_apply,
        //R.drawable.no_collision,
        //R.drawable.no_collision_effect,
        R.drawable.collision_effect,
        R.drawable.outscreen,
        R.drawable.fingerdown,
        R.drawable.fingerup,
        R.drawable.fingermove,
        R.drawable.gear,
        R.drawable.back_arrow,
        R.drawable.jpeg
        //R.drawable.timebutton3
        )
  drawables_to_load.foreach { id =>
    bitmaps(id) = res.getDrawable(id)
  }
  
  private val fingerDrawable: Drawable = bitmaps(R.drawable.finger)
  
  var selectPaint = new Paint()
  selectPaint.setStrokeWidth(4)
  selectPaint.setColor(0xAAFFFFFF)
  selectPaint.setStyle(Paint.Style.STROKE)
  selectPaint.setAntiAlias(true)
  var circlePaint = new Paint()
  circlePaint.setColor(0xFF000000)
  circlePaint.setStyle(Paint.Style.STROKE)
  circlePaint.setStrokeWidth(4)
  circlePaint.setAntiAlias(true)
  var touchDownPaint = new Paint()
  touchDownPaint.setColor(0xAAFF0000)
  touchDownPaint.setStyle(Paint.Style.FILL_AND_STROKE)
  touchDownPaint.setStrokeWidth(2)
  touchDownPaint.setAntiAlias(true)
  var touchUpPaint = new Paint()
  touchUpPaint.set(touchDownPaint)
  touchUpPaint.setColor(0xAA00FF00)
  var touchMovePaint = new Paint()
  touchMovePaint.set(touchDownPaint)
  touchMovePaint.setColor(0xAAFFFF00)
  var touchSelectedPaint = new Paint()
  touchSelectedPaint.set(touchDownPaint)
  touchSelectedPaint.setStyle(Paint.Style.STROKE)
  touchSelectedPaint.setColor(0xAAFFFFFF)
  touchSelectedPaint.setStrokeWidth(4)
  var distancePaint = new Paint()
  distancePaint.set(touchMovePaint)
  distancePaint.setPathEffect(new DashPathEffect(Array[Float](5.0f,5.0f), 0))
  var velocityPaint = new Paint()
  velocityPaint.setColor(0x88FF00FF)
  velocityPaint.setStyle(Paint.Style.STROKE)
  velocityPaint.setStrokeWidth(0.1f)
  velocityPaint.setAntiAlias(true)
  var velocityPaintShaded = new Paint()
  velocityPaintShaded.set(velocityPaint)
  velocityPaintShaded.setPathEffect(new DashPathEffect(Array[Float](5.0f,5.0f), 0))
  final val cross_size = 15
  var rectFData = new RectF(0, 0, 0, 0)
  var rectData = new Rect(0, 0, 0, 0)
  
  // Draw events in the GameView referential
  def drawEventOn(event: Event, timestamp: Int, canvas: Canvas): Unit = {
    var paint: Paint = this.paint
    val eventIsSelected = eventEditor.selectedEvent.exists(_ == event)
    event match {
      case e if eventIsSelected =>
        if(event.isFinger) paint = touchSelectedPaint
      case FingerRelated(_) =>
        paint = touchDownPaint
      case _ =>
    }
    val dtime = (game.time - timestamp) * (if(event.isInstanceOf[FingerDown]) -1 else 1)
    val power = if(eventIsSelected) 1f else (if(dtime < 0) 0f else (300-Math.min(Math.max(dtime, 0), 300))/300f)
    var finger: List[(Float, Float, Int)] = Nil // The last int is the opacity
    val alpha = (0xEA*power + 0x20*(1-power)).round.toInt
    if(paint != null ) {
      // Emphasis for all events that appeared in the past.
      paint.setStrokeWidth((5*power + 2 * (1-power)).round.toInt)
      paint.setAlpha(alpha)
    }
    event match {
      case BeginContact(c) => 
        paint.setColor(0xFFFF0000)
        paint.setAlpha(alpha)
        val p = mapVectorFromGame(c.point)
        rectFData.set(p.x - 24, p.y - 21, p.x + 25, p.y + 21)
        rectFData.round(rectData)
        val bitmap = if(Some(event) == eventEditor.selectedEvent) bitmaps(R.drawable.bingselected) else  bitmaps(R.drawable.bing)
        bitmap.setBounds(rectData)
        bitmap.setAlpha(alpha)
        canvas.drawCircle(p.x, p.y, mapRadiusI(10), paint) // TODO : Delete
        bitmap.draw(canvas)
      case CurrentContact(c) => 
        paint.setColor(0xFF00FF00)
        paint.setAlpha(alpha)
        val p = mapVectorFromGame(c.point)
        canvas.drawCircle(p.x, p.y, mapRadiusI(10), paint)
      case EndContact(c) => 
        paint.setColor(0xFF0000FF)
        paint.setAlpha(alpha)
        val p = mapVectorFromGame(c.point)
        canvas.drawCircle(p.x, p.y, mapRadiusI(10), paint)
      case AccelerometerChanged(v) =>
        paint.setStrokeWidth(mapRadiusI(2))
        paint.setColor(0xFFFF00FF)
        paint.setAlpha(alpha)
        val pos = mapVectorToGame(Vec2(100, 100))
        canvas.drawLine(pos.x, pos.y, pos.x + v.x*5, pos.y + v.y*5, paint)
      case FingerUp(v, obj) =>
        val p = mapVectorFromGame(v)
        canvas.drawCircle(p.x, p.y, cross_size, circlePaint)
        canvas.drawCircle(p.x, p.y, cross_size, paint)
        if(timestamp == game.time) finger = (p.x, p.y, 0xBA)::finger
      case FingerDown(v, obj) =>
        val p = mapVectorFromGame(v)
        drawCross(canvas, p.x, p.y, paint)
        if(timestamp == game.time) finger = (p.x, p.y, 0xBA)::finger
      case FingerMove(v, v2, obj) =>
        val p = mapVectorFromGame(v)
        val p2 = mapVectorFromGame(v2)
        if(event == eventEditor.selectedEvent) {
          drawCross(canvas, p.x, p.y, touchSelectedPaint)
          canvas.drawCircle(p2.x, p2.y, 15, touchSelectedPaint)
        } else {
          if(timestamp == game.time) finger = (p2.x, p2.y, 0xBA)::finger
        }
        canvas.drawLine(p.x, p.y, p2.x, p2.y, paint)
      case _ => //Do nothing
    }
    def recDrawFinger(l: List[(Float, Float, Int)]): Unit = l match {
      case Nil =>
      case (x, y, alpha)::q => drawFinger(canvas, x, y, alpha)
        recDrawFinger(q)
    }
    recDrawFinger(finger)
  }

  
  /** Draws a cross at the given position */
  def drawCross(canvas: Canvas, x: Float, y: Float, paint: Paint) = {
    canvas.drawLine(x - cross_size, y - cross_size, x + cross_size, y + cross_size, paint)
    canvas.drawLine(x - cross_size, y + cross_size, x + cross_size, y - cross_size, paint)
  }
  def drawFinger(canvas: Canvas, x: Float, y: Float, alpha: Int) = {
    val width = 44
    val height = 64
    val xc = 8
    val yc = 63
    val left = (x - xc).toInt
    val top = (y - yc).toInt
    fingerDrawable.setBounds(left, top, left + width - 1, top + height - 1)
    fingerDrawable.setAlpha(alpha)
    fingerDrawable.draw(canvas)
  }
  
  val metrics = new DisplayMetrics();    
  context.getSystemService(Context.WINDOW_SERVICE).asInstanceOf[WindowManager].getDefaultDisplay().getMetrics(metrics);    
  lazy val button_size:Float = if(5 * 80 * metrics.density >= Math.min(metrics.widthPixels, metrics.heightPixels)) 40 * metrics.density else 80 * metrics.density
  
  /** Draws the menu on the canvas */
  def drawMenuOn(canvas: Canvas) = {
    // Draw the fps on the top right of the screen
    //mFPSPaint.setTextSize(20)
    //canvas.drawText("fps:" + fps.toString(), mWidth - 90, 20, mFPSPaint)
    MenuOptions.button_size = button_size
    state match {
      case Editing => 
        eventEditor.selectedEvent match {
          case Some(event) => drawEventOn(event, eventEditor.selectedTime.toInt, canvas)
          case _ =>
        }
        //StaticMenu.draw(canvas, this, shapeEditor.selectedShape, bitmaps, button_size/2, button_size/2)
        //if(game.currentTime == 0)
        //GameMenu.draw(canvas, this, shapeEditor.selectedShape, bitmaps, 0, 0)
      case Running =>
    }
    if(shapeEditor.selectedShape != null) {
      val (x, y) = shapeEditor.selectedShape match {
        case selectedShape: Movable =>
          if(MenuOptions.modify_prev) {
            (selectedShape.x.get,
             selectedShape.y.get)
          } else {
            (selectedShape.x.next,
             selectedShape.y.next)
          }
        case _ =>
          (0f, 0f)
      }
      val p = mapVectorFromGame(Vec2(x, y))
      val selectionx = Math.min(Math.max(p.x, button_size), mWidth - button_size*3.5f)
      val selectiony = Math.min(Math.max(p.y, button_size*1.5f), mHeight-button_size*1.5f)
      
      val cx = selectionx
      val cy = selectiony
      
      // Display the shape's menu.
      ShapeMenu.draw(canvas, this, shapeEditor.selectedShape, bitmaps, cx, cy)
      if(ColorMenu.activated && ColorMenu.registeredAction == None) {
        ColorMenu.draw(canvas, this, shapeEditor.selectedShape, bitmaps, PaintButton.getX(), PaintButton.getY())
      }
      if(SystemMenu.activated) {
        SystemMenu.draw(canvas, this, shapeEditor.selectedShape, bitmaps, SystemButton.getX(), SystemButton.getY())
      }
    }
    eventEditor.selectedEvent match {
      case Some(SelectableEvent(x, y)) =>
        val p = mapVectorFromGame(Vec2(x, y))
        EventMenu.draw(canvas, this, shapeEditor.selectedShape, bitmaps, p.x, p.y)
      case _ =>
    }
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
  var fingerUpCanceled = false
  var touchDownOriginal = Vec2(0, 0)
  var touchDownOriginalGame = Vec2(0, 0)
  var selectedShapeCoords = Vec2(0 ,0)
  var mDisplacementX: Float = 0
  var mDisplacementY: Float = 0

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
        fingerUpCanceled = false
        val x = pos.x
        val y = pos.y
        var return_value = highLightMenu(x, y)
        val p = mapVectorToGame(pos)
        touchDownOriginalGame = p
        touchDownOriginal = pos
        shapeEditor.selectedShape match {
          case selectedShape: Movable =>
              if(MenuOptions.modify_prev) {
                selectedShapeCoords = selectedShape.center.get
              } else {
                selectedShapeCoords = selectedShape.center.next
              }
              MenuOptions.selected_shape_first_x = selectedShapeCoords.x
              MenuOptions.selected_shape_first_y = selectedShapeCoords.y
          case _ =>
        }
        if(MoveButton.hovered) {
          mDisplacementX = 0
          mDisplacementY = 0
        }
    }
  }
  
  /** Highlights the corresponding menu under the finger
   * Returns true if a menu has been highlighted.
   **/
  def highLightMenu(x: Float, y: Float): Boolean = {
    var stg_hovered = false
    gameEngineEditors foreach { editor =>
      if(editor.isVisible && !stg_hovered) {
        stg_hovered = editor.testHovering(x, y, button_size)
        if(stg_hovered && Options.Access.showTooltips) {
          editor.fireTooltips(context)
        }
      }
    }
    stg_hovered
  }
  
  /**
   * Selects objects
   */
  var previousSelectedShape: GameObject = null
  def performSelection(res: Vec2) = {
    val objectsTouched = game.objectFingerAt(res)
    obj_to_highlight = objectsTouched.toSet
    val rulesConcerned = game.getRulesbyObject(objectsTouched)
    updateCodeView(rulesConcerned, objectsTouched)
    shapeEditor.unselect()
    // The selected shape should be the first after the previousSelectedShape,
    // or the closest else.
    var afterPreviousSelectedShape = false
    var shapeToSelect: GameObject = null
    var minDistance = -1f
    def checkShapeToSelect(shape: GameObject) = {
      if(previousSelectedShape != null && !afterPreviousSelectedShape) {
        if(previousSelectedShape == shape) {
           afterPreviousSelectedShape = true
        }
      } else { // We are after the previously selected shape, so we can check the distance to the finger.
        shape match {
          case shape: Movable =>
            val x = res.x + (if(MenuOptions.modify_prev) shape.x.next - shape.x.get else 0)
            val y = res.y + (if(MenuOptions.modify_prev) shape.y.next - shape.y.get else 0)// - shape.y + shape.prev_y
            //if(shape.selectableBy(x, y)) {
              val dist = shape.distanceSelection(x, y)
              if(dist < minDistance || minDistance == -1) {
                minDistance = dist
                shapeToSelect = shape
              }
            //}
          case _ =>
        }
      }
    }
    
    objectsTouched foreach checkShapeToSelect
    if(shapeToSelect == null) {
      if(afterPreviousSelectedShape) {
        objectsTouched foreach checkShapeToSelect
      } else {
        previousSelectedShape = null
        objectsTouched foreach checkShapeToSelect
      }
    }
    if(shapeToSelect != null) {
      shapeEditor.select(shapeToSelect)
      previousSelectedShape = shapeToSelect
    }
  }

  /**
   * Called when a finger is up
   */
  override def onFingerUp(pos: Vec2): Unit = state match {
    case Running => 
      val res = mapVectorToGame(pos)
      game.onFingerUp(res)
      fingerIsDown = false
      currentFingerPos = res
    case Editing =>
      // Select an object below if any and display the corresponding code
      val res = mapVectorToGame(pos)

      var menuSelected:Boolean = false
      //numberEditor.unselect()
      ColorMenu.activated = false
      SystemMenu.activated = false
      //if(fingerUpCanceled) return
      //GameMenu.onFingerUp(this, selectedShape, x, y)
      val x = pos.x
      val y = pos.y
      if(shapeEditor.selectedShape != null) {
        menuSelected = ShapeMenu.onFingerUp(this, shapeEditor.selectedShape, x, y)
        //if(ColorMenu.activated) {
          //ColorMenu.onFingerUp(this, selectedShape, x, y)
        //}
      }
      if(!menuSelected && EventMenu.isActivated) {
        menuSelected = EventMenu.onFingerUp(this, shapeEditor.selectedShape, x, y)
      }
      /*if(!menuSelected && ruleEditor.selectedRule != null && mRuleState != STATE_SELECTING_EVENTS) { // Test is top left menu activated
        menuSelected = RuleMenu.onFingerUp(this, shapeEditor.selectedShape, x, y)
      }*/
      /*if(!menuSelected) { // Test is top left menu activated
        menuSelected = StaticMenu.onFingerUp(this, shapeEditor.selectedShape, x, y)
      }*/
      
      /*if(EditRuleButton.selected) {
        ruleEditor.updateSelectedRule()
      }*/
      /*for(action <- ruleEditor.selectedRuleActions) { // Convert this to a menu ?
        action.hovered = false
      }*/
  
      if(!menuSelected) {
        // If the previous attempts to select a menu failed,
        // We dissmiss all selection and we select something else depending on the game editor type.
        ShapeMenu.cancelHovered()
        ColorMenu.cancelHovered()
        SystemMenu.cancelHovered()
        //RuleMenu.cancelHovered()
        
        val touchCoords = res
        
        if(mRuleState == STATE_SELECTING_EVENTS) {
          
          // Detects objects and events.
          
          // If we are making a rule, we select the events first
          var oneShapeSelectable = false
          var closestTimeStampDiff: Long = -1
          var closestSpaceDistance: Float = -1
          var closestEvent: Event = null
          var closestEventTime: Int = 0
          eventEditor.select(null, 0)
          def updateClosest(i: Event, timestamp: Int): Unit = {
            i match {
              case FingerDown(_,_) |  FingerUp(_,_) |  FingerMove(_,_,_) | BeginContact(_) |  CurrentContact(_) |  EndContact(_) => 
                //canvas.drawCircle(i.x1, i.y1, 20, touchMovePaint)
                if(i.selectableBy(res.x, res.y)) {
                  val distTime = Math.abs(timestamp - game.time)
                  val distSpace = i.distanceSquareTo(res.x, res.y)
                  if(distTime < closestTimeStampDiff || (distTime == closestTimeStampDiff && (distSpace < closestSpaceDistance || closestSpaceDistance == -1)) || closestTimeStampDiff == -1) {
                    closestEvent = i
                    closestEventTime = timestamp
                    if(distTime < closestTimeStampDiff || closestSpaceDistance == -1) {
                      closestSpaceDistance = distSpace
                    }
                    closestTimeStampDiff = distTime
                  }
                }
              case _ =>
            }
          }
          game.foreachEvent(updateClosest(_, _))
          //game.triggerEvents foreach (updateClosest(_))
          
          // Try to get number events even if no event on it
          if(closestEvent == null) {
            performSelection(res)
            if(shapeEditor.selectedShape != null) {
              shapeEditor.selectedShape match {
                case d:Box[Int] if d.className == "Box[Int]" =>
                  //TODO : val p = game.triggerEvents.addEvent(game.currentTime, INTEGER_CHANGE_EVENT, d, null, d.x, d.y, d.value, d.value)
                  //closestEvent = p
                case d: Positionable =>
                  val p  = game.addEvent(FingerDown(Vec2(d.x.get, d.y.get), MSet(d: GameObject)), game.time.toInt)
                  val p2 = game.addEvent(FingerUp(Vec2(d.x.get, d.y.get), MSet(d: GameObject)), game.time.toInt)
                  closestEvent = p
              }
              shapeEditor.select(null)
            }
          }
          
          // If we got an event:
          if(closestEvent != null) {
            //game.storeInitialState(game.currentTime == 0)
            //game.prepareNewValues()
            if(closestEvent.isNumerical) {
              /*EventMenu.activate(false)
              val d = closestEvent.value.shape1.asInstanceOf[IntegerBox]
              val name = d.mName
              val newValue = closestEvent.value.y2.toInt
              val integer = new Integer(newValue)
              CustomDialogs.launchChoiceDialog(context, String.format(res.getString(R.string.choose_trigger), name),
                  List(
                    String.format(res.getString(R.string.trigger_integer_1), name),
                    String.format(res.getString(R.string.trigger_integer_2), name, integer),
                    String.format(res.getString(R.string.trigger_integer_3), name, integer),
                    String.format(res.getString(R.string.trigger_integer_4), name, integer))
                    ++ (
                      if(newValue != 0) {
                        if(newValue > 0) {
                          List(String.format(res.getString(R.string.trigger_integer_5), name),
                               String.format(res.getString(R.string.trigger_integer_7), name))
                        } else if(newValue < 0) {
                          List(String.format(res.getString(R.string.trigger_integer_6), name),
                               String.format(res.getString(R.string.trigger_integer_8), name))
                        } else Nil
                      } else Nil)
                    ,
                       {i:Int =>
                         closestEvent.value.code = INTEGER_CHANGE_EVENT + i
                         if(i == 4) {
                           if(newValue > 0) {
                             closestEvent.value.code = INTEGER_POSITIVE_EVENT
                           } else if(newValue == 0) {
                             // Normally this should not happen
                             throw new Exception("newValue == 0 but the point 4 was chosen")
                           } else {
                             closestEvent.value.code = INTEGER_NEGATIVE_EVENT
                           }
                           
                         } else if(i == 5) {
                            if(newValue > 0) {
                             closestEvent.value.code = INTEGER_GREATER_EQUAL_EVENT
                             closestEvent.value.y2 = 0
                           } else if(newValue == 0) {
                             // Normally this should not happen
                             throw new Exception("newValue == 0 but the point 4 was chosen")
                           } else {
                             closestEvent.value.code = INTEGER_LESS_EQUAL_EVENT
                             closestEvent.value.y2 = 0
                           }
                         }
                         eventEditor.select(closestEvent)
                         setModeSelectEffects()
                       },
                       {() =>})*/
            } else if(closestEvent.isCrossing) {
              /*val d = closestEvent.value.shape1
              val name = d.mName
              EventMenu.activate(false)
              CustomDialogs.launchChoiceDialog(context, String.format(res.getString(R.string.choose_trigger_outofscreen), name),
                  List(
                    String.format(res.getString(R.string.trigger_outscreen_1), name),
                    String.format(res.getString(R.string.trigger_outscreen_2), name)),
                       {i:Int =>
                         closestEvent.value.code = BEYOND_SCREEN_EVENT + i
                         eventEditor.select(closestEvent)
                         setModeSelectEffects()},
                       {() =>
                         
                       })*/
            } else if(closestEvent.isContact) {
              eventEditor.select(closestEvent, closestEventTime)
              EventMenu.activate(true)
            } else closestEvent match {
              case FingerRelated(coord, obj) => 
                eventEditor.select(closestEvent, closestEventTime, false)
                var shapesBelow = obj
                var shapeBelow = if(obj.nonEmpty) obj.head else null
                var minDistance = -1f
                shapesBelow foreach { shape =>
                  val x = coord.x
                  val y = coord.y
                  if(shape.selectableBy(x, y)) {
                    val newDist = shape.distanceSelection(x, y)
                    if(newDist < minDistance || minDistance == -1) {
                      shapeBelow = shape
                      minDistance = newDist
                    }
                  }
                }
                //closestEvent.value.shape1 = shapeBelow
                EventMenu.activate(true, shapeBelow)
              case _ =>
                EventMenu.activate(false)
                eventEditor.select(closestEvent, closestEventTime)
                setModeSelectEffects()
            }
          }
        } else if(mRuleState == STATE_MODIFYING_CATEGORY /*|| CameraButton.isSelected*/) {
          //categoryEditor.onFingerUp(touchCoords(0), touchCoords(1))
        } else if(mRuleState == STATE_SELECTING_EFFECTS) {
          performSelection(res/*, false*/)
        } else {
          performSelection(res)
        }
      }
      super.onFingerUp(pos)
  }
  
  /**
   * Sets an image for the selected shape
   * @param i The Bitmap to apply to the shape.
   */
  def setImageSelectedShape(i: Bitmap) = {
    if(shapeEditor.selectedShape != null) {
      shapeEditor.selectedShape match {
        case selectedShape: Colorable =>
          val finalbitmap = selectedShape match {
            case c: Circle =>
              getRoundedShape(i)
            case _ =>
              i
          }
          val id = Stream.from(0).find(i => !(bitmaps contains i)).get
          bitmaps(id) = new BitmapDrawable(finalbitmap)
          selectedShape.color setNext id
        case _ =>
      }
    }
  }
  
 /**
  * Making image in circular shape
  * Source: http://www.androiddevelopersolution.com/2012/09/crop-image-in-circular-shape-in-android.html
  */
 def getRoundedShape(scaleBitmapImage: Bitmap): Bitmap = {
  val targetSize = Math.min(scaleBitmapImage.getWidth, scaleBitmapImage.getHeight)
  val targetBitmap = Bitmap.createBitmap(targetSize, 
                            targetSize,Bitmap.Config.ARGB_8888);
  
  val canvas = new Canvas(targetBitmap);
  val path = new Path();
  path.addCircle((targetSize.toFloat - 1) / 2,
  (targetSize.toFloat - 1) / 2,
  (targetSize / 2),
          Path.Direction.CCW);
  
  canvas.clipPath(path);
  val sourceBitmap = scaleBitmapImage;
  canvas.drawBitmap(sourceBitmap, 
                                new Rect(0, 0, sourceBitmap.getWidth(), sourceBitmap.getHeight()), 
                                new Rect(0, 0, targetSize, targetSize), null);
  return targetBitmap;
 }
 
  
  /**
   * Updates the code view
   * @param rules The rules to display
   * @param objects The objects to highlight.
   */
  def updateCodeView(rules: Iterable[Expr], objects: Iterable[GameObject]) = {
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
  
  /**
   * Puts a new matrix for the view.
   * @param m The matrix for the new transformation.
   */
  def push(m: Matrix) = {
    m.invert(matrixI)
    
    grid = Grid(matrixI, width=mWidth, numSteps=15, stroke_width=1, color=0x88000000)
    if(game != null) game.FINGER_SIZE = matrixI.mapRadius(35)
  }

  /**
   * One Finger moves
   * @param from The first coordinate
   * @param to   The second coordinate
   */
  override def onOneFingerMove(from: Vec2, to: Vec2): Unit = state match {
    case Running => 
      val res = mapVectorToGame(to)
      game.onOneFingerMove(mapVectorToGame(from), res)
      currentFingerPos = res
    case Editing =>
      //matrix.postTranslate(to.x - from.x, to.y - from.y)
      //push(matrix)
      super.onOneFingerMove(from, to)
      mDisplacementX = to.x - touchDownOriginal.x
      mDisplacementY = to.y - touchDownOriginal.y
      val touchCoords = mapVectorToGame(to)
      val touchCoords2 = mapVectorToGame(from)
      val shiftX = touchCoords.x - touchCoords2.x
      val shiftY = touchCoords.y - touchCoords2.y
      val simpleRelativeX = touchCoords.x - touchDownOriginalGame.x
      val simpleRelativeY = touchCoords.y - touchDownOriginalGame.y
      var relativeX = simpleRelativeX
      var relativeY = simpleRelativeY
      // Snap to grid.
      if(Math.abs(relativeX) < 0.05f) { relativeX = 0 }
      if(Math.abs(relativeY) < 0.05f) { relativeY = 0 }
      ShapeMenu.onFingerMove(this, shapeEditor.selectedShape, relativeX, relativeY, shiftX, shiftY, mDisplacementX, mDisplacementY)
      if(ColorMenu.activated) {
        ColorMenu.testHovering(to.x, to.y, button_size)
        ColorMenu.onFingerMove(this, shapeEditor.selectedShape, relativeX, relativeY, shiftX, shiftY, mDisplacementX, mDisplacementY)
      }
      if(SystemMenu.activated) {
        SystemMenu.testHovering(to.x, to.y, button_size)
        SystemMenu.onFingerMove(this, shapeEditor.selectedShape, relativeX, relativeY, shiftX, shiftY, mDisplacementX, mDisplacementY)
      }
      if(EventMenu.isActivated) {
        EventMenu.testHovering(to.x, to.y, button_size)
        EventMenu.onFingerMove(this, shapeEditor.selectedShape, relativeX, relativeY, shiftX, shiftY, mDisplacementX, mDisplacementY)
      }
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
//  def mapVectorFromGame(p: Vec2V): Vec2V = {
//    val toMap = Array(p.x, p.y)
//    matrix.mapPoints(toMap)
//    Vec2V(toMap(0), toMap(1))
//  }
  

  /** pixels to meters */
  def mapVectorToGame(p: Vec2): Vec2 = {
    val toMap = Array(p.x, p.y)
    matrixI.mapPoints(toMap)
    Vec2(toMap(0), toMap(1))
  }
  
  
  /** meters to pixels */
//  def mapVectorToGame(p: Vec2V): Vec2V = {
//    val toMap = Array(p.x, p.y)
//    matrixI.mapPoints(toMap)
//    Vec2V(toMap(0), toMap(1))
//  }

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