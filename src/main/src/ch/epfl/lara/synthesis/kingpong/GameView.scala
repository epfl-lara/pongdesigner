package ch.epfl.lara.synthesis.kingpong

import scala.util.Try
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

trait GameViewInterface {
}

/**
 * Handling the time bar
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
  def setTime(t: Int) = {
    progressBar.setProgress(t)
    progressBar.setSecondaryProgress(t)
  }
  
  def onStartTrackingTouch(seekBar: SeekBar):Unit = {
  }
  def onStopTrackingTouch(seekBar: SeekBar):Unit = {
  }
}


trait ActionBarHandler extends common.ContextUtils {
  private var actionBar: ExpandableListView = _
  private var actionBarAdapter: ExpandableListAdapter = _
  def menuCallBacks: String => Unit
  
  def setActionBar(actionBar: ExpandableListView): Unit = {
    this.actionBar = actionBar
    if(actionBar != null) {
      
      val menuLabels: IndexedSeq[String] = getStringArray(R.array.menu_arrays_hint)
      val menuDrawables = getArray(R.array.menu_arrays_drawable) map getDrawable
      val submenusDrawables = getArray(R.array.menu_arrays) map getDrawableArray
      val submenusLabels = getArray(R.array.menu_arrays_strings) map getStringArray map {u: Array[String] => u:IndexedSeq[String]}
      
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
  with GameViewInterface
  with ProgressBarHandler
  with ActionBarHandler
  with common.ContextUtils {
  import GameView._
  import common.Implicits._

  private var activity: Activity = null
  private var codeview: EditTextCursorWatcher = null
  
  
  def menuCallBacks: String => Unit = { s =>
    game.rectangle(DefaultCategory("test"))(name="test", x=0, y=0, width=10, height=5)
  }

  /** The game model currently rendered. */
  private var game: Game = null
  def setGame(g: Game) = game = g
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
  var codeMapping = Map[Int, Category]()   // TODO add the mapping and highlight the corresponding objects.
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
    layoutResize()
  }

  /** Change the current state to Running. */
  def toRunning(): Unit = if (state == Editing) {
    Log.d("kingpong", "toRunning()")
    // Remove objects that have been created after the date.
    game.gc()
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

  def update(): Unit = {
    state match {
      case Running =>
        
        game.update()
        setTime(game.time.toInt)
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
    canvas.setMatrix(matrix)
    canvas.drawRGB(0xFF, 0xFF, 0xFF)

    paint.setStrokeWidth(mapRadiusI(1))
    paintSelected.setStrokeWidth(mapRadiusI(3))
    if(game == null) return;
    game.objects foreach { o => 
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
        val value = b.value.get.toString
        canvas.drawText(value, b.x.get, b.y.get, paint)
        if(obj_to_highlight contains b) canvas.drawText(value, b.x.get, b.y.get, paint)
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
      case _ =>
    }}}
    
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
        val pos = mapVectorI(Vec2(100, 100))
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

  def onAccelerometerChanged(vector: Vec2): Unit = {
    // do NOT touch the given vector! 
    // It is mutable for performance purposes.
    state match {
      case Running => 
        game.onAccelerometerChanged(vector.clone)
      case Editing =>
        //TODO
    } 
  }
  
  var currentFingerPos: Vec2 = null
  var fingerIsDown = false

  def onFingerDown(pos: Vec2): Unit = state match {
    case Running => 
      val res = mapVectorI(pos)
      game.onFingerDown(res)
      currentFingerPos = res
      fingerIsDown = true
    case Editing =>
      //TODO
  }

  def onFingerUp(pos: Vec2): Unit = state match {
    case Running => 
      val res = mapVectorI(pos)
      game.onFingerUp(res)
      fingerIsDown = false
      currentFingerPos = res
    case Editing =>
      // Select an object below if any and display the corresponding code
      val res = mapVectorI(pos)
      val objectsTouched = game.objectFingerAt(res)
      val rulesConcerned = game.getRulesbyObject(objectsTouched)
      updateCodeView(rulesConcerned, objectsTouched)
  }
  
  def updateCodeView(rules: Iterable[Stat], objects: Iterable[GameObject]) = {
    val header = PrettyPrinterExtended.printGameObjectDef(objects)
    val all = PrettyPrinterExtended.print(rules, header + "\n")
    val r: CharSequence = all.c
    val mapping = all.map
    val mObjects = mapping.mObjects
    codeMapping = mapping.mPosCategories
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
    if(game != null) game.FINGER_SIZE = matrixI.mapRadius(35)
  }

  def onOneFingerMove(from: Vec2, to: Vec2): Unit = state match {
    case Running => 
      val res = mapVectorI(to)
      game.onOneFingerMove(mapVectorI(from), res)
      currentFingerPos = res
    case Editing =>
      matrix.postTranslate(to.x - from.x, to.y - from.y)
      push(matrix)
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
  def mapVector(p: Vec2): Vec2 = {
    val toMap = Array(p.x, p.y)
    matrix.mapPoints(toMap)
    Vec2(toMap(0), toMap(1))
  }

  /** pixels to meters */
  // TODO: perfom the change on the same vector, not a new one.
  def mapVectorI(p: Vec2): Vec2 = {
    val toMap = Array(p.x, p.y)
    matrixI.mapPoints(toMap)
    Vec2(toMap(0), toMap(1))
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
          last(pointerIndex).set(point)

        // A finger moves
        case MotionEvent.ACTION_MOVE =>
          if (me.getPointerCount() == 1) {
            val pointerIndex = Math.min(me.getPointerId(0), FINGERS - 1)
            val from = last(pointerIndex)
            val to = Vec2(me.getX(0), me.getY(0))
            //Log.d("GameView", s"Moved from ${from.x}, ${from.y} to ${to.x}, ${to.y}")
            onOneFingerMove(from, to)
            last(pointerIndex).set(to)
            
          } else if (me.getPointerCount() == 2) {
            val pointerIndex1 = Math.min(me.getPointerId(0), FINGERS - 1)
            val pointerIndex2 = Math.min(me.getPointerId(1), FINGERS - 1)
            val from1 = last(pointerIndex1)
            val from2 = last(pointerIndex2)
            val to1 = Vec2(me.getX(0), me.getY(0))
            val to2 = Vec2(me.getX(1), me.getY(1))
            onTwoFingersMove(from1, to1, from2, to2)
            last(pointerIndex1).set(to1)
            last(pointerIndex2).set(to2)
          }

        case MotionEvent.ACTION_UP | MotionEvent.ACTION_POINTER_UP =>
          val pointerIndex = (action & MotionEvent.ACTION_POINTER_INDEX_MASK) >> MotionEvent.ACTION_POINTER_INDEX_SHIFT
          val point = Vec2(me.getX(pointerIndex), me.getY(pointerIndex))
          if(last(pointerIndex).x != point.x || last(pointerIndex).y != point.y)
            onOneFingerMove(last(pointerIndex), point)
          onFingerUp(point)
          last(pointerIndex).set(point)

        case _ => //Do nothing
      }
    }
  }

}