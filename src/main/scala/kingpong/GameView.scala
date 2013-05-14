package ch.epfl.lara.synthesis.kingpong

import scala.util.Try

import android.view.SurfaceView
import android.view.MotionEvent
import android.view.SurfaceHolder

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

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.objects._

import org.jbox2d.common.MathUtils

object GameView {

  sealed trait GameState
  case object Running extends GameState
  case object Editing extends GameState

  // 1 meter is equivalent to 100 pixels (with default zoom)
  val BOX2D_RATIO = 100

  val FINGERS = 10
}

class GameView(context: Context, attrs: AttributeSet) extends SurfaceView(context, attrs) 
                                                      with SurfaceHolder.Callback {
  import GameView._

  /** The game model currently rendered. */
  private var game: Game = new EmptyGame()

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


  /** Called by the activity when the game has to sleep deeply. 
   *  The state is changed to `Editing` and the game loop is stoped.
   */
  def onPause(): Unit = {
    Log.d("kingpong", "onPause()")
    state = Editing
    stopLoop()
  }

  /** Called by the activity after a deep sleep. 
   *  The state is keeped to `Editing` and the game loop is started.
   */
  def onResume(): Unit = {
    Log.d("kingpong", "onResume()")
    state = Editing
    if (isSurfaceCreated)
      startLoop()
  }

  /** Change the current state to Editing. */
  def toEditing(): Unit = if (state == Running) {
    Log.d("kingpong", "toEditing()")
    state = Editing
  }

  /** Change the current state to Running. */
  def toRunning(): Unit = if (state == Editing) {
    Log.d("kingpong", "toRunning()")
    state = Running
  }

  /** Reset the game to its initial state. */
  def backToBeginning(): Unit = {
    //TODO
  }

  def reset(newGame: Game): Unit = {
    game = newGame
  }

  def update(): Unit = {

    state match {
      case Running =>
        game.update()
      case Editing =>
        //TODO
    }
  }


  private val rectF = new RectF()
  private val paint = new Paint()
  def render(canvas: Canvas): Unit = {
    canvas.setMatrix(matrix)
    canvas.drawRGB(0xFF, 0xFF, 0xFF)

    game.objects foreach { o => o match {
      case r: Rectangle =>
        paint.setColor(0xFF000000) // TODO r.color
        if(!r.visible.get)
          paint.setAlpha(0x80)

        canvas.save()
        canvas.rotate(radToDegree(r.angle.get), r.x.get, r.y.get)
        canvas.drawRect(r.x.get - r.width.get/2, r.y.get - r.height.get/2, r.x.get + r.width.get/2, r.y.get + r.height.get/2, paint)
        canvas.restore()

      case c: Circle => 
        paint.setColor(0xFF000000) // TODO c.color
        if(!c.visible.get)
          paint.setAlpha(0x80)
        canvas.drawCircle(c.x.get, c.y.get, c.radius.get, paint)
    }}

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
  }

  def surfaceChanged(holder: SurfaceHolder, format: Int, width: Int, height: Int): Unit = {
    Log.d("kingpong", "surfaceChanged()")
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


  def onFingerDown(pos: Vec2): Unit = state match {
    case Running => 
      game.onFingerDown(mapVectorI(pos))
    case Editing =>
      //TODO
  }

  def onFingerUp(pos: Vec2): Unit = state match {
    case Running => 
      game.onFingerUp(mapVectorI(pos))
    case Editing =>
      //TODO
  }

  def onOneFingerMove(from: Vec2, to: Vec2): Unit = {
    matrix.postTranslate(to.x - from.x, to.y - from.y)
    matrix.invert(matrixI)
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
    matrix.invert(matrixI)
  }

  /** meters to pixels */
  def mapVector(p: Vec2): Vec2 = {
    val toMap = Array(p.x, p.y)
    matrix.mapPoints(toMap)
    Vec2(toMap(0), toMap(1))
  }

  /** pixels to meters */
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
    matrix.reset() // identity matrix
    //matrix.postScale(1, -1); // upside-down
    matrix.postScale(BOX2D_RATIO, BOX2D_RATIO)
    matrix.invert(matrixI)
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
  private object EventHolder {
    import EventHolder._

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
            val pointer = Math.min(me.getPointerId(0), FINGERS - 1)
            val from = last(pointer)
            val to = Vec2(me.getX(0), me.getY(0))
            onOneFingerMove(from, to)
            last(pointer).set(to)
            
          } else if (me.getPointerCount() == 2) {
            val pointer1 = Math.min(me.getPointerId(0), FINGERS - 1)
            val pointer2 = Math.min(me.getPointerId(1), FINGERS - 1)
            val from1 = last(pointer1)
            val from2 = last(pointer2)
            val to1 = Vec2(me.getX(0), me.getY(0))
            val to2 = Vec2(me.getX(1), me.getY(1))
            onTwoFingersMove(from1, to1, from2, to2)
            last(pointer1).set(to1)
            last(pointer2).set(to2)
          }

        case MotionEvent.ACTION_UP | MotionEvent.ACTION_POINTER_UP =>
          val pointerIndex = (action & MotionEvent.ACTION_POINTER_INDEX_MASK) >> MotionEvent.ACTION_POINTER_INDEX_SHIFT
          val point = Vec2(me.getX(pointerIndex), me.getY(pointerIndex))
          onFingerUp(point)
          last(pointerIndex).set(point)

        case _ => //Do nothing
      }
    }
  }

}