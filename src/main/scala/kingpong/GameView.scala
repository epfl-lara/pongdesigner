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

object GameView {

  // 1 meter is equivalent to 100 pixels (with default zoom)
  val BOX2D_RATIO = 100

  // Pi in float
  val PI = 3.141592f
}

class GameView(context: Context, attrs: AttributeSet) extends SurfaceView(context, attrs) 
                                                      with SurfaceHolder.Callback {
  import GameView._


  /** The game model currently rendered. */
  private var game: Game = new EmptyGame()

  /** The main game loop that calls `update()` and `render()`. */
  private val gameLoop = new GameLoop(getHolder(), this)

  /** The transformation applied to the canvas. 
   *  Transforms from Box2D units (meters) to pixels.
   */
  private val matrix = new Matrix()

  /** The inverse transformation matrix from pixels to meters. */
  private val matrixI = new Matrix()

  // Register to intercept events
  getHolder().addCallback(this)

  def reset(newGame: Game): Unit = {
    game = newGame
  }

  def update(): Unit = {
    game.update()
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
  }

  def surfaceChanged(holder: SurfaceHolder, format: Int, width: Int, height: Int): Unit = {
    Log.d("kingpong", "surfaceChanged")
    computeTransformationMatrices()

  }

  def surfaceCreated(holder: SurfaceHolder): Unit = {
    if (!gameLoop.running) {
      gameLoop.running = true
      gameLoop.start()
    }
  }

  def surfaceDestroyed(holder: SurfaceHolder): Unit = {
    gameLoop.running = false
    var retry = true
    while (retry) Try { 
      gameLoop.join()
      retry = false
    }
  }

  def mapVector(p: Vec2): Vec2 = {
    val toMap = Array(p.x, p.y)
    matrix.mapVectors(toMap)
    Vec2(toMap(0), toMap(1))
  }

  /** meters to pixels */
  def mapRadius(r: Float): Float = matrix.mapRadius(r)

  /** pixels to meters */
  def mapRadiusI(r: Float): Float = matrixI.mapRadius(r)

  def radToDegree(r: Float): Float = (r * 180 / PI) % 360

  private def computeTransformationMatrices() = {
    matrix.reset() // identity matrix
    //matrix.postScale(1, -1); // upside-down
    matrix.postScale(BOX2D_RATIO, BOX2D_RATIO)
    matrix.invert(matrixI)
  }

}