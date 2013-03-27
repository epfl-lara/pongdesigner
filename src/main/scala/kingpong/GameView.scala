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

class GameView(context: Context) extends SurfaceView(context) 
                                 with SurfaceHolder.Callback {

  /** The game model currently rendered. */
  var game: Game = new EmptyGame()

  /** The main game loop that calls `update()` and `render()`. */
  val gameLoop = new GameLoop(getHolder(), this)

  // Register to intercept eventss
  getHolder().addCallback(this)

  def reset(newGame: Game): Unit = {
    game = newGame

    //TODO Implement reset()
    ???
  }

  def update(): Unit = {
    //TODO Implement update()
    ???
  }

  def render(c: Canvas): Unit = {
    //TODO Implement render()
    ???
  }


  def surfaceChanged(holder: SurfaceHolder, format: Int, width: Int, height: Int): Unit = {
    ???
  }

  def surfaceCreated(holder: SurfaceHolder): Unit = {
    gameLoop.running = true
    gameLoop.start()
  }

  def surfaceDestroyed(holder: SurfaceHolder): Unit = {
    gameLoop.running = false
    var retry = true
    while (retry) Try { 
      gameLoop.join()
      retry = false
    }
  }

}