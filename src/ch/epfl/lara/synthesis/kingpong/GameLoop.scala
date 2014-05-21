package ch.epfl.lara.synthesis.kingpong

import scala.util.Try

import android.graphics.Canvas
import android.view.SurfaceHolder
import android.util.Log


object GameLoop {
  val MAX_FPS = 30
  val MAX_FRAMES_SKIPPED = 5
  val FRAME_PERIOD_MS: Float = 1000f / MAX_FPS // In milliseconds
  val FRAME_PERIOD_S: Float = 1f / MAX_FPS // In seconds
}

class GameLoop(holder: SurfaceHolder, view: GameView) extends Thread {
  import GameLoop._
  
  /** Is the loop running. */
  private var running: Boolean = true

  /** Ask this game loop to stop. */
  def requestStop(): Unit = {
    running = false
  }

  override def run(): Unit = {
    var canvas: Canvas = null
    Log.d("kingpong", "Starting game loop")

    while(running) {
      canvas = null
      try {
        canvas = holder.lockCanvas()
        holder.synchronized {
          val beginTime = System.currentTimeMillis()
          var framesSkipped = 0

          view.update()
          view.render(canvas)

          val timeDiff = System.currentTimeMillis() - beginTime
          var sleepTime = FRAME_PERIOD_MS - timeDiff

          if (sleepTime > 0) Try {
            Thread.sleep(sleepTime.toLong)
          }

          while (sleepTime < 0 && framesSkipped < MAX_FRAMES_SKIPPED) {
            //Log.d("kingpong", "GameLoop missed a frame.")
            view.update()
            sleepTime += FRAME_PERIOD_MS
            framesSkipped += 1
          }
        }
      } catch {
        case e: Exception =>
          e.printStackTrace()
      } finally {
        if (canvas != null) holder.unlockCanvasAndPost(canvas)
      }
    }
  }

}