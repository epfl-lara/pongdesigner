package ch.epfl.lara.synthesis.kingpong

import scala.util.Try

import android.graphics.Canvas
import android.view.SurfaceHolder
import android.util.Log


object GameLoop {
  val MAX_FPS = 50
  val MAX_FRAMES_SKIPPED = 5
  val FRAME_PERIOD = 1000 / MAX_FPS
}

class GameLoop(holder: SurfaceHolder, view: GameView) extends Thread {
  import GameLoop._
  
  /** Is the loop running. */
  var running: Boolean = false

  override def run(): Unit = {
    var canvas: Canvas = null
    Log.d("GameLoop", "Sarting game loop")

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
          var sleepTime = (FRAME_PERIOD - timeDiff).asInstanceOf[Int]

          if (sleepTime > 0) Try {
            Thread.sleep(sleepTime)
          }

          while (sleepTime < 0 && framesSkipped < MAX_FRAMES_SKIPPED) {
            view.update()
            sleepTime += FRAME_PERIOD
            framesSkipped += 1
          }
        }
      } finally {
        if (canvas != null) holder.unlockCanvasAndPost(canvas)
      }
    }
  }

}