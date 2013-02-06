package ch.epfl.lara.synthesis.kingpong;


import android.view.MotionEvent
import android.graphics.Canvas
import android.view.SurfaceHolder
import android.os.Handler
import android.view.SurfaceView
import android.content.Context
import scala.collection.mutable.ArrayBuffer
import android.graphics.Bitmap
import android.graphics.Rect
import android.graphics.RectF
import android.graphics.drawable.NinePatchDrawable
import android.graphics.Paint
import android.util.Log
import java.io.IOException
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import android.graphics.Matrix
import scala.collection.mutable.HashMap
import android.util.AttributeSet
import scala.collection.mutable.Queue
import scala.collection.mutable.LinkedList
import scala.collection.mutable.Stack
import android.graphics.drawable.BitmapDrawable
import android.graphics.PorterDuffColorFilter
import android.graphics.PorterDuff
import android.graphics.PorterDuff.Mode
import android.graphics.drawable.Drawable
import android.widget.Toast
import android.widget.SeekBar
import android.os.Vibrator


/**
 * GameEngineView is used to
 *   - Draw the game repeatedly
 *   - Handle interaction from the user
 */
trait GameEngineView extends SurfaceView with SurfaceHolder.Callback with Runnable {
  import GameShapes._
  
  /** Properties about the game it handles */
  protected var game: Game = null
  def setGame(g: Game) = {
    if(mWidth > 0 && mHeight > 0) {
      computeMatrix()
    }
    game = g
    game.setGameEngine(this)
  }
  def hasGame(): Boolean = game != null
  def getGame(): Game = game
  def outputGame() {
    game.outputItself()
    Toast.makeText(getContext(), "Game written", 1000).show()
  }
  
  /** Vibration used for haptic feedback */
  private var vibratorInstance: Vibrator = null
  def vibrate(): Unit = {
    if(vibratorInstance == null) {
      vibratorInstance = getContext().getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[Vibrator]
      if(vibratorInstance == null) return
    }
    vibratorInstance.vibrate(50)
  }
  
  def set2DAcceleration(x: Float, y: Float) = {
    if(game != null) {
      game.set2DAcceleration(x, y)
    }
  }
  
  var ghostModeActivated = false
  def isInGhostMode() = ghostModeActivated
  def activateGhostMode() = ghostModeActivated = true
  def deactivateGhostMode() = ghostModeActivated = false
  def reset()
  
  /** Game running conditions */
  var displayThread: Thread = null
  var mRun: Boolean = true

  /** Surface properties */
  var mWidth: Int = 0
  var mHeight: Int = 0

  /** Background image */
  private var backGroundImage: Bitmap = null
  def setBackground(background: Bitmap) = {
    backGroundImage = background
  }

  /** Callback invoked when the surface dimensions change. */
  def surfaceChanged(holder: SurfaceHolder, format: Int, width: Int, height: Int) {
    if (width > 0 && height > 0) {
      mWidth = getWidth()
      mHeight = getHeight()
      if(game != null) {
        game.computeRegularBounds()
      }
      computeMatrix()
      if (backGroundImage != null) {
        backGroundImage = Bitmap.createScaledBitmap( backGroundImage, 
          mWidth,        // new width
          mHeight,       // new height
          true);
      }
    }
  }
  
  /** Updates the game physics since last time */
  def updateGamePhysics()
  
  /** Draws the game at the current time */
  def drawGameOn(c: Canvas)
  
  /** Draws the menu on the canvas */
  def drawMenuOn(c: Canvas)
  
  /** Draws the game on the given canvas */
  def drawBackgroundThenGame(c: Canvas) = {
    if(game != null)  {
      c.drawBitmap(backGroundImage, 0, 0, null)
      c.save()
      c.setMatrix(mMatrix)
      drawGameOn(c)
      c.restore()
      drawMenuOn(c)
    }
  }
  
  /** The matrices used for transformation */
  var mMatrix: Matrix = new Matrix
  var mIMatrix: Matrix = new Matrix
  
  /** Compute matrices to fit the canvas to the screen */
  def computeMatrix() = {
    if(game != null) {
      mMatrix.setRectToRect (new RectF(game.minX, game.minY, game.maxX, game.maxY), new RectF(0, 0, mWidth, mHeight), Matrix.ScaleToFit.CENTER)
      mMatrix.invert(mIMatrix)
    }
  }
        
  /** Running thread which start, animates and displays the game */
  def run() {
    if(game != null) game.setGameEngine(this)
    reset()
    var c:Canvas = null;
    val holder: SurfaceHolder = getHolder()
    while(mRun) {
      try {
        c = holder.lockCanvas(null);
        holder.synchronized {
          updateGamePhysics()
          drawBackgroundThenGame(c)
        }
      } catch {
        case e: NullPointerException =>
          e.printStackTrace();
        case e: Throwable =>
          e.printStackTrace();
      } finally {
        if (c != null) {
          holder.unlockCanvasAndPost(c);
        }
      }
    }
  }
  
  def appendLog(text: String) {
    Log.d("CustomGame", text+"\n")
    val logFile = new File("sdcard/log.file");
    if (!logFile.exists()) {
       try {
          logFile.createNewFile();
       } catch {
         case e: IOException =>
          e.printStackTrace();
       }
    }
    try {
       //BufferedWriter for performance, true to set append to file flag
       val buf = new BufferedWriter(new FileWriter(logFile, true)); 
       buf.append(text);
       buf.newLine();
       buf.close();
    } catch {
      case e: IOException =>
      e.printStackTrace();
    }
  }
  
  /** Previous coordinates to use until 10 fingers at the same time.*/
  val lastX = Array[Float](0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  val lastY = Array[Float](0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  final val maxPointers = 0
  var coords = Array(0.0f, 0.0f)
  
  /** Are we in game edit mode or not */
  protected var editMode = false
  
  /** When we move finger, is a menu activated */
  var menu_activated = false
  
  /** Handles finger presses and movements  */
  override def onTouchEvent(me: MotionEvent): Boolean = {
      val action = me.getAction()
      (action  & MotionEvent.ACTION_MASK) match {
        // A finger gets down.
        case i if i == MotionEvent.ACTION_DOWN || i == MotionEvent.ACTION_POINTER_DOWN =>
          val pointerIndex = (action & MotionEvent.ACTION_POINTER_INDEX_MASK) >> MotionEvent.ACTION_POINTER_INDEX_SHIFT
          val pointerId = Math.min(me.getPointerId(pointerIndex), lastX.size - 1)
          //Log.d("GameEngineView", "Finger down event at " + me.getX(pointerIndex) + ", " + me.getY(pointerIndex))
          if(game != null) {
            coords(0) = me.getX(pointerIndex)
            coords(1) = me.getY(pointerIndex)
            if(editMode) {
              menu_activated = onFingerDown(coords(0), coords(1))
              if(menu_activated) vibrate()
              lastX(pointerId) = coords(0)
              lastY(pointerId) = coords(1)
            } else {
              menu_activated = false
              mIMatrix.mapPoints(coords)
              val x = coords(0)
              val y = coords(1)
              if(game.onFingerDownEvent(x, y)) {
                vibrate()
              }
              lastX(pointerId) = x
              lastY(pointerId) = y
            }
            //appendLog("Pointer Id=" + pointerId + " went down on " + me.getX() + "," + me.getY())
          }
        // Any pointer might move.
        case MotionEvent.ACTION_MOVE =>
          if(game != null) {
            if(editMode && !menu_activated) {
              // We move the area
              if(me.getPointerCount() == 1) {
              val pointerId = Math.min(me.getPointerId(0), lastX.size - 1)
              var xFrom = lastX(pointerId)
              var yFrom = lastY(pointerId)
              val xTo = me.getX(0)
              val yTo = me.getY(0)
              mMatrix.postTranslate(xTo - xFrom, yTo - yFrom)
              mMatrix.invert(mIMatrix)
              lastX(pointerId) = xTo
              lastY(pointerId) = yTo
              // We stretch the area
              } else if(me.getPointerCount() == 2) {
              val pointerId = Math.min(me.getPointerId(0), lastX.size - 1)
              val pointerId2 = Math.min(me.getPointerId(1), lastX.size - 1)
              var x1From = lastX(pointerId)
              var y1From = lastY(pointerId)
              val x1To = me.getX(0)
              val y1To = me.getY(0)
              var x2From = lastX(pointerId2)
              var y2From = lastY(pointerId2)
              val x2To = me.getX(1)
              val y2To = me.getY(1)
              val dFrom = Math.sqrt((x1From - x2From) * (x1From - x2From) + (y1From - y2From) * (y1From - y2From))
              val dTo = Math.sqrt((x1To- x2To) * (x1To - x2To) + (y1To - y2To) * (y1To - y2To))
              val scale = if(dFrom != 0) (dTo / dFrom).toFloat else 1.0f
              val d1 = Math.sqrt((x1To - x1From) * (x1To - x1From) + (y1To - y1From) * (y1To - y1From)).toFloat
              val d2 = Math.sqrt((x2To - x2From) * (x2To - x2From) + (y2To - y2From) * (y2To - y2From)).toFloat
              val p = if(d1+d2 != 0) d2/(d1+d2) else 0
              mMatrix.postTranslate((x1To + x2To)/2 - (x1From + x2From)/2, (y1To + y2To)/2 - (y1From + y2From)/2)
              mMatrix.postScale(scale, scale, x1From * p + x2From * (1 - p), y1From * p + y2From * (1 - p))
              mMatrix.invert(mIMatrix)
              lastX(pointerId) = x1To
              lastY(pointerId) = y1To
              lastX(pointerId2) = x2To
              lastY(pointerId2) = y2To
              }
              
            } else {
              for(pointerIndex <- 0 until me.getPointerCount()) {
                //val historySize = me.getHistorySize()
                val pointerId = Math.min(me.getPointerId(pointerIndex), lastX.size - 1)
                var xFrom = lastX(pointerId)
                var yFrom = lastY(pointerId)
                var xTo = 0.0f
                var yTo = 0.0f
                if(editMode) {
                  xTo = me.getX(pointerIndex)
                  yTo = me.getY(pointerIndex)
                  onFingerMove(xFrom, yFrom, xTo, yTo)
                } else {
                  coords(0) = me.getX(pointerIndex)
                  coords(1) = me.getY(pointerIndex)
                  mIMatrix.mapPoints(coords)
                  xTo = coords(0)
                  yTo = coords(1)
                  game.onFingerMoveEvent(xFrom, yFrom, xTo, yTo)
                }
                //appendLog("Pointer Id=" + pointerId + " moved from " + xFrom + "," + yFrom + " to " + xTo + "," + yTo )
                lastX(pointerId) = xTo
                lastY(pointerId) = yTo
              }
            }
          }
        case i if i == MotionEvent.ACTION_UP || i == MotionEvent.ACTION_POINTER_UP =>
          if(game != null) {
            val pointerIndex = (action & MotionEvent.ACTION_POINTER_INDEX_MASK) >>
              MotionEvent.ACTION_POINTER_INDEX_SHIFT
            val pointerId = Math.min(me.getPointerId(pointerIndex), lastX.size - 1)
            coords(0) = me.getX(pointerIndex)
            coords(1) = me.getY(pointerIndex)
            if(editMode) {
              onFingerUp(coords(0), coords(1))
            } else {
              mIMatrix.mapPoints(coords)
              val x = coords(0)
              val y = coords(1)
              game.onFingerUpEvent(x, y)
            }
            //appendLog("Pointer Id=" + pointerId + " went up on " + me.getX() + "," + me.getY())
          }
        case _ =>
      }
      true
  }
  
  /**
   * Global pause and resume methods
   */
  def onPause() {
    paused = true
  }
  def onResume() {
    paused = false
  }
  protected var paused: Boolean = false
  def isOnPause: Boolean = paused
  def isInEditMode(): Boolean
  def enterEditMode()
  def exitEditMode()
  
  def surfaceCreated(holder: SurfaceHolder) = {
    if (displayThread == null || displayThread.getState() == Thread.State.TERMINATED)
    {
        displayThread = new Thread(this)
    }
    displayThread.start();
  }

  def surfaceDestroyed(holder: SurfaceHolder) = {
    mRun = false
  }
  
  def setTimeBar(t: SeekBar)
  
  var holder = getHolder()
  holder.addCallback(this)
  
  def onFingerDown(x: Float, y: Float): Boolean
  def onFingerUp(x:Float, y: Float)
  def onFingerMove(xFrom: Float, yFrom: Float, xTo: Float, yTo: Float)
}
