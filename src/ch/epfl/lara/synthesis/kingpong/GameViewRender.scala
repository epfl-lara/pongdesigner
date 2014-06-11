package ch.epfl.lara.synthesis.kingpong

import java.io.File
import scala.collection.mutable.{HashMap => MMap}
import org.jbox2d.collision.WorldManifold
import org.jbox2d.common.MathUtils
import android.content.Context
import android.graphics.Canvas
import android.graphics.DashPathEffect
import android.graphics.LinearGradient
import android.graphics.Matrix
import android.graphics.Paint
import android.graphics.Path
import android.graphics.PorterDuff
import android.graphics.Rect
import android.graphics.RectF
import android.graphics.Shader
import android.graphics.drawable.Drawable
import android.media.MediaPlayer
import android.media.MediaRecorder
import android.media.AudioManager
import android.net.Uri
import android.os.Environment
import android.util._
import android.view.WindowManager
import ch.epfl.lara.synthesis.kingpong.common._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.menus._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.rules.Events._

class GameViewRender(val context: Context) extends ContextUtils {
  import GameView._
  
  private val screenDensity = context.getResources.getDisplayMetrics.density
  
  private val paint = new Paint()
  paint.setAntiAlias(true)
  private val paintPrev = new Paint(paint)
  private val paintSelected = new Paint(paint)
  paintSelected.setStyle(Paint.Style.STROKE)
  paintSelected.setColor(color(R.color.selection))
  private val render_out_vec = Vec2(0f, 0f)
  private val render_out_vec2 = Vec2(0f, 0f)
  private val render_out_vec3 = Vec2(0f, 0f)
  private val render_in_array = Array.ofDim[Float](2)
  val tmpMatrix = new Matrix
  val tmpDrawingObjectRect1 = new RectF(0, 0, 1, 1)
  val tmpDrawingObjectRect2 = new RectF(0, 0, 1, 1)
  var circlePaint = new Paint()
  circlePaint.setColor(0xFF000000)
  circlePaint.setStyle(Paint.Style.STROKE)
  circlePaint.setStrokeWidth(4)
  circlePaint.setAntiAlias(true)
  var touchDownPaint = new Paint()
  touchDownPaint.setColor(0xAAFF0000)
  touchDownPaint.setStyle(Paint.Style.FILL_AND_STROKE)
  touchDownPaint.setStrokeWidth(3)
  touchDownPaint.setAntiAlias(true)
  var touchUpPaint = new Paint()
  touchUpPaint.set(touchDownPaint)
  touchUpPaint.setColor(0xAA00FF00)
  var touchMovePaint = new Paint()
  touchMovePaint.set(touchDownPaint)
  touchMovePaint.setColor(0xAAFFFF00)

  private val touchSelectedPaint = new Paint(paintSelected)
  touchSelectedPaint.setStyle(Paint.Style.STROKE)
  touchSelectedPaint.setStrokeWidth(2)
  
  val distancePaint = new Paint()
  distancePaint.set(touchMovePaint)
  distancePaint.setColor(0xAAFF8800)
  distancePaint.setPathEffect(new DashPathEffect(Array[Float](0.5f,0.5f), 0))
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
  
  val metrics = new DisplayMetrics()
  context.getSystemService(Context.WINDOW_SERVICE).asInstanceOf[WindowManager].getDefaultDisplay().getMetrics(metrics)
  lazy val button_size:Float = if(5 * 80 * metrics.density >= Math.min(metrics.widthPixels, metrics.heightPixels)) 40 * metrics.density else 80 * metrics.density
  
  private[kingpong] var grid: Grid = new Grid(step=1, offset=0, stroke_width=1, color=0x88000000)

  /** meters to pixels */
  @inline private def mapRadius(matrix: Matrix, r: Float): Float = matrix.mapRadius(r)

  /** pixels to meters */
  @inline private def mapRadiusI(matrixI: Matrix, r: Float): Float = matrixI.mapRadius(r)
  
  @inline private def radToDegree(r: Float): Float = r * MathUtils.RAD2DEG
  
   /** meters to pixels */
  /*def mapVectorFromGame(matrix: Matrix, p: Vec2): Vec2 = {
    val toMap = Array(p.x, p.y)
    matrix.mapPoints(toMap)
    Vec2(toMap(0), toMap(1))
  }*/
  
  var sounds = Map[SoundRecorded, MediaPlayer]()
  var soundsTTSplayed = Set[SoundTTS]()
  var soundsTTSplaying = Map[SoundTTS, MediaPlayer]()
  var recorders = Map[SoundRecorder, (Int, MediaRecorder, String)]()
  
  def mapVectorFromGame(matrix: Matrix, in: Array[Float], out: Vec2): Vec2 = {
    matrix.mapPoints(in)
    out.x = in(0)
    out.y = in(1)
    out
  }
  def mapVectorFromGame(matrix: Matrix, in: Vec2, inarray: Array[Float], out: Vec2): Vec2 = {
    inarray(0) = in.x
    inarray(1) = in.y
    matrix.mapPoints(inarray)
    out.x = inarray(0)
    out.y = inarray(1)
    out
  }
  
  /** pixels to meters */
  def mapVectorToGame(matrixI: Matrix, p: Vec2): Vec2 = {
    val toMap = Array(p.x, p.y)
    matrixI.mapPoints(toMap)
    Vec2(toMap(0), toMap(1))
  }
  
  private val tmpPath = new Path()
  var lastx = 0f
  var lasty = 0f
  var lastwidth = 0f
  var lastcolor = 0x0
  val textPreviewMap = MMap[String, String]()
  
  def render(canvas: Canvas, gameView: GameView, matrix: Matrix, matrixI: Matrix, game: Game, obj_to_highlight: Set[GameObject], width_highlight: Float, bitmaps: MMap[Int, Drawable], state: GameState, isSelectingEvents: Boolean, isFixingRules: Boolean, eventEditor: EventEditor, shapeEditor: ShapeEditor): Unit = {
    canvas.drawRGB(0xFF, 0xFF, 0xFF)
    if(state == Editing) grid.drawOn(matrix, matrixI, canvas)
    canvas.save()
    canvas.setMatrix(matrix)
    Options.Event.selectableAreaRadius = mapRadiusI(matrixI, 30)
    
    paint.setStyle(Paint.Style.FILL)
    paint.setStrokeWidth(mapRadiusI(matrixI, 3))
    // alias to `paint.setLinearText(true)`, since it is deprecated.
    paint.setFlags(paint.getFlags() | Paint.LINEAR_TEXT_FLAG | Paint.SUBPIXEL_TEXT_FLAG)
    paintSelected.setStrokeWidth(mapRadiusI(matrixI, width_highlight))
    paintSelected.setFlags(paintSelected.getFlags() | Paint.LINEAR_TEXT_FLAG | Paint.SUBPIXEL_TEXT_FLAG)
    paintPrev.set(paint)
    //paintPrev.setStyle(Paint.Style.FILL)
    //paintPrev.setStrokeWidth(mapRadiusI(matrixI, 3))
    distancePaint.setStrokeWidth(mapRadiusI(matrixI, 2))
    
    def drawObject(o: GameObject): Unit = {
      paint.setStyle(Paint.Style.FILL)
      o match {
        case o: Positionable =>
          val colorPrev = o.color.get
          val colorNext = o.color.next
          val visible_prev = o.visible.get
          val visible_next = o.visible.next
          if(colorPrev != colorNext || visible_prev != visible_next) {
            val cPrev = if(visible_prev) colorPrev else (colorPrev & 0xFFFFFF) + (((colorPrev >>> 24)*0.5).toInt << 24)
            val cNext = if(visible_next) colorNext else (colorNext & 0xFFFFFF) + (((colorNext >>> 24)*0.5).toInt << 24)
            if(o.x.get != o.x.next || o.y.get != o.y.next) {
              paintPrev.setColor(cPrev)
              paint.setColor(cNext)
            } else {
              paint.setShader(new LinearGradient(o.left.get, o.y.get, o.right.get, o.y.get, Array(cPrev, cPrev,cNext,cNext), Array(0, 0.4375f,0.5625f,1), Shader.TileMode.MIRROR));
            }
          } else {
            paint.setShader(null)
            paint.setColor(colorPrev)
            if (!visible_prev) {
              if(state == Editing) {
                paint.setAlpha(paint.getAlpha()/2)
              } else {
                paint.setAlpha(0x00)
              }
            }
          }
        
        case _ => //MIKAEL what to do here ?
      }
      

      o match {
        case o: Gravity =>
          if(o.visible.get || o.visible.next || state == Editing) {
            drawVelocity(o, o.x.next, o. y.next, o.vectorNext, velocityPaint)
		        if(o.vectorNext.x != o.vector.x || o.vectorNext.y != o.vector.y) {
		          drawVelocity(o, o.x.get, o.y.get, o.vector, velocityPaintShaded)
		        }
          }
        case soundTTS: SoundTTS => 
          if(state == Running) {
            maybePlay(gameView, soundTTS)
          }
          if(state == Editing || soundTTS.visible.next) {
	          val textNext = if(soundTTS.x.get != soundTTS.x.next || soundTTS.y.get != soundTTS.y.next || soundTTS.height.get != soundTTS.height.next) {
	            paintPrev.setTextSize(soundTTS.height.get)
	            canvas.save()
	            canvas.rotate(radToDegree(soundTTS.angle.get), soundTTS.x.get, soundTTS.y.get)
	            val text = soundTTS.value.get
	            val value = if(text.length > 10) textPreviewMap.getOrElseUpdate(text, text.substring(0, 10)+"\u2026") else text
	            canvas.drawText(value, soundTTS.x.get, soundTTS.y.get, paintPrev)
	            canvas.restore()
	            canvas.drawLine(soundTTS.x.get, soundTTS.y.get, soundTTS.x.next, soundTTS.y.next, distancePaint)
	            soundTTS.value.next
	          } else if(soundTTS.value.get != soundTTS.value.next) { // Same place, different value.
	            soundTTS.value.get + "=>" + soundTTS.value.next
	          } else {
	            soundTTS.value.next
	          }
	          val valueDisplayNext = if(textNext.length > 10) textPreviewMap.getOrElseUpdate(textNext, textNext.substring(0, 10)+"\u2026") else textNext
	          paint.setTextSize(soundTTS.height.next)
	          canvas.save()
	          canvas.rotate(radToDegree(soundTTS.angle.next), soundTTS.x.next, soundTTS.y.next)
	          canvas.drawText(valueDisplayNext, soundTTS.x.next, soundTTS.y.next, paint)
	          if(obj_to_highlight contains soundTTS) {
	            paintSelected.setTextSize(soundTTS.height.next)
	            canvas.drawText(valueDisplayNext, soundTTS.x.next, soundTTS.y.next, paintSelected)
	          }
	          canvas.restore()
          }
        case soundRecorder : SoundRecorder =>
          if(state == Editing) {
            drawBitmapInGame(canvas, matrix, soundRecorder, bitmaps(R.drawable.bm_microphone), 0xFF)
          }
          if(state == Running) {
            maybeRecord(game, soundRecorder)
          }
        case sound : SoundRecorded =>
          if(state == Editing || gameView.editWhileRunning) {
            if(sound.startTime <= game.time && game.time <= sound.endTime) {
              drawBitmapInGame(canvas, matrix, sound, bitmaps(R.drawable.bm_music), 0xFF)
            } else {
              drawBitmapInGame(canvas, matrix, sound, bitmaps(R.drawable.bm_music), 0x8F)
            }
          }
          if(state == Editing) {
            sounds.get(sound) match {
              case None =>
              case Some(mp) =>
                mp.release()
			          sounds -= sound
            }
            //drawBitmapInGame(canvas, matrix, d, bitmaps(R.drawable.bm_microphone))
          }
          if(state == Running) {
            maybePlay(game, sound)
          }
        case d : DrawingObject =>
          canvas.save()
          canvas.rotate(radToDegree(d.angle.next), d.x.next, d.y.next)
          paint.setStyle(Paint.Style.STROKE)

          //canvas.setMatrix(tmpMatrix)
          //if(state == Editing) {
            canvas.drawRect(d.left.next, d.top.next, d.right.next, d.bottom.next, paint)
          //}
            if (obj_to_highlight contains d) 
            canvas.drawRect(d.left.next, d.top.next, d.right.next, d.bottom.next, paintSelected)
            
          tmpDrawingObjectRect2.set(d.left.next, d.top.next, d.right.next, d.bottom.next)
          tmpMatrix.setRectToRect(tmpDrawingObjectRect1, tmpDrawingObjectRect2, Matrix.ScaleToFit.FILL)
          canvas.concat(tmpMatrix)
          
          val oldStrokeJoin = paint.getStrokeJoin()
          paint.setStrokeJoin(Paint.Join.ROUND)
          d.getDrawingElements foreach { case d@DrawingElement(time, fromx, fromy, tox, toy, width, color) =>
            if(time <= game.time) {
              if(fromx == lastx && fromy == lasty && width == lastwidth && color == lastcolor) {
                var prevTan = d.prevTan
                val predNextTan = if(d.pred != null) {
                  val pred = d.pred
                  val predNextTan = pred.nextTan
                  if(predNextTan != null) {
                    if(prevTan == null) {
                      prevTan = predNextTan
                      null
                    } else {
                      predNextTan
                    }
                  } else {
                    null
                  }
                } else null
                //val radius =  Math.max(Math.abs(lastx-tox)/50, Math.abs(lasty-toy)/50)
                if(prevTan != null) {
                  //canvas.drawCircle(prevTan._1, prevTan._2, radius, paint)
                  if(predNextTan != null) {
                    //canvas.drawCircle(predNextTan._1, predNextTan._2, radius, paint)
                    //tmpPath.cubicTo(predNextTan._1, predNextTan._2, prevTan._1, prevTan._2, tox, toy)
                  } else {
                    //tmpPath.quadTo(prevTan._1, prevTan._2, tox, toy)
                  }
                } else {
                  //tmpPath.lineTo(tox, toy)
                }
                tmpPath.lineTo(tox, toy)
                //canvas.drawLine(tox - radius, toy-radius, tox+radius, toy+radius, paint)
                //canvas.drawLine(tox - radius, toy+radius, tox+radius, toy-radius, paint)
                //canvas.drawCircle(tox, toy, radius, paint)
              } else {
                if(!tmpPath.isEmpty()) {
                  paint.setColor(lastcolor)
                  paint.setStrokeWidth(lastwidth)
                  
                  canvas.drawPath(tmpPath, paint)
                  tmpPath.reset()
                }
                tmpPath.moveTo(fromx, fromy)
                tmpPath.lineTo(tox, toy)
              }
              lastx = tox
              lasty = toy
              lastwidth = width
              lastcolor = color
            }
          }
          if(!tmpPath.isEmpty()) {
            paint.setColor(lastcolor)
            paint.setStrokeWidth(lastwidth)
            canvas.drawPath(tmpPath, paint)
            tmpPath.reset()
          }
          paint.setStrokeJoin(oldStrokeJoin)
          
          canvas.restore()
          
        case r: Rectangle =>
          if(r.x.get != r.x.next || r.y.get != r.y.next || r.width.get != r.width.next || r.height.get != r.height.next) {
            canvas.save()
            canvas.rotate(radToDegree(r.angle.get), r.x.get, r.y.get)
            canvas.drawRect(r.left.get, r.top.get, r.right.get, r.bottom.get, paintPrev)
            canvas.restore()
            canvas.drawLine(r.x.get, r.y.get, r.x.next, r.y.next, distancePaint)
          }
          canvas.save()
          canvas.rotate(radToDegree(r.angle.next), r.x.next, r.y.next)
          canvas.drawRect(r.left.next, r.top.next, r.right.next, r.bottom.next, paint)
          
          if (obj_to_highlight contains r) 
            canvas.drawRect(r.left.next, r.top.next, r.right.next, r.bottom.next, paintSelected)
          canvas.restore()
        case c: Circle => 
          if(c.x.get != c.x.next || c.y.get != c.y.next || c.radius.get != c.radius.next) {
            canvas.drawCircle(c.x.get, c.y.get, c.radius.get, paintPrev)
            canvas.drawLine(c.x.get, c.y.get, c.x.next, c.y.next, distancePaint)
          }
          canvas.drawCircle(c.x.next, c.y.next, c.radius.next, paint)
          if (obj_to_highlight contains c) 
            canvas.drawCircle(c.x.next, c.y.next, c.radius.next, paintSelected)
        
        case arr: Array2D =>
          paint.setStyle(Paint.Style.STROKE)
          canvas.drawRect(arr.left.next, arr.top.next, arr.right.next, arr.bottom.next, paint)
          if (obj_to_highlight contains arr) 
            canvas.drawRect(arr.left.next, arr.top.next, arr.right.next, arr.bottom.next, paintSelected)
          
          // reset the paint style
          paint.setStyle(Paint.Style.FILL)
                  
        case cell: Cell =>
          paint.setStyle(Paint.Style.STROKE)   
          canvas.drawRect(cell.left.next, cell.top.next, cell.right.next, cell.bottom.next, paint)
          if (obj_to_highlight contains cell) 
            canvas.drawRect(cell.left.next, cell.top.next, cell.right.next, cell.bottom.next, paintSelected)
          
          // reset the paint style
          paint.setStyle(Paint.Style.FILL)
        
        case b: IntBox => 
          paint.setTextSize(b.height.next)
          paint.setTextAlign(Paint.Align.LEFT)
          paintSelected.setTextAlign(Paint.Align.LEFT)

          canvas.save()
          canvas.rotate(radToDegree(b.angle.next), b.x.next, b.y.next)
          val value = if (b.name.next.trim.isEmpty) {
            paint.setTextAlign(Paint.Align.CENTER)
            paintSelected.setTextAlign(Paint.Align.CENTER)
            b.value.next.toString
          } else {
            b.name.next + ":" + b.value.next.toString
          }

          val textHeight = paint.descent - paint.ascent
          val textOffset = (textHeight / 2) - paint.descent

          canvas.drawText(value, b.x.next, b.y.next + textOffset, paint)
          if(obj_to_highlight contains b) {
            paintSelected.setTextSize(b.height.next)
            canvas.drawText(value, b.x.next, b.y.next + textOffset, paintSelected)
          }
          canvas.restore()
          
        case b: StringBox => 
          
          val valueDisplayNext = if(b.x.get != b.x.next || b.y.get != b.y.next || b.height.get != b.height.next) {
            paintPrev.setTextSize(b.height.get)
            canvas.save()
            canvas.rotate(radToDegree(b.angle.get), b.x.get, b.y.get)
            val value = b.value.get.toString
            canvas.drawText(value, b.x.get, b.y.get, paintPrev)
            canvas.restore()
            canvas.drawLine(b.x.get, b.y.get, b.x.next, b.y.next, distancePaint)
            b.value.next
          } else if(b.value.get != b.value.next) { // Same place, different value.
            b.value.get + "=>" + b.value.next
          } else {
            b.value.next
          }
          paint.setTextSize(b.height.next)
          paint.setTextAlign(Paint.Align.LEFT)
          
          canvas.save()
          canvas.rotate(radToDegree(b.angle.next), b.x.next, b.y.next)
          canvas.drawText(valueDisplayNext, b.x.next, b.y.next, paint)
          if(obj_to_highlight contains b) {
            paintSelected.setTextSize(b.height.next)
            paintSelected.setTextAlign(Paint.Align.LEFT)
            canvas.drawText(valueDisplayNext, b.x.next, b.y.next, paintSelected)
          }
          canvas.restore()

        case b: BooleanBox =>
          paint.setTextSize(b.height.next)
          paint.setTextAlign(Paint.Align.LEFT)
          canvas.save()
          canvas.rotate(radToDegree(b.angle.next), b.x.next, b.y.next)
          val c = b.value.next
          val h = b.height.next
          val x = b.x.next
          val y = b.y.next
          canvas.drawText(b.name.next, x + h*3/2, y+h/4, paint)
          if(obj_to_highlight contains b) {
            paintSelected.setTextSize(b.height.next)
            paintSelected.setTextAlign(Paint.Align.LEFT)
            canvas.drawText(b.name.next, b.x.next, b.y.next, paintSelected)
          }
          
          canvas.drawRect(x, y - h/2, x + h, y + h/2, paint)
          if(c) {
            paint.setColor(0xFFBBBBBB)
          } else {
            paint.setColor(0xFF333333)
          }
          canvas.drawCircle(x + h/2, y, h/2, paint)
          canvas.restore()
          
        case r: RandomGenerator =>
          paint.setTextSize(r.height.get)
          val value = r.value.get.toString
          canvas.drawText(value, r.x.get, r.y.get, paint)
          
        case j: Joystick =>
          paint.setAlpha(0x20)
          canvas.drawCircle(j.x.next, j.y.next, j.radius.next, paint)
          paint.setAlpha(0x40)
          canvas.drawCircle(j.x.next + j.relative_x.next, j.y.next + j.relative_y.next, j.radius.next/2, paint)
          if(obj_to_highlight contains j) canvas.drawCircle(j.x.next, j.y.next, j.radius.next, paintSelected)
          
        case r: Character =>
          canvas.save()
          canvas.rotate(radToDegree(r.angle.next), r.x.next, r.y.next)
          canvas.drawRect(r.x.next - r.width.next/2, r.y.next - r.height.next/2, r.x.next + r.width.next/2, r.y.next + r.height.next/2, paint)
          if(obj_to_highlight contains r) canvas.drawRect(r.x.next - r.width.next/2, r.y.next - r.height.next/2, r.x.next + r.width.next/2, r.y.next + r.height.next/2, paintSelected)
          canvas.restore()
      }
      
      o match {
        case e: Positionable with Directionable =>
          val c = e.color.next
          if(c >>> 24 == 0 && (bitmaps contains c))  { // It's a picture
            drawBitmapInGame(canvas, matrix, e, bitmaps(c), 0xFF)
          }
        case _ =>
      }
    }
  
    /** Draw a velocity vector from the given point */
    def drawVelocity(o: GameObject, x: Float, y: Float, velocity: Vec2, paint: Paint) {
      //o match { //canvas: Canvas, v: Float, x: Float, y: Float, vx: Float, vy: Float, paint: Paint
       // case o: Speed with Movable =>
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
       // case _ =>
      //}
    }
    
    if(game == null) return;
    game.pixelsByUnit = mapRadius(matrix, 1)

    game.aliveObjects foreach drawObject
    
    
    if(state == Editing) game.aliveObjects foreach {
      case o: Speed with Movable =>
        drawVelocity(o, o.x.next, o. y.next, o.velocity.next, velocityPaint)
        if(o.velocity.next.x != o.velocity.get.x || o.velocity.next.y != o.velocity.get.y) {
          drawVelocity(o, o.x.get, o.y.get, o.velocity.get, velocityPaintShaded)
        }
      case _ =>
    }
    canvas.restore()
    //this.objects foreach drawObject
    
    /*if(fingerIsDown) {
      paint.setColor(0xAAFF0000)
      canvas.drawCircle(currentFingerPos.x, currentFingerPos.y, game.FINGER_SIZE, paint)
    }*/
    if(isSelectingEvents) {
      EventDrawing.matrix = matrix
      EventDrawing.matrixI = matrixI
      EventDrawing.canvas = canvas
      EventDrawing.gameView = gameView
      EventDrawing.eventEditor = eventEditor
      EventDrawing.noAssignmentEvent = true
      // Divide the luminosity by two
      canvas.drawColor(0xFF808080, PorterDuff.Mode.MULTIPLY)
      // Add a quarter of the luminosity
      canvas.drawColor(0xFF404040, PorterDuff.Mode.ADD)
      

      game.foreachEvent(EventDrawing)
    }
    if(isFixingRules) {
      EventDrawing.matrix = matrix
      EventDrawing.matrixI = matrixI
      EventDrawing.canvas = canvas
      EventDrawing.gameView = gameView
      EventDrawing.eventEditor = eventEditor
      EventDrawing.noAssignmentEvent = false
      // Divide the luminosity by two
      canvas.drawColor(0xFF808080, PorterDuff.Mode.MULTIPLY)
      // Add a quarter of the luminosity, except more red.
      canvas.drawColor(0xFF804040, PorterDuff.Mode.ADD)
      
      game.foreachEvent(EventDrawing)
    }
    drawMenuOn(canvas, gameView, matrix, matrixI, state, eventEditor, shapeEditor)
    drawDebugOn(canvas, gameView)
  }
  
  def drawDebugOn(canvas: Canvas, gameView: GameView): Unit = {
    paint.setColor(ColorConstants.black)
    paint.setTextSize(10 * screenDensity)
    val value = "t = " + gameView.getGame.time
    canvas.drawText(value, 5 * screenDensity, 10 * screenDensity, paint)
  }
  
  def drawBitmapInGame(canvas: Canvas, matrix: Matrix, e: Positionable with Directionable, bitmap: Drawable, alpha: Int) = {
    canvas.restore()
    canvas.save()
    val center = render_out_vec
    render_in_array(0) = e.x.next
    render_in_array(1) = e.y.next
    mapVectorFromGame(matrix, render_in_array, center)
    canvas.rotate(radToDegree(e.angle.next), center.x, center.y)
    val d = bitmap
    val leftTop = render_out_vec2
    render_in_array(0) = e.left.next
    render_in_array(1) = e.top.next
    mapVectorFromGame(matrix, render_in_array, leftTop)
    val rightBottom = render_out_vec3
    render_in_array(0) = e.right.next
    render_in_array(1) = e.bottom.next
    mapVectorFromGame(matrix, render_in_array, rightBottom)
    d.setBounds(leftTop.x.toInt, leftTop.y.toInt, rightBottom.x.toInt, rightBottom.y.toInt)
    d.setAlpha(alpha)
    d.draw(canvas)
    canvas.restore()
    canvas.save()
    canvas.setMatrix(matrix)
  }
  
  /** Draws the menu on the canvas */
  def drawMenuOn(canvas: Canvas, gameView: GameView, matrix: Matrix, matrixI: Matrix, state: GameState, eventEditor: EventEditor, shapeEditor: ShapeEditor) = {
    MenuOptions.button_size = button_size
    state match {
      case Editing => 
        for((event, time) <- eventEditor.selectedEventTime) {
          drawEventOn(event, gameView, eventEditor, time, canvas, matrix, matrixI)
        }
        //StaticMenu.draw(canvas, this, shapeEditor.selectedShape, bitmaps, button_size/2, button_size/2)
        //if(game.currentTime == 0)
        //GameMenu.draw(canvas, this, shapeEditor.selectedShape, bitmaps, 0, 0)
      case Running =>
    }
    if(shapeEditor.selectedShape != null) {
      val (x, y) = shapeEditor.selectedShape match {
        case selectedShape: Positionable =>
            (selectedShape.x.getPrevNext,
             selectedShape.y.getPrevNext)
        case _ =>
          (0f, 0f)
      }
      val p = render_out_vec2
      render_in_array(0) = x
      render_in_array(1) = y
      mapVectorFromGame(matrix, render_in_array, p)
      val selectionx = Math.min(Math.max(p.x, button_size), canvas.getWidth() - button_size*3.5f)
      val selectiony = Math.min(Math.max(p.y, button_size*1.5f), canvas.getHeight() - button_size*1.5f)
      
      val cx = selectionx
      val cy = selectiony
      
      val bitmaps = gameView.bitmaps
      
      // Display the shape's menu.
      ShapeMenu.draw(canvas, gameView, shapeEditor.selectedShape, bitmaps, cx, cy)
      if(ColorMenu.activated && ColorMenu.registeredAction == None) {
        ColorMenu.draw(canvas, gameView, shapeEditor.selectedShape, bitmaps, PaintButton.getX(), PaintButton.getY())
      }
      if(SystemMenu.activated) {
        SystemMenu.draw(canvas, gameView, shapeEditor.selectedShape, bitmaps, SystemButton.getX(), SystemButton.getY())
      }
    }
    /*for((event, time) <- eventEditor.selectedEventTime) event match {
      case SelectableEvent(x, y) if EventMenu.isActivated =>
        val p = mapVectorFromGame(Vec2(x, y))
        EventMenu.draw(canvas, this, shapeEditor.selectedShape, bitmaps, p.x, p.y)
      case _ =>
    }*/
  }
  
  implicit val tmpManifold = new WorldManifold()
  object ExistenceTester extends (((ch.epfl.lara.synthesis.kingpong.rules.Events.Event, Int)) => Boolean) {
    private var mEvent: Event = _
    def setEvent(event: Event) = mEvent = event
    def apply(e: (Event, Int)) = e._1 == mEvent
  }
  // Draw events in the GameView referential
  def drawEventOn(event: Event, gameView: GameView, eventEditor: EventEditor, timestamp: Int, canvas: Canvas, matrix: Matrix, matrixI: Matrix): Unit = {
    var paint: Paint = this.paint
    ExistenceTester.setEvent(event)
    val eventIsSelected = eventEditor.selectedEventTime.exists(ExistenceTester)
    event match {
      case e if eventIsSelected =>
        if(event.isFinger) paint = touchSelectedPaint
      case FingerRelated(_) =>
        paint = touchDownPaint
      case _ =>
    }
    val game = gameView.getGame
    val dtime = (game.time - timestamp) * (if(event.isInstanceOf[FingerDown]) -1 else 1)
    val power = if(eventIsSelected) 1f else (if(dtime < 0) 0f else (300-Math.min(Math.max(dtime, 0), 300))/300f)
    var finger: List[(Float, Float, Int)] = Nil // The last int is the opacity
    val alpha = (0xEA*power + 0x20*(1-power)).round.toInt
    if(paint != null ) {
      // Emphasis for all events that appeared in the past.
      paint.setStrokeWidth((5*power + 2 * (1-power)).round.toInt)
      paint.setAlpha(alpha)
    }
    val bitmaps = gameView.bitmaps
    event match {
      case c: AssignmentEvent => 
        paint.setColor(0xFFFF0000)
        paint.setAlpha(alpha)
        val p = mapVectorFromGame(matrix, c.p, render_in_array, render_out_vec)
        rectFData.set(p.x - 24, p.y - 24, p.x + 25, p.y + 24)
        rectFData.round(rectData)
        val bitmap = if(eventEditor.selectedEventTime.indexWhere(_._1 == event) >= 0) bitmaps(R.drawable.bm_assignmentselected) else  bitmaps(R.drawable.bm_assignment)
        bitmap.setBounds(rectData)
        bitmap.setAlpha(alpha)
        canvas.drawCircle(p.x, p.y, mapRadiusI(matrixI, 10), paint) // TODO : Delete
        bitmap.draw(canvas)
      case c: BeginContact => 
        paint.setColor(0xFFFF0000)
        paint.setAlpha(alpha)
        val p = mapVectorFromGame(matrix, c.p, render_in_array, render_out_vec)
        rectFData.set(p.x - 24, p.y - 21, p.x + 25, p.y + 21)
        rectFData.round(rectData)
        val bitmap = if(eventEditor.selectedEventTime.indexWhere(_._1 == event) >= 0) bitmaps(R.drawable.bm_bingselected) else  bitmaps(R.drawable.bm_bing)
        bitmap.setBounds(rectData)
        bitmap.setAlpha(alpha)
        canvas.drawCircle(p.x, p.y, mapRadiusI(matrixI, 10), paint) // TODO : Delete
        bitmap.draw(canvas)
      case c: CurrentContact => 
        paint.setColor(0xFF00FF00)
        paint.setAlpha(alpha)
        val p = mapVectorFromGame(matrix, c.p, render_in_array, render_out_vec)
        canvas.drawCircle(p.x, p.y, mapRadiusI(matrixI, 10), paint)
      case c: EndContact => 
        paint.setColor(0xFF0000FF)
        paint.setAlpha(alpha)
        val p = mapVectorFromGame(matrix, c.p, render_in_array, render_out_vec)
        canvas.drawCircle(p.x, p.y, mapRadiusI(matrixI, 10), paint)
      case AccelerometerChanged(v) =>
        paint.setStrokeWidth(mapRadiusI(matrixI, 2))
        paint.setColor(0xFFFF00FF)
        paint.setAlpha(alpha)
        val pos = mapVectorToGame(matrixI, Vec2(100, 100))
        canvas.drawLine(pos.x, pos.y, pos.x + v.x*5, pos.y + v.y*5, paint)
      case FingerUp(v, obj) =>
        val p = mapVectorFromGame(matrix, v, render_in_array, render_out_vec)
        canvas.drawCircle(p.x, p.y, cross_size, circlePaint)
        canvas.drawCircle(p.x, p.y, cross_size, paint)
        if(timestamp == game.time) finger = (p.x, p.y, 0xBA)::finger
      case FingerDown(v, obj) =>
        val p = mapVectorFromGame(matrix, v, render_in_array, render_out_vec)
        drawCross(canvas, p.x, p.y, paint)
        if(timestamp == game.time) finger = (p.x, p.y, 0xBA)::finger
      case FingerMove(v, v2, obj) =>
        val p = mapVectorFromGame(matrix, v, render_in_array, render_out_vec)
        val p2 = mapVectorFromGame(matrix, v2, render_in_array, render_out_vec2)
        if(eventEditor.selectedEventTime.indexWhere(_._1 == event) >= 0) {
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
      case (x, y, alpha)::q => drawFinger(canvas, bitmaps(R.drawable.bm_finger), x, y, alpha)
        recDrawFinger(q)
    }
    recDrawFinger(finger)
  }

  
  /** Draws a cross at the given position */
  def drawCross(canvas: Canvas, x: Float, y: Float, paint: Paint) = {
    canvas.drawLine(x - cross_size, y - cross_size, x + cross_size, y + cross_size, paint)
    canvas.drawLine(x - cross_size, y + cross_size, x + cross_size, y - cross_size, paint)
  }
  
  def drawFinger(canvas: Canvas, fingerDrawable: Drawable, x: Float, y: Float, alpha: Int) = {
    val width = 72
    val height = 101
    val xc = 27
    val yc = 8
    val left = (x - xc).toInt
    val top = (y - yc).toInt
    fingerDrawable.setBounds(left, top, left + width - 1, top + height - 1)
    fingerDrawable.setAlpha(alpha)
    fingerDrawable.draw(canvas)
  }
  
  object EventDrawing extends((Event, Int) => Unit) {
    var noAssignmentEvent: Boolean = true
    var matrix: Matrix = _
    var matrixI: Matrix = _
    var canvas: Canvas = _
    var gameView: GameView = _
    var eventEditor: EventEditor = _
    def apply(event: Event, time: Int): Unit = {
      if(event.isInstanceOf[AssignmentEvent] ^ noAssignmentEvent)
        drawEventOn(event, gameView, eventEditor, time, canvas, matrix, matrixI)
    }
  }
  
  def maybePlay(gameView: GameView, sound: SoundTTS) = {
    if(sound.time.next < gameView.getGame.time && !soundsTTSplayed(sound)) {
      gameView.readAloud(sound.language.next, sound.value.next)
      soundsTTSplayed += sound
    } else if(sound.time.next >= gameView.getGame.time) {
      soundsTTSplayed -= sound
    }
  }
  
  def maybePlay(game: Game, sound: SoundRecorded) = { // Called when the game is running again.
    @inline def relativeTime(time: Int) = sound.duration * (time - sound.startTime)/(sound.endTime - sound.startTime)
    val relativeStartTime = relativeTime(game.time)
    if(game.time >= sound.endTime && (sounds contains sound)) {
      sounds(sound).release()
			sounds -= sound
    } else if(game.time >= sound.startTime && game.time <= sound.endTime) { // Must be playing
			sounds.get(sound) match {
			  case None =>
			    val mp = new MediaPlayer();
					mp.setAudioStreamType(AudioManager.STREAM_MUSIC);
					val myUri = Uri.parse(sound.uri);
					mp.setDataSource(context, myUri);
					mp.setOnPreparedListener(new MediaPlayer.OnPreparedListener {
					  def onPrepared(mp: MediaPlayer) = {
					    sound.duration = mp.getDuration()
					    mp.seekTo(sound.duration * (game.time - sound.startTime)/(sound.endTime - sound.startTime)) // Might be different when it is prepared.
					    mp.start()
					  }
					})
					sounds += sound -> mp
					mp.prepareAsync()
			  case Some(mediaPlayer) => // A Media player already existed.
			    try {
				    if(!mediaPlayer.isPlaying) { // Not yet playing or completed.
						  try { // If not yet playing
						    sound.duration = mediaPlayer.getDuration()
						    mediaPlayer.seekTo(sound.duration * (game.time - sound.startTime)/(sound.endTime - sound.startTime));
						    mediaPlayer.start()
						  } catch { // Not yet prepared. Need to wait for the preparation to be finished.
						    case e: IllegalStateException =>
						      mediaPlayer.setOnPreparedListener(new MediaPlayer.OnPreparedListener {
								    def onPrepared(mp: MediaPlayer) = {
								      sound.duration = mediaPlayer.getDuration()
								      mediaPlayer.seekTo(relativeTime(game.time));
								      mediaPlayer.start();
								    }
								  })
						  }
				    } else { // Playing: Just check that it is playing at the right moment the first time.
				      if(Math.abs(mediaPlayer.getCurrentPosition() - relativeTime(game.time)) > 500) {
				        //mediaPlayer.seekTo(relativeTime(game.time))
				      }
				    }
			    } catch {
			      case e: IllegalStateException =>
			    }
				}
    } else if(relativeStartTime < 0 && !(sounds contains sound)) {
      // Prepare for streaming.
      val mp = new MediaPlayer()
			mp.setAudioStreamType(AudioManager.STREAM_MUSIC)
			val myUri = Uri.parse(sound.uri)
			mp.setDataSource(context, myUri)
			mp.prepareAsync()
			sounds += sound -> mp
    }
  }
  
  /**
   * If all recorded sounds of this soundRecorder are before the current time, starts a new recording process.
   */
  def maybeRecord(game: Game, soundRecorder: SoundRecorder) {
    recorders.get(soundRecorder) match {
      case Some(mr) => // Do nothing. Already recording.
      case None =>
        var i = 0
        var minTimeRecording = -1
        while(i < soundRecorder.recordings.length) {
          val record = soundRecorder.recordings(i)
          minTimeRecording = Math.max(minTimeRecording, record.endTime)
          i += 1
        }
        if(minTimeRecording == -1 || minTimeRecording <= game.time && soundRecorder.recording.next) {
	        val record = new MediaRecorder
					record.setAudioSource(0 /*MediaRecorder.AudioSource.MIC*/)  // Default = microphone
					record.setOutputFormat(2/*MediaRecorder.OutputFormat.MPEG_4*/) // mpg4
					val sampleDir = Environment.getExternalStorageDirectory();
			    val audiofile = File.createTempFile(soundRecorder.name.get + soundRecorder.numRecords, ".mp4", sampleDir);
			    val uri = Uri.fromFile(audiofile)
					record.setOutputFile(audiofile.getAbsolutePath())
					record.setAudioEncoder(1/*MediaRecorder.AudioEncoder.AMR_NB*/) // AMR_NB
					//record.setAudioEncoder(MediaRecorder.getAudioSourceMax());
					record.setAudioEncodingBitRate(16)
					record.setAudioSamplingRate(44100)
					record.setAudioChannels(1)
					record.prepare()
					record.start()
	        recorders += soundRecorder -> ((game.time, record, uri.toString))
        }
    }
  }
  def stopRecording(game: Game) {
    for((soundRecorder, (startTime, record, uri)) <- recorders) {
      val obj = SoundRecorded(soundRecorder, soundRecorder.numRecords, startTime, game.time, uri)
      soundRecorder.recordings += obj
      game.add(obj)
      record.release()
    }
    recorders = Map()
  }
}


