package ch.epfl.lara.synthesis.kingpong

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import android.widget.SeekBar
import android.view.SurfaceView
import android.util.AttributeSet
import android.util.Log
import android.content.Context
import android.graphics.Canvas
import android.graphics.drawable.Drawable
import android.graphics.drawable.NinePatchDrawable
import android.graphics.RectF
import android.content.res.Resources
import android.graphics.Paint
import android.text.TextPaint
import android.graphics.Rect
import android.graphics.Path
import android.graphics.PathDashPathEffect
import android.graphics.drawable.BitmapDrawable
import android.graphics.PorterDuffColorFilter
import android.graphics.PorterDuff
import android.graphics.PorterDuff.Mode
import android.graphics.DashPathEffect
import android.util.DisplayMetrics
import android.view.WindowManager
import android.graphics.Color
import android.widget.Toast
import android.graphics.Typeface
import android.text.DynamicLayout
import android.text.Layout.Alignment
import android.content.res.Configuration
import ch.epfl.lara.synthesis.kingpong.ast.CodeGenerator
import ch.epfl.lara.synthesis.kingpong.ast.Category
import ch.epfl.lara.synthesis.kingpong.ast._
import ch.epfl.lara.synthesis.kingpong.menus._

/**
 * Implements a game engine view
 */
class GameEngine2DView(context: Context, attrs: AttributeSet, defStyle: Int) extends SurfaceView(context, attrs, defStyle) with GameEngineView with SeekBar.OnSeekBarChangeListener {
  import GameShapes._
  import TriggerEvent._
 
  def this(context: Context, attrs: AttributeSet) {
    this(context, attrs, 0)
  }
  def this(context: Context) {
    this(context, null, 0)
  }
  
  /** Selected objects, events and rules */
  var selectedShape: GameShapes.Shape = null
  var selectedEvent: ParameterHistory[TriggerEvent] = null    
  var selectedRule: ReactiveRule = null
  var selectedRuleConstants = new ArrayBuffer[(Int, Int, Int, Expression)]
  //selectedRuleConstants += ((2, 13, 1, Expression.NONE))
  var selectedRuleString: String = null // "testRuleDisplay() {\n  constant = 1\n}"
  var selectedRuleStringSplit: Array[String] = null //selectedRuleString.split("\n")
  var selectedCategory: Category[Shape] = null
  
  /** If the user is modifying an Integerbox, the selected rule is normally automatically reapplied. */
  var integerBoxWatched: IntegerBox = null // TODO : check the behavior
  var integerEvent: TriggerEvent = null
  
  /** Absolute start time of the game + editor */
  var startTime = 0L
  
  /** Total time spent in edit time*/
  private var totalEditTime = 0L
  
  /** When the last edit started */
  var enteredEditTime = 0L
  
  /** Time bar changes */
  private var mTimeBar: SeekBar = null
  def setTimeBar(timeBar: SeekBar) = {
    if(timeBar != null) {
      mTimeBar = timeBar
      mTimeBar.setMax(TriggerEvent.lengthStoredEvent)
      mTimeBar.setProgress(0)
      mTimeBar.setSecondaryProgress(0)
      mTimeBar.setOnSeekBarChangeListener(this)
    }
  }
  
  /** When the progress changes from the user. */
  def onProgressChanged(seekBar: SeekBar, progress: Int, fromUser: Boolean) = {
    if(fromUser && editMode) {
      game.returnToTime(game.maxTime + seekBar.getProgress() - seekBar.getSecondaryProgress())
      /*game.getArena foreach {
        shape => shape.storeNew()
      }*/
    }
  }
  def onStartTrackingTouch(seekBar: SeekBar):Unit = {
  }
  def onStopTrackingTouch(seekBar: SeekBar):Unit = {
  }
  
  /** Resets the game */
  def reset() {
    game.reset()
    totalEditTime = 0L
    minTimePosition = 0L
    mTimeBar.setProgress(0)
    mTimeBar.setSecondaryProgress(0)
    startTime = System.currentTimeMillis()
    selectShape(null)
    selectEvent(null)
    setModeModifyGame()
    game.AccelerometerGravity.reset()
    game.Gravity2D.reset()
    enterEditMode()
  }
  
  /** Resumes the game */
  override def onResume() {
    super.onResume()
  }
  
  /** Pauses the game */
  override def onPause() {
    super.onPause()
    enterEditMode()
  }

  /** Checks if the game is in edit mode */
  override def isInEditMode(): Boolean = editMode
  
  /** Enter game edit mode */
  def enterEditMode() {
    editMode = true
    enteredEditTime = System.currentTimeMillis()
    game.enterEditMode()
    computeMatrix()
  }

  /** Exit game edit mode*/
  def exitEditMode() {
    if(editMode) {
      game.exitEditMode()
      computeMatrix()

      selectShape(null)
      selectEvent(null)
      selectedCategory = null
      CameraButton.selected = false
      AccelerometerButton.selected = false
      Gravity2DButton.selected = false
      setModeModifyGame()
      //mAddRuleButton.text = res.getString(R.string.design_rule)
      MovingMenu.menus foreach { _.hovered = false}
      StaticMenu.menus foreach { _.hovered = false}
      mMenuPositionX = 0
      mMenuPositionY = 0
      if(enteredEditTime != 0) {
        totalEditTime += System.currentTimeMillis() - enteredEditTime - (game.currentTime - game.maxTime)
        enteredEditTime = 0
      }
      editMode = false
    }
  }
  
  var minTimePosition: Long = 0
  
  /** Updates the game physics */
  def updateGamePhysics() = {
    val systemTime = System.currentTimeMillis()
    
    var newTime = systemTime - startTime - totalEditTime
    
    if(game != null && !editMode && !paused) {
      if(1000 < newTime - game.currentTime) { // There was some delay and we are incrementing the time of the game too quickly.
        totalEditTime += newTime - game.currentTime - 10
        newTime -= newTime - game.currentTime - 10
      }
      game.advanceTimeTo(newTime)
    }
    if(mTimeBar != null && !paused && !editMode) {
      if(newTime - minTimePosition > TriggerEvent.lengthStoredEvent) {
        minTimePosition = newTime - TriggerEvent.lengthStoredEvent
      }
      val timeBarPosition = (newTime - minTimePosition).toInt
      mTimeBar.setSecondaryProgress(timeBarPosition)
      mTimeBar.setProgress(timeBarPosition)
    }
  }
  
  /** Draw this game on the canvas */
  /** Drawing data */
  /** Some drawables to display a nice graphical interface*/
  private val res: Resources = context.getResources()
  private val rectBitmapVert: NinePatchDrawable = res.getDrawable(R.drawable.btn_green_glossy_vert).asInstanceOf[NinePatchDrawable]
  private val rectBitmapHorz: NinePatchDrawable = res.getDrawable(R.drawable.btn_green_glossy_horz).asInstanceOf[NinePatchDrawable]
  private val circBitmap: Drawable = res.getDrawable(R.drawable.glowingball)
  private val bingBitmap : Drawable = res.getDrawable(R.drawable.bing)
  private val bingSelectedBitmap : Drawable = res.getDrawable(R.drawable.bingselected)
  private val numbersBitmap : Drawable = res.getDrawable(R.drawable.numbers)
  private val numbersSelectedBitmap : Drawable = res.getDrawable(R.drawable.numbersselected)
  private val zeroBitmap : Drawable = res.getDrawable(R.drawable.zero)
  private val zeroSelectedBitmap : Drawable = res.getDrawable(R.drawable.zeroselected)
  private val outscreenBitmap : Drawable = res.getDrawable(R.drawable.outscreen)
  private val outscreenSelectedBitmap : Drawable = res.getDrawable(R.drawable.outscreen_selected)
  private val useBitmaps = true
  
  private val allColors = res.getStringArray(R.array.colors)
  MenuOptions.allColors = allColors
  MenuOptions.context = context
  
  /** Menu drawing */
  val bitmaps = new HashMap[Int, Drawable]()
  val drawables_to_load:List[Int] =
    List(R.drawable.flat_button,
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
        R.drawable.flat_button_resizable_highlighted
        )
  drawables_to_load.foreach { id =>
    bitmaps(id) = res.getDrawable(id)
  }

  private val overlayNailBig : Drawable = res.getDrawable(R.drawable.nail_big)
  private val overlayCamera: Drawable = res.getDrawable(R.drawable.menu_camera)
  /** Menus and their effects
   */
  var mMenuPositionX: Float = 0
  var mMenuPositionY: Float = 0
  var mDisplacementX: Float = 0
  var mDisplacementY: Float = 0
  
 
  /** The possible states of the game */
  final val STATE_MODIFYING_GAME = 0 // All effects are applied immediately
  final val STATE_SELECTING_EVENTS = 1 // Only events can be selected 
  final val STATE_SELECTING_EFFECTS = 2 // Effects are applied only on the future state, the previous state remains the same.
  final val STATE_MODIFYING_CATEGORY = 3 // Objects are selected to be included to a given category
  var mRuleState = STATE_MODIFYING_GAME
  MenuOptions.copy_to_prev = true
  
  /**
   * Paints to use for drawing
   */
  var rectFData = new RectF(0, 0, 0, 0)
  var rectData = new Rect(0, 0, 0, 0)
  var defaultPaint = new Paint()
  defaultPaint.setColor(0xFF00FFFF)
  defaultPaint.setStyle(Paint.Style.FILL_AND_STROKE)
  defaultPaint.setStrokeWidth(2)
  defaultPaint.setAntiAlias(true)
  var whitePaint = new Paint()
  whitePaint.setColor(0xFFFFFFFF)
  whitePaint.setStyle(Paint.Style.FILL_AND_STROKE)
  whitePaint.setStrokeWidth(1)
  whitePaint.setAntiAlias(true)
  var blackPaint = new Paint()
  blackPaint.setColor(0xFF000000)
  blackPaint.setStyle(Paint.Style.FILL_AND_STROKE)
  blackPaint.setStrokeWidth(1)
  blackPaint.setAntiAlias(true)
  var shadedPaint = new Paint()
  shadedPaint.setColor(0x8800FFFF)
  shadedPaint.setStyle(Paint.Style.STROKE)
  shadedPaint.setStrokeWidth(2)
  shadedPaint.setAntiAlias(true)
  var velocityPaint = new Paint()
  velocityPaint.setColor(0x88FF00FF)
  velocityPaint.setStyle(Paint.Style.STROKE)
  velocityPaint.setStrokeWidth(4)
  velocityPaint.setAntiAlias(true)
  var velocityPaintShaded = new Paint()
  velocityPaintShaded.set(velocityPaint)
  velocityPaintShaded.setPathEffect(new DashPathEffect(Array[Float](5.0f,5.0f), 0))
  
  var selectPaint = new Paint()
  selectPaint.setStrokeWidth(4)
  selectPaint.setColor(0xAA00FF00)
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
  val mCodePaint = new TextPaint()
  mCodePaint.setTypeface(Typeface.MONOSPACE)
  mCodePaint.setAntiAlias(true)
  mCodePaint.setSubpixelText(true)
  mCodePaint.setColor(0xFFFFFFFF)
  val mCodeConstantPaint = new Paint()
  mCodeConstantPaint.setColor(0xAA00FFFF)
  mCodeConstantPaint.setStyle(Paint.Style.STROKE)
  mCodeConstantPaint.setAntiAlias(true)
  mCodeConstantPaint.setPathEffect(new DashPathEffect(Array[Float](5.0f,5.0f), 0))
  
  /** Custom color filters */
  private var filters = HashMap[Int, PorterDuffColorFilter]()
  private def getFilter(color: Int) = filters.getOrElseUpdate(color, { new PorterDuffColorFilter(color, Mode.MULTIPLY)} )
  val metrics = new DisplayMetrics();    
  context.getSystemService(Context.WINDOW_SERVICE).asInstanceOf[WindowManager].getDefaultDisplay().getMetrics(metrics);    
  lazy val button_size:Float = 80 * metrics.density
  
  /** Temporary coordinates for mapping */
  var drawcoords1 = Array(0.0f, 0.0f)
  var drawcoords2 = Array(0.0f, 0.0f)
  
  /** Draws the game on the given canvas */
  def drawGameOn(canvas: Canvas): Unit = {
    if(game == null) return
    drawcoords1(0) = 0
    drawcoords1(1) = 0
    drawcoords2(0) = mWidth
    drawcoords2(1) = mHeight
    mIMatrix.mapPoints(drawcoords1)
    mIMatrix.mapPoints(drawcoords2)

    // Display a dark box outside the active layout
    if(editMode) {
      canvas.drawRect(drawcoords1(0), drawcoords1(1), 0, drawcoords2(1), blackPaint)
      canvas.drawRect(drawcoords1(0), drawcoords1(1), drawcoords2(0), 0, blackPaint)
      canvas.drawRect(game.layoutWidth, drawcoords1(1), drawcoords2(0), drawcoords2(1), blackPaint)
      canvas.drawRect(drawcoords1(0), game.layoutHeight, drawcoords2(0), drawcoords2(1), blackPaint)
    }
        
    val shapes = game.getArena
    shapes foreach { s =>
      if(editMode || !s.isOutsideRectangle(game.Camera.x, game.Camera.y, game.Camera.x + game.Camera.width, game.Camera.y + game.Camera.height)) {
        s match {
          case r:GameShapes.Rectangle =>
            var rx = r.x
            var ry = r.y
            var width = r.width
            var height = r.height
            var color = r.color
            if(!r.visible) {
              color = (color & 0x00FFFFFF) | 0x80000000
            }
            rectFData.set(rx, ry, rx + width, ry + height)
            rectFData.round(rectData)
            if(r.visible || editMode) {
              if(rectBitmapVert != null && height >= width && useBitmaps) {
                rectBitmapVert.setColorFilter(getFilter(color))
                rectBitmapVert.setBounds(rectData)
                rectBitmapVert.draw(canvas)
              } else if(rectBitmapHorz != null && height <= width && useBitmaps) {
                rectBitmapHorz.setColorFilter(getFilter(color))
                rectBitmapHorz.setBounds(rectData)
                rectBitmapHorz.draw(canvas)
              } else {
                defaultPaint.setColor(color)
                if(!r.visible) defaultPaint.setAlpha(0x80)
                canvas.drawRect(rectFData, defaultPaint)
              }
            }
            if(editMode) {
              val middleX = rx + width / 2
              val middleY = ry + height / 2
              if(r.x != r.prev_x || r.y != r.prev_y || r.width != r.prev_width || r.height != r.prev_height) {
                rectFData.set(r.prev_x, r.prev_y, r.prev_x + r.prev_width, r.prev_y + r.prev_height)
                canvas.drawRect(rectFData, shadedPaint)
                canvas.drawLine(r.prev_x + r.prev_width / 2, r.prev_y + r.prev_height / 2, middleX, middleY, distancePaint)
              }
            }
          case c:GameShapes.Circle =>
            var cx = c.x
            var cy = c.y
            var radius = c.radius
            var color = c.color
            if(!c.visible) {
              color = (color & 0x00FFFFFF) | 0x80000000
            }
            if(editMode) {
              if(ghostModeActivated) {
                c.history_x.foreachWithUntil(c.history_y, c.history_radius, game.currentTime){
                  (i1, i2, i3) =>
                   canvas.drawCircle(i1.value, i2.value, i3.value, shadedPaint)
                }
              }
            }
            if(c.visible || editMode) {
              if(circBitmap != null && useBitmaps) {
                rectFData.set(cx - radius+2, cy - radius+2, cx + radius-2, cy + radius-2)
                rectFData.round(rectData)
                circBitmap.setBounds(rectData)
                circBitmap.setColorFilter(getFilter(color))
                circBitmap.draw(canvas)
              } else {
                defaultPaint.setColor(color)
                if(!c.visible) defaultPaint.setAlpha(0x80)
                canvas.drawCircle(cx, cy, radius, defaultPaint)
              }
            }
            if(editMode && (c.x != c.prev_x || c.y != c.prev_y)) {
              canvas.drawCircle(c.prev_x, c.prev_y, radius, shadedPaint)
              canvas.drawLine(c.x, c.y, c.prev_x, c.prev_y, distancePaint)
            }
          case d:GameShapes.IntegerBox =>
            var x = d.x
            var y = d.y
            var height = d.height
            var color = d.color
            defaultPaint.setColor(color)
            if(!d.visible) defaultPaint.setAlpha(0x80)
            defaultPaint.setTextSize(height)
            val text = if(editMode) d.getStringModify else d.getString
            defaultPaint.getTextBounds(text, 0, text.length(), rectData)
            val shift = (rectData.bottom - rectData.top)/2 + d.height/2
            if(d.visible || editMode) canvas.drawText(text, x, y + shift, defaultPaint)          
            if(editMode) {
              //rectFData.set(x, y, x + d.width, y + d.height)
              //canvas.drawRect(rectFData, distancePaint)
              if(d.x != d.prev_x || d.y != d.prev_y) {
                shadedPaint.setTextSize(height);
                canvas.drawText(text, d.prev_x, d.prev_y + shift, shadedPaint)
                canvas.drawLine(d.prev_x, d.prev_y, d.x, d.y, distancePaint)
              }
            }
          case d:GameShapes.TextBox =>
            var x = d.x
            var y = d.y
            var height = d.height
            var width = d.width
            var color = d.color
            defaultPaint.setColor(color)
            if(!d.visible) defaultPaint.setAlpha(0x80)
            defaultPaint.setTextSize(height)
            val text = if(editMode) d.getStringModify else d.getString
            if(text != null) {
              defaultPaint.getTextBounds(text, 0, text.length(), rectData)
              val shift = (rectData.bottom - rectData.top)/2 + d.height/2
              if(d.visible || editMode) canvas.drawText(text, x, y + shift, defaultPaint)
              if(editMode && (d.x != d.prev_x || d.y != d.prev_y)) {
                shadedPaint.setTextSize(d.height);
                canvas.drawText(text, d.prev_x, d.prev_y, shadedPaint)
                canvas.drawLine(d.prev_x, d.prev_y + shift, d.x, d.y + shift, distancePaint)
              }
              if(selectedShape == d) {
                canvas.drawLine(x, y, x, y + height, distancePaint)
                canvas.drawLine(x, y + height, x + width, y + height, distancePaint)
                canvas.drawLine(x + width, y + height, x + width, y, distancePaint)
                canvas.drawLine(x + width, y, x, y, distancePaint)
              }
            }
          case _ =>
        }
        // Draws the arrow
        if(editMode) {
          //if(!s.noVelocity) {
            val v = s.velocity
            if(v != 0) {
              drawVelocity(canvas, s.velocity, s.x, s.y, s.velocity_x, s.velocity_y, velocityPaint)
              if(s.velocity_x != s.prev_velocity_x || s.velocity_y != s.prev_velocity_y) {
                drawVelocity(canvas, s.prev_velocity, s.prev_x, s.prev_y, s.prev_velocity_x, s.prev_velocity_y, velocityPaintShaded)
              }
            }
          //}
          if(s.noVelocity ){
            val sx = s.centerX
            val sy = s.centerY
            rectFData.set(sx+1, sy-50, sx+41, sy+1)
            rectFData.round(rectData)
            overlayNailBig.setBounds(rectData)
            overlayNailBig.draw(canvas)
          }
        }
      }
    }
    
    if(CameraButton.selected) {
      var x = game.Camera.x
      var y = game.Camera.y
      var height = game.Camera.height
      var width = game.Camera.width
      canvas.drawLine(x, y, x, y + height, distancePaint)
      canvas.drawLine(x, y + height, x + width, y + height, distancePaint)
      canvas.drawLine(x + width, y + height, x + width, y, distancePaint)
      canvas.drawLine(x + width, y, x, y, distancePaint)
      rectFData.set(x+3*width/8, y+3*height/8, x+5*width/8, y+5*height/8)
      rectFData.round(rectData)
      overlayCamera.setBounds(rectData)
      overlayCamera.setAlpha(0x77)
      overlayCamera.draw(canvas)
      overlayCamera.setAlpha(0xFF)
    }
     
    // Displays the current category as selected objects
    if(selectedCategory != null) {
      selectedCategory foreach { shape => 
        shape match {
          case r:GameShapes.Rectangular =>
          var rx = r.x
          var ry = r.y
          var width = r.width
          var height = r.height

          rectFData.set(rx, ry, rx + width, ry + height)
          rectFData.round(rectData)
          if(r.visible || editMode) {
            canvas.drawRect(rectFData, selectPaint)
          }
        case c:GameShapes.Circle =>
          var cx = c.x
          var cy = c.y
          var radius = c.radius
          canvas.drawCircle(cx, cy, radius, selectPaint)
        case _ =>
        }
      }
    }
    
    // Display a dark box around the visible zone.
    if(!editMode) {
      canvas.drawRect(drawcoords1(0), drawcoords1(1), game.Camera.x, drawcoords2(1), blackPaint)
      canvas.drawRect(drawcoords1(0), drawcoords1(1), drawcoords2(0), game.Camera.y, blackPaint)
      canvas.drawRect(game.Camera.x + game.Camera.width, drawcoords1(1), drawcoords2(0), drawcoords2(1), blackPaint)
      canvas.drawRect(drawcoords1(0), game.Camera.y + game.Camera.height, drawcoords2(0), drawcoords2(1), blackPaint)
    }
  }
  
  /** Draw a velocity vector from the given point */
  def drawVelocity(canvas: Canvas, v: Float, x: Float, y: Float, vx: Float, vy: Float, paint: Paint) {
    val middleX = x
    val middleY = y
    val toX = middleX + vx * 1000
    val toY = middleY + vy * 1000
    canvas.drawLine(middleX, middleY, toX, toY, paint)
    val unitVectorX = vx / v
    val unitVectorY = vy / v
    val cosRot = -0.87f * 20
    val sinRot = 0.5f * 20
    canvas.drawLine(toX, toY, toX + cosRot*unitVectorX + sinRot*unitVectorY, toY - sinRot*unitVectorX + cosRot*unitVectorY, velocityPaint)
    canvas.drawLine(toX, toY, toX + cosRot*unitVectorX - sinRot*unitVectorY, toY + sinRot*unitVectorX + cosRot*unitVectorY, velocityPaint)
  }
  
  /** Temporary coordinates for mapping */
  var mapcoords = Array(0.0f, 0.0f)
  final val cross_size = 15 // Size for the cross corresponding to the finger down event.
  /** Draws a cross at the given position */
  def drawCross(canvas: Canvas, x: Float, y: Float, paint: Paint) = {
    canvas.drawLine(coords(0) - cross_size, coords(1) - cross_size, coords(0) + cross_size, coords(1) + cross_size, paint)
    canvas.drawLine(coords(0) - cross_size, coords(1) + cross_size, coords(0) + cross_size, coords(1) - cross_size, paint)
  }
  
  /** Draws the input event on the canvas */
  def drawInputEventOn(i: ParameterHistory[TriggerEvent], canvas: Canvas) = {
    var paint: Paint = null
    if(selectedEvent == i) {
      i.value.code match {
        case TOUCHDOWN_EVENT | TOUCHUP_EVENT =>
          paint = touchSelectedPaint
        case TOUCHMOVE_EVENT =>
          paint = touchSelectedPaint
        case _ =>
      }
    } else {
      i.value.code match {
        case TOUCHDOWN_EVENT =>
          paint = touchDownPaint
        case TOUCHUP_EVENT =>
          paint = touchUpPaint
        case TOUCHMOVE_EVENT =>
          paint = touchMovePaint
        case _ =>
      }
    }
    if(paint != null ) {
      if((i.timestamp < game.currentTime) == (i.value.code == TOUCHUP_EVENT)) {
        paint.setStrokeWidth(5)
        paint.setAlpha(0xAA)
      } else {
        paint.setStrokeWidth(2)
        paint.setAlpha(0x88)
      }
    }
    i.value.code match {
      case TOUCHDOWN_EVENT =>
        coords(0) = i.value.x1
        coords(1) = i.value.y1
        mMatrix.mapPoints(coords)
        drawCross(canvas, coords(0), coords(1), paint)
        
        //canvas.drawCircle(coords(0), coords(1), 20, circlePaint)
        //canvas.drawCircle(coords(0), coords(1), 20, paint)
      case TOUCHUP_EVENT =>
        coords(0) = i.value.x1
        coords(1) = i.value.y1
        mMatrix.mapPoints(coords)
        canvas.drawCircle(coords(0), coords(1), 15, circlePaint)
        canvas.drawCircle(coords(0), coords(1), 15, paint)
      case TOUCHMOVE_EVENT =>
        coords(0) = i.value.x1
        coords(1) = i.value.y1
        mMatrix.mapPoints(coords)
        mapcoords(0) = i.value.x2
        mapcoords(1) = i.value.y2
        mMatrix.mapPoints(mapcoords)
        if(i == selectedEvent) {
          drawCross(canvas, coords(0), coords(1), touchSelectedPaint)

          canvas.drawCircle(mapcoords(0), mapcoords(1), 15, touchSelectedPaint)
        }
        canvas.drawLine(coords(0), coords(1), mapcoords(0), mapcoords(1), paint)
      case COLLISION_EVENT => 
        coords(0) = i.value.x1
        coords(1) = i.value.y1
        mMatrix.mapPoints(coords)
        val ix1 = coords(0)
        val iy1 = coords(1)
        rectFData.set(ix1 - 24, iy1 - 21, ix1 + 25, iy1 + 21)
        rectFData.round(rectData)
        val bitmap = if(i == selectedEvent) bingSelectedBitmap else bingBitmap
        bitmap.setBounds(rectData)
        if(i.timestamp < game.currentTime || i == selectedEvent) bitmap.setAlpha(255) else bitmap.setAlpha(128)
        bitmap.draw(canvas)
      case BEYOND_SCREEN_EVENT | BEYOND_SIDE_EVENT =>
        coords(0) = i.value.x1
        coords(1) = i.value.y1
        mMatrix.mapPoints(coords)
        val ix1 = coords(0)
        val iy1 = coords(1)
        rectFData.set(ix1 - 23, iy1 - 19, ix1 + 22, iy1 + 20)
        rectFData.round(rectData)
        val bitmap = if(i == selectedEvent) outscreenSelectedBitmap else outscreenBitmap
        bitmap.setBounds(rectData)
        if(i.timestamp < game.currentTime || i == selectedEvent) bitmap.setAlpha(255) else bitmap.setAlpha(128)
        bitmap.draw(canvas)
      case INTEGER_CHANGE_EVENT | INTEGER_EQUAL_EVENT | INTEGER_GREATER_EQUAL_EVENT | INTEGER_LESS_EQUAL_EVENT | INTEGER_POSITIVE_EVENT | INTEGER_NEGATIVE_EVENT =>
        coords(0) = i.value.x1
        coords(1) = i.value.y1
        mMatrix.mapPoints(coords)
        val ix1 = coords(0)
        val iy1 = coords(1)
        rectFData.set(ix1 - 20, iy1 - 20, ix1 + 19, iy1 + 19)
        rectFData.round(rectData)
        val bitmap = if(i == selectedEvent) numbersSelectedBitmap else numbersBitmap
        bitmap.setBounds(rectData)
        if(i.timestamp < game.currentTime || i == selectedEvent) bitmap.setAlpha(255) else bitmap.setAlpha(128)
        bitmap.draw(canvas)
      case _ =>
    }
  }
  
  /** Draws the menu on the canvas */
  def drawMenuOn(canvas: Canvas) = {
    MenuOptions.button_size = button_size
    if(editMode) {
      if(selectedEvent != null) {
        drawInputEventOn(selectedEvent, canvas)
      }
      if(mRuleState == STATE_SELECTING_EVENTS) {
        game.triggerEvents.foreachFromFarAway(game.currentTime) {i: ParameterHistory[TriggerEvent] =>
          drawInputEventOn(i, canvas)
        }
        game.inputEvents.foreachFromFarAway(game.currentTime){i: ParameterHistory[TriggerEvent] =>
          drawInputEventOn(i, canvas)
        }
      }
    }
    if(editMode) {
      StaticMenu.draw(canvas, this, selectedShape, bitmaps, button_size/2, button_size/2)
    }
    if(selectedShape != null) {
      if(MenuOptions.modify_prev) {
        coords(0) = selectedShape.prevCenterX
        coords(1) = selectedShape.prevCenterY
      } else {
        coords(0) = selectedShape.centerX
        coords(1) = selectedShape.centerY
      }
      mMatrix.mapPoints(coords)
      val selectionx = Math.min(Math.max(coords(0), button_size), mWidth - button_size*3.5f)
      val selectiony = Math.min(Math.max(coords(1), button_size*1.5f), mHeight-button_size*1.5f)
      
      val cx = selectionx + mMenuPositionX
      val cy = selectiony + mMenuPositionY
      
      // Display the shape's menu.
      MovingMenu.draw(canvas, this, selectedShape, bitmaps, cx, cy) * button_size
    }
  }
  
  /** Conversion coordinates */
  var touchCoords = Array(0.0f, 0.0f)
  var touchCoords2 = Array(0.0f, 0.0f)
  var touchCoordsDownGame = Array(0.0f, 0.0f)
  var touchCoordsDownOriginal = Array(0.0f, 0.0f)
  var selectedShapeGameCoords = Array(0.0f, 0.0f)
  
  /**Called when the finger is down at these coordinates */
  def onFingerDown(x: Float, y: Float):Boolean = {
    val return_value = highLightMenu(x, y)
    touchCoordsDownGame(0) = x
    touchCoordsDownGame(1) = y
    mIMatrix.mapPoints(touchCoordsDownGame)
    touchCoordsDownOriginal(0) = x
    touchCoordsDownOriginal(1) = y
    if(selectedShape != null) {
      if(MenuOptions.modify_prev) {
        selectedShapeGameCoords(0) = selectedShape.prev_x
        selectedShapeGameCoords(1) = selectedShape.prev_y
      } else {
        selectedShapeGameCoords(0) = selectedShape.x
        selectedShapeGameCoords(1) = selectedShape.y
      }
      MenuOptions.selected_shape_first_x = selectedShapeGameCoords(0)
      MenuOptions.selected_shape_first_y = selectedShapeGameCoords(1)
    }
    if(PaintButton.hovered) {
      mDisplacementX = 0
      mDisplacementY = 0
    }
    if(MoveButton.hovered) {
      mDisplacementX = 0
      mDisplacementY = 0
    }
    return_value
  }
  
  /** Selects a given rule. Adds a watcher on the integer if the rule selected depends on an integer. */
  def selectRule(rule: ReactiveRule, event: TriggerEvent = null) = {
    selectedRule = rule
    selectedRuleConstants.clear()
    selectedRuleString = rule.toScalaString("", game.context, selectedRuleConstants, Some(1), Some(1))
    selectedRuleStringSplit = selectedRuleString.split("\n")
    integerBoxWatched = null
    if(event != null) {
      if(event.code == INTEGER_CHANGE_EVENT) {
        integerBoxWatched = event.shape1.asInstanceOf[IntegerBox]
        integerEvent = event
      }
    } else {
      integerBoxWatched = null
      integerEvent = null
    }
  }
  
  /** Finds or creates an event for a given rule. */
  def findEventForRule(rule: ReactiveRule): TriggerEvent = {
    rule match {
      case d@WhenEverRule(condition, code) =>
        null
      case d@WhenFingerMovesOnRule(obj, coords, code) =>
        null
      case d@WhenFingerDownOnRule(EIdentShape(shape1), code) =>
        val c = new TriggerEvent
        c.code = TOUCHDOWN_EVENT
        c.shape1 = shape1
        c.shape2 = null
        c.x1 = c.shape1.centerX
        c.y1 = c.shape1.centerY
        c
      case d@WhenFingerUpOnRule(EIdentShape(shape1), code) =>
        val c = new TriggerEvent
        c.code = TOUCHUP_EVENT
        c.shape1 = shape1
        c.shape2 = null
        c.x1 = c.shape1.centerX
        c.y1 = c.shape1.centerY
        c
      case d@WhenCollisionBetweenRule(EIdentShape(shape1), EIdentShape(shape2), code) =>
        val c = new TriggerEvent
        c.code = COLLISION_EVENT
        c.shape1 = shape1
        c.shape2 = shape2
        c.x1 = (shape1.x + shape2.x)/2
        c.y1 = (shape1.y + shape2.y)/2
        c
      case d@WhenIntegerChangesRule(EIdentShape(shape1), coords, code) =>
        val c = new TriggerEvent
        c.code = INTEGER_CHANGE_EVENT
        c.shape1 = shape1
        c.shape2 = null
        c.x1 = shape1.x
        c.y1 = shape1.y
        c
      case d@NoCollisionBetweenRule(obj1, obj2) => 
        null
      case d@NoCollisionEffectBetweenRule(obj1, obj2) =>
        null
      case _ => null
    }
  }
  
  /**
   * Called when the finger is released at these coordinates
   * Handles menu interaction
   **/
  def onFingerUp(x: Float, y: Float) = {
    //Log.d("GameEngine2DView", "Menu Selected at " + x + ", " + y)
    var menuSelected:Boolean = false
    if(selectedShape != null) {
      menuSelected = MovingMenu.onFingerUp(this, selectedShape, x, y)
    }
    if(!menuSelected) { // Test is top left menu activated
      mMenuPositionX = 0
      mMenuPositionY = 0
      
      menuSelected = StaticMenu.onFingerUp(this, selectedShape, x, y)
    }

    if(!menuSelected) {
      // If the previous attempts to select a menu failed,
      // We dissmiss all selection and we select something else depending on the game editor type.
      MovingMenu.menus foreach {
        _.hovered = false
      }
      
      touchCoords(0) = x
      touchCoords(1) = y
      mIMatrix.mapPoints(touchCoords)
      var oneShapeSelectable = false
      
      var closestTimeStampDiff: Long = -1
      var closestEvent: ParameterHistory[TriggerEvent] = null
      
      // If we are making a rule, we select the events first
      if(mRuleState == STATE_SELECTING_EVENTS) {
        selectEvent(null)
        def updateClosest(i: ParameterHistory[TriggerEvent]): Unit = {
          i.value.code match {
            case COLLISION_EVENT | TOUCHMOVE_EVENT | TOUCHUP_EVENT | TOUCHDOWN_EVENT | BEYOND_SCREEN_EVENT | BEYOND_SIDE_EVENT | INTEGER_CHANGE_EVENT | INTEGER_EQUAL_EVENT | INTEGER_GREATER_EQUAL_EVENT | INTEGER_LESS_EQUAL_EVENT => 
              //canvas.drawCircle(i.x1, i.y1, 20, touchMovePaint)
              if(i.value.selectableBy(touchCoords(0), touchCoords(1))) {
                val distTime = Math.abs(i.timestamp - game.currentTime)
                if(distTime < closestTimeStampDiff || (distTime == closestTimeStampDiff && (i.value.code == TOUCHUP_EVENT || i.value.code == TOUCHDOWN_EVENT)) || closestTimeStampDiff == -1) {
                  closestEvent = i
                  closestTimeStampDiff = distTime
                }
              }
            case _ =>
          }
        }
        game.inputEvents foreach (updateClosest(_))
        game.triggerEvents foreach (updateClosest(_))
        if(closestEvent != null) {
          selectEvent(closestEvent)
          //game.storeInitialState(game.currentTime == 0)
          //game.prepareNewValues()
          if(closestEvent.value.code == INTEGER_CHANGE_EVENT ||
             closestEvent.value.code == INTEGER_EQUAL_EVENT ||
             closestEvent.value.code == INTEGER_GREATER_EQUAL_EVENT ||
             closestEvent.value.code == INTEGER_LESS_EQUAL_EVENT ||
             closestEvent.value.code == INTEGER_POSITIVE_EVENT ||
             closestEvent.value.code == INTEGER_NEGATIVE_EVENT
          ) {

            val d = closestEvent.value.shape1.asInstanceOf[IntegerBox]
            val name = d.mName
            val oldValue = closestEvent.value.x2.toInt
            val newValue = closestEvent.value.y2.toInt
            val integer = new Integer(newValue)
            CustomDialogs.launchChoiceDialog(context, String.format(res.getString(R.string.choose_trigger), name),
                List(
                  String.format(res.getString(R.string.trigger_integer_1), name),
                  String.format(res.getString(R.string.trigger_integer_2), name, integer),
                  String.format(res.getString(R.string.trigger_integer_3), name, integer),
                  String.format(res.getString(R.string.trigger_integer_4), name, integer))
                  ++ (if(newValue > 0)
                        List(String.format(res.getString(R.string.trigger_integer_5), name)) else 
                     (if(newValue < 0)
                        List(String.format(res.getString(R.string.trigger_integer_6), name)) else Nil))
                  ,
                     {i:Int =>
                       closestEvent.value.code = INTEGER_CHANGE_EVENT + i
                       if(i == 4 && newValue < 0) closestEvent.value.code = INTEGER_NEGATIVE_EVENT
                       setModeSelect()
                     },
                     {() =>})
          } else if(closestEvent.value.code == BEYOND_SCREEN_EVENT ||
             closestEvent.value.code == BEYOND_SIDE_EVENT) {
            val d = closestEvent.value.shape1
            val name = d.mName
            CustomDialogs.launchChoiceDialog(context, String.format(res.getString(R.string.choose_trigger_outofscreen), name),
                List(
                  String.format(res.getString(R.string.trigger_outscreen_1), name),
                  String.format(res.getString(R.string.trigger_outscreen_2), name)),
                     {i:Int =>
                       closestEvent.value.code = BEYOND_SCREEN_EVENT + i
                       setModeSelect()},
                     {() =>})
          } else if(closestEvent.value.code == COLLISION_EVENT) {
            val d1 = closestEvent.value.shape1
            val d2 = closestEvent.value.shape2
            CustomDialogs.launchChoiceDialog(context, String.format(res.getString(R.string.choose_collision), d1.mName, d2.mName),
                List(
                  String.format(res.getString(R.string.trigger_collision_1)),
                  String.format(res.getString(R.string.trigger_collision_2), d1.mName, d2.mName),
                  String.format(res.getString(R.string.trigger_collision_3), d1.mName, d2.mName)),
                  {i: Int =>
                    if(i == 0) {
                      setModeSelect()
                    } else if(i == 1) {
                      game.insertRule(NoCollisionBetweenRule(EIdentShape(d1), EIdentShape(d2)), game.currentTime)
                      setModeModifyGame()
                    } else {
                      game.insertRule(NoCollisionEffectBetweenRule(EIdentShape(d1), EIdentShape(d2)), game.currentTime)
                      setModeSelect()
                    }
                  },
                  {() =>}
                )
          } else {
            setModeSelect()
          }
        }
      } else if(mRuleState == STATE_MODIFYING_CATEGORY) {
        modifySelection(touchCoords(0), touchCoords(1))
      } else {
        chooseSelectedShape(touchCoords(0), touchCoords(1))
      }
    }
  }
  
  def setModeSelectCategory() = {
    mRuleState = STATE_MODIFYING_CATEGORY
    MenuOptions.copy_to_prev = true
    MenuOptions.modify_prev = false
  }
  
  /** Switches the current mode to selecting effects */
  def setModeSelect(): Unit = {
    mRuleState = STATE_SELECTING_EFFECTS
    MenuOptions.copy_to_prev = false
    MenuOptions.modify_prev = false
    Toast.makeText(context, res.getString(R.string.select_effects_toast), 2000).show()
  }
  
  /** Switches the current mode to selecting events. */
  def setModeSelectingEffects() = {
    mRuleState = STATE_SELECTING_EVENTS
    MenuOptions.modify_prev = false // irrelevant
    MenuOptions.copy_to_prev = true // irrelevant
  }
  
  /** Switches the current mode to the global modification of the game */
  def setModeModifyGame() {
    mRuleState = STATE_MODIFYING_GAME
    MenuOptions.modify_prev = false
    MenuOptions.copy_to_prev = true
    enterEditMode()
    AddRuleButton.hovered = false
    selectEvent(null)
  }

  /** Select the shape closest to the coordinates */
  def chooseSelectedShape(xTouch: Float, yTouch: Float) = {
    var minDistance = -1.0f
    selectShape(null)
    
    game.getArena foreach { shape =>
      val x = xTouch + (if(MenuOptions.modify_prev) shape.x - shape.prev_x else 0)
      val y = yTouch + (if(MenuOptions.modify_prev) shape.y - shape.prev_y else 0)// - shape.y + shape.prev_y
      if(shape.selectableBy(x, y)) {
        val dist = shape.distanceSelection(x, y)
        if(dist < minDistance || minDistance == -1) {
          minDistance = dist
          selectShape(shape)
        }
      }
    }
    if(selectedShape == null) {
      mMenuPositionX = 0
      mMenuPositionY = 0
      if(CameraButton.selected) {
        game.Camera.reset()
      }
    }
  }
  
  /** Changes the selection of the category. */
  def modifySelection(xTouch: Float, yTouch: Float) = {
    if(selectedCategory != null)  {
      game.getArena foreach { shape =>
        val x = xTouch - shape.x + shape.prev_x
        val y = yTouch - shape.y + shape.prev_y
        if(shape.selectableBy(x, y) && shape.distanceSelection(x, y) == 0) {
          selectedCategory.turnOnOff(shape)
        }
      }
    }
  }
  
  /** Called when the finger moves by a small amount */
  def onFingerMove(xFrom: Float, yFrom: Float, xTo: Float, yTo: Float) = {
    //Log.d("Menu", "Menu hovered (" + mMenuPositionX + ", " + mMenuPositionY + ") from " + xFrom + ", " + yFrom + " to " + xTo + ", " + yTo)
    touchCoords(0) = xTo
    touchCoords(1) = yTo
    touchCoords2(0) = xFrom
    touchCoords2(1) = yFrom
    mDisplacementX = xTo - touchCoordsDownOriginal(0)
    mDisplacementY = yTo - touchCoordsDownOriginal(1)
    mIMatrix.mapPoints(touchCoords)
    mIMatrix.mapPoints(touchCoords2)
    val shiftX = touchCoords(0) - touchCoords2(0)
    val shiftY = touchCoords(1) - touchCoords2(1)
    
    var relativeX = touchCoords(0) - touchCoordsDownGame(0)
    var relativeY = touchCoords(1) - touchCoordsDownGame(1)
    if(Math.abs(relativeX) < 10) {
      relativeX = 0
    }
    if(Math.abs(relativeY) < 10) {
      relativeY = 0
    }
    MovingMenu.onFingerMove(selectedShape, relativeX, relativeY, shiftX, shiftY, mDisplacementX, mDisplacementY)
    if(MoveButton.hovered) {
      mMenuPositionX = 0
      mMenuPositionY = 0
    }
  }
  
  /** Highlights the corresponding menu under the finger*/
  def highLightMenu(x: Float, y: Float): Boolean = {
    if(selectedShape != null) {
      MovingMenu.testHovering(x, y, button_size)
    }
    val menu_not_hovered = MovingMenu.menus.foldLeft(true) { (notHovered, menu ) => notHovered && !menu.hovered }
    if(menu_not_hovered) {
      StaticMenu.menus foreach (_.testHovering(x, y, button_size))
    }
    var something_highlighted = false
    MovingMenu.menus foreach { menu => something_highlighted = something_highlighted || menu.hovered }
    StaticMenu.menus foreach { menu => something_highlighted = something_highlighted || menu.hovered }
    something_highlighted
  }

  /** Renames the selected shape and tries to find an unexisting name
   *  Removes the trailing numbers at the end
   **/
  def renameSelectedShape(baseName: String): Unit = {
    if(selectedShape != null) {
      val newName = getBrandNewName(baseName, game.context)
      game.context.remove(selectedShape.mName)
      selectedShape named newName
      game.context(newName) = EIdentShape(selectedShape)
    }
  }

  /**
   * Select a given shape.
   **/
  def selectShape(shape: GameShapes.Shape) = {
    selectedShape = shape
    if(selectedShape != null) {
      if(CameraButton.selected && selectedCategory != null) {
        selectedCategory.add(shape)
        selectedShape = game.Camera
      }

      mMenuPositionX = 0 //touchCoords(0) - touchCoordsDownOriginal(0)
      mMenuPositionY = 0 //touchCoords(1) - touchCoordsDownOriginal(1)
    }
  }

  /**
   * Selects the given event by returning to the time this event occurred
   * Changes the game new values by the rule.
   **/
  def selectEvent(i: ParameterHistory[TriggerEvent]) = {
    selectedEvent = TriggerEvent.getInputEvent(i)
    if(i != null) {
      val ruleToStopBefore = CodeGenerator.getRuleFromEvent(game, i.value) match { case Some(r) => r; case _ => null }
      game.returnToTime(i.timestamp, ruleToStopBefore)
      // Apply the rule that corresponds to the event.
      mTimeBar.setProgress((i.timestamp - minTimePosition).toInt)
      if(ruleToStopBefore != null) {
        triggerRule(ruleToStopBefore, i.value)
      }
    }
  }

  /**
   * Triggers the rule
   **/
  def triggerRule(e: ReactiveRule, i: TriggerEvent) = {
    i.code match {
      case TOUCHMOVE_EVENT =>
        game.updateContextMoveCoordinates(i.x1, i.y1, i.x2, i.y2)
      case INTEGER_CHANGE_EVENT | INTEGER_EQUAL_EVENT | INTEGER_GREATER_EQUAL_EVENT | INTEGER_LESS_EQUAL_EVENT | INTEGER_POSITIVE_EVENT | INTEGER_NEGATIVE_EVENT =>
        game.updateContextValueChanged(i.y2.toInt)
        i.shape1.asInstanceOf[IntegerBox].prev_value = i.x2.toInt
        i.shape1.asInstanceOf[IntegerBox].value = i.y2.toInt
      case _ =>
    }
    Expression.execute(e.code, game.context)
  }
}
