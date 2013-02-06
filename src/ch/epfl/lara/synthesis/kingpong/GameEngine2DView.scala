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
import scala.collection.mutable.HashMap
import android.view.WindowManager
import android.graphics.Color
import android.widget.Toast
import android.graphics.Typeface
import android.text.DynamicLayout
import android.text.Layout.Alignment
import android.content.res.Configuration

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
  var selectedCategory: Category = null
  
  var isForceFieldSelected: Boolean = false
  var isGravity2DSelected: Boolean = false
  var isCameraSelected: Boolean = false
  
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
    mRuleState = STATE_MODIFYING_GAME
    GameShapes.AccelerometerGravity.reset()
    GameShapes.Gravity2D.reset()
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
      mRuleState = STATE_MODIFYING_GAME
      //mAddRuleButton.text = res.getString(R.string.design_rule)
      mAddRuleButton.hovered = false
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
  
  /** Menu drawing */
  private val flatButton : Drawable = res.getDrawable(R.drawable.flat_button)
  private val flatButtonH : Drawable = res.getDrawable(R.drawable.flat_button_highlighted)
  private val flatButtonS : Drawable = res.getDrawable(R.drawable.flat_button_selected)
  private val flatButtonSH : Drawable = res.getDrawable(R.drawable.flat_button_selected_highlighted)
  private val flatButtonM1 : Drawable = res.getDrawable(R.drawable.flat_button_m1)
  private val flatButtonP1 : Drawable = res.getDrawable(R.drawable.flat_button_p1)
  private val overlayMove: Drawable = res.getDrawable(R.drawable.cross_move)
  private val overlaySpeed: Drawable = res.getDrawable(R.drawable.move_velocity)
  private val overlaySize : Drawable = res.getDrawable(R.drawable.move_size)
  private val overlayNail : Drawable = res.getDrawable(R.drawable.nail)
  private val overlayNailBig : Drawable = res.getDrawable(R.drawable.nail_big)
  private val overlayTrashcan : Drawable = res.getDrawable(R.drawable.trashcan)
  private val overlayEye : Drawable = res.getDrawable(R.drawable.eye)
  private val overlayPaint : Drawable = res.getDrawable(R.drawable.menu_paint)
  private val overlayNone: Drawable = res.getDrawable(R.drawable.none)
  private val flatButtonResizable: NinePatchDrawable = res.getDrawable(R.drawable.flat_button_resizable).asInstanceOf[NinePatchDrawable]
  private val flatButtonResizableH: NinePatchDrawable = res.getDrawable(R.drawable.flat_button_resizable_highlighted).asInstanceOf[NinePatchDrawable]
  private val overlayAddRectangle: Drawable = res.getDrawable(R.drawable.menu_add_rect)
  private val overlayAddCircle: Drawable = res.getDrawable(R.drawable.menu_add_circle)
  private val overlayAddDigit: Drawable = res.getDrawable(R.drawable.menu_add_digit)
  private val overlayAddText: Drawable = res.getDrawable(R.drawable.menu_add_text)
  private val overlayModifyText: Drawable = res.getDrawable(R.drawable.modify_text)
  private val overlayPlus: Drawable = res.getDrawable(R.drawable.plus)
  private val overlayForceField: Drawable = res.getDrawable(R.drawable.menu_add_accelerometer)
  private val overlayGravity2D: Drawable = res.getDrawable(R.drawable.menu_add_force_field)
  private val overlayAddRule: Drawable = res.getDrawable(R.drawable.menu_rule_editor)
  private val overlayCreateRule: Drawable = res.getDrawable(R.drawable.menu_rule_maker)
  private val overlayPrevEffects: Drawable = res.getDrawable(R.drawable.prev_effects)
  private val overlayNextEffects: Drawable = res.getDrawable(R.drawable.next_effects)
  private val overlayCamera: Drawable = res.getDrawable(R.drawable.menu_camera)
  
  /**
   * Menus and their effects
   */
  var mMenuPositionX: Float = 0
  var mMenuPositionY: Float = 0
  var mDisplacementX: Float = 0
  var mDisplacementY: Float = 0
  var modifyPreviousValues: Boolean = false
  /** The move button */
  var mMoveButton = new MenuButton(0, 0) { override def onFingerUp(selectedShape: Shape, x: Float, y: Float) = {
      if(selectedShape.noVelocity) {
        if(modifyPreviousValues) {
          if(Math.abs(selectedShape.prev_x - selectedShapeGameCoords(0)) >= 10) selectedShape.prev_x = Math.floor((selectedShape.prev_x+2.5f)/5).toFloat * 5
          if(Math.abs(selectedShape.prev_y - selectedShapeGameCoords(1)) >= 10) selectedShape.prev_y = Math.floor((selectedShape.prev_y+2.5f)/5).toFloat * 5   
        } else {
          if(Math.abs(selectedShape.x - selectedShapeGameCoords(0)) >= 10) selectedShape.x = Math.floor((selectedShape.x+2.5f)/5).toFloat * 5
          if(Math.abs(selectedShape.y - selectedShapeGameCoords(1)) >= 10) selectedShape.y = Math.floor((selectedShape.y+2.5f)/5).toFloat * 5          
        }
        if(mRuleState == STATE_MODIFYING_GAME) {
          selectedShape.prev_x = selectedShape.x
          selectedShape.prev_y = selectedShape.y
        }
      }
      hovered = false
    }
  }
  /** The speed button */
  var mSpeedButton = new MenuButton(0, 0) { override def onFingerUp(selectedShape: Shape, x: Float, y: Float) = {
    if(selectedShape.noVelocity) selectedShape.noVelocity = false
      hovered = false
    }
  }
  /** The size button */
  var mSizeButton = new MenuButton(0, 0) { override def onFingerUp(selectedShape: Shape, x: Float, y: Float) = {
      selectedShape match {
        case c:GameShapes.Circle =>
          if(modifyPreviousValues) {
            c.prev_radius = Math.floor((c.prev_radius + 2.5f)/5).toFloat * 5
          } else {
            c.radius = Math.floor((c.radius+2.5f)/5).toFloat * 5
          }
          if(mRuleState == STATE_MODIFYING_GAME) {
            c.prev_radius = c.radius
          }
        case r:GameShapes.Rectangular =>
          if(modifyPreviousValues) {
            r.prev_width = (Math.floor((r.prev_width + 2.5f)/5) * 5).toInt
            r.prev_height = (Math.floor((r.prev_height + 2.5f)/5) * 5).toInt
          } else {
            r.width = (Math.floor((r.width + 2.5f)/5) * 5).toInt
            r.height = (Math.floor((r.height + 2.5f)/5) * 5).toInt
          }
          if(mRuleState == STATE_MODIFYING_GAME) {
            r.prev_width = r.width
            r.prev_height = r.height
          }
        case _ =>
      }
      hovered = false
    }
  }
  /** The button to change between noVelocity and velocity available */
  var mPinButton = new MenuButton(0, 0) { override def onFingerUp(selectedShape: Shape, x: Float, y: Float) = {
      selectedShape.noVelocity = !selectedShape.noVelocity
      if(selectedShape.noVelocity) {
        if(modifyPreviousValues) {
          selectedShape.prev_velocity_x = 0
          selectedShape.prev_velocity_y = 0
        } else {
          selectedShape.velocity_x = 0
          selectedShape.velocity_y = 0
        }
        if(mRuleState == STATE_MODIFYING_GAME) {
          selectedShape.prev_velocity_x = selectedShape.velocity_x
          selectedShape.prev_velocity_y = selectedShape.velocity_y
        }
      }
      hovered = false
    }
  }
  /** The visibility button */
  var mVisibilityButton = new MenuButton(0, 0) { override def onFingerUp(selectedShape: Shape, x: Float, y: Float) = {
      if(modifyPreviousValues) {
        selectedShape.prev_visible = !selectedShape.prev_visible
      } else {
        selectedShape.visible = !selectedShape.visible
      }
      if(mRuleState == STATE_MODIFYING_GAME) {
        selectedShape.prev_visible = selectedShape.visible
      }
      hovered = false
    }
  }
  /** The send to trash button */
  var mTrashButton = new MenuButton(0, 0) { override def onFingerUp(selectedShape: Shape, x: Float, y: Float) = {
    CustomDialogs.launchOKCancelDialog(context,
        String.format(res.getString(R.string.delete_title), selectedShape.mName),
        res.getString(R.string.confirm_delete), false, { _ => game.deleteShape(selectedShape); selectShape(null)}, {_ => ()})
    hovered = false
  }}
  /** The button to change the color */
  var mPaintButton = new MenuButton(0, 0) { override def onFingerUp(selectedShape: Shape, x: Float, y: Float) = {
      val selectedColor:Int = ((mDisplacementX / (button_size/3)).toInt % 6 + 6) % 6
      var color = Color.parseColor(allColors(selectedColor))
      if(selectedColor == 0 && selectedShape.color == color) {
        color = Color.parseColor(allColors(1))
      }
      if(modifyPreviousValues) {
        selectedShape.prev_color = color
      } else {
        selectedShape.color = color
      }
      if(mRuleState == STATE_MODIFYING_GAME) {
        selectedShape.prev_color = color
      }
      hovered = false
    }
  }
  /** The button to increment the displayed integer */
  var mIncrementButton = new MenuButton(0, 0) { override def onFingerUp(selectedShape: Shape, x: Float, y: Float) = {
      selectedShape match {
        case d:IntegerBox =>
          if(modifyPreviousValues) {
            d.prev_value = d.prev_value + 1
          } else {
            d.value = d.value + 1
          }
          if(mRuleState == STATE_MODIFYING_GAME) {
            d.prev_value = d.value
          } else {
            if(integerBoxWatched != null && integerBoxWatched == d) {
              game.restorePrev(integerBoxWatched)
              integerEvent.x2 = d.prev_value
              integerEvent.y2 = d.value
              triggerRule(selectedRule, integerEvent)
            }
          }
        case _ =>
      }
      hovered = false
    }
  }
  /** The button to decrement the displayed integer */
  var mDecrementButton = new MenuButton(0, 0) {
    override def onFingerUp(selectedShape: Shape, x: Float, y: Float) = {
      selectedShape match {
        case d:IntegerBox =>
          if(modifyPreviousValues) {
            d.prev_value = d.prev_value - 1
          } else {
            d.value = d.value - 1
          }
          if(mRuleState == STATE_MODIFYING_GAME)  {
            d.prev_value = d.value
          } else {
            if(integerBoxWatched != null && integerBoxWatched == d) {
              game.restorePrev(integerBoxWatched)
              integerEvent.x2 = d.prev_value
              integerEvent.y2 = d.value
              triggerRule(selectedRule, integerEvent)
            }
          }
        case _ =>
      }
      hovered = false
    }
  }
  /** The button to modify the text */
  var mModifyTextButton = new MenuButton(0, 0) {
    override def onFingerUp(selectedShape: Shape, x: Float, y: Float) = {
      selectedShape match {
        case d:TextBox =>
          def updateText(s: String): Unit = {
            if(modifyPreviousValues) {
              d.prev_text = s
            } else {
              d.text = s
            }
            if(mRuleState == STATE_MODIFYING_GAME) {
              d.prev_text = d.text
            }
          }
          CustomDialogs.launchChoiceDialog(context,
              String.format(res.getString(R.string.modify_text_title), d.text), R.array.text_possibilities,
              updateText(_), {() => })
        case _ => 
      }
      hovered = false
    }
  }
  /** The button to modify the name */
  var mNameButton = new MenuTextButton(null) { override def onFingerUp(selectedShape: Shape, x: Float, y: Float) = {
      var array = selectedShape match {
        case r:Rectangle => R.array.rename_rectangles
        case r:TextBox => R.array.rename_textbox
        case r:IntegerBox => R.array.rename_integerbox
        case r:Circle => R.array.rename_circles
        case _ => R.array.rename_circles
      }
      CustomDialogs.launchChoiceDialog(context, String.format(res.getString(R.string.rename_title), selectedShape.mName), array, renameSelectedShape(_), {() => })
      hovered = false
    }
  }
  lazy val listMovingMenus = List(mNameButton, mIncrementButton, mDecrementButton, mModifyTextButton, mTrashButton, mPinButton, mVisibilityButton, mPaintButton, mMoveButton, mSpeedButton, mSizeButton)
  
  
  /** The button to choose an existing rule */
  var mChooseExistingRuleButton = new MenuTextButton(res.getString(R.string.existing_rule))

  /** The buttons to create custom shapes */
  lazy val mAddRectangleButton = new MenuButton(button_size*0.5f, button_size*0.5f)
  lazy val mAddCircleButton = new MenuButton(button_size*1.5f, button_size*0.5f)
  lazy val mAddDigitButton = new MenuButton(button_size*2.5f, button_size*0.5f)
  lazy val mAddTextButton = new MenuButton(button_size*3.5f, button_size*0.5f)
  /** The button to access the rule editor */
  lazy val mAddForceFieldButton = new MenuButton(button_size*0.5f, button_size*1.5f)
  lazy val mAddGravity2DButton = new MenuButton(button_size*1.5f, button_size*1.5f)
  lazy val mCameraButton = new MenuButton(button_size*2.5f, button_size*1.5f)
  lazy val mAddRuleButton = new MenuButton(button_size*0.5f, button_size*2.5f)
  lazy val mSelectPrevNextButton = new MenuButton(button_size*1.5f, button_size*2.5f)
  
  lazy val listStaticMenus = List(mAddRectangleButton, mAddCircleButton, mAddDigitButton, mAddTextButton, mAddForceFieldButton, mAddGravity2DButton, mCameraButton, mAddRuleButton, mChooseExistingRuleButton, mSelectPrevNextButton)
  
  /** The possible states of the game */
  final val STATE_MODIFYING_GAME = 0 // All effects are applied immediately
  final val STATE_SELECTING_EVENTS = 1 // Only events can be selected 
  final val STATE_SELECTING_EFFECTS = 2 // Effects are applied only on the future state, the previous state remains the same.
  final val STATE_MODIFYING_CATEGORY = 3 // Objects are selected to be included to a given category
  var mRuleState = STATE_MODIFYING_GAME
  
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
  
  /** Draws the game on the given canvas */
  def drawGameOn(canvas: Canvas): Unit = {
    if(game == null) return
    /*if(editMode) { // TODO : Soon to be replaced by the camera size.
      canvas.drawLine(0, 0, game.screenWidth, 0, distancePaint)
      canvas.drawLine(game.screenWidth, 0, game.screenWidth, game.screenHeight, distancePaint)
      canvas.drawLine(game.screenWidth, game.screenHeight, 0, game.screenHeight, distancePaint)
      canvas.drawLine(0, game.screenHeight, 0, 0, distancePaint)
    }*/
    val shapes = game.getArena
    shapes foreach { s =>
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
    
    if(isCameraSelected) {
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
      /*rectBitmapVert.setAlpha(180)
      rectBitmapVert.setColorFilter(getFilter(0xFFFFFFFF))
      circBitmap.setAlpha(180)
      rectBitmapVert.setColorFilter(getFilter(0xFFFFFFFF))*/
      displayButtonCenteredOn(canvas, mAddRectangleButton, mAddRectangleButton.x, mAddRectangleButton.y, if(mAddRectangleButton.hovered) flatButtonH else flatButton, overlayAddRectangle)
      displayButtonCenteredOn(canvas, mAddCircleButton, mAddCircleButton.x, mAddCircleButton.y, if(mAddCircleButton.hovered) flatButtonH else flatButton, overlayAddCircle)
      displayButtonCenteredOn(canvas, mAddDigitButton, mAddDigitButton.x, mAddDigitButton.y, if(mAddDigitButton.hovered) flatButtonH else flatButton, overlayAddDigit)
      displayButtonCenteredOn(canvas, mAddTextButton, mAddTextButton.x, mAddTextButton.y, if(mAddTextButton.hovered) flatButtonH else flatButton, overlayAddText)
      displayButtonCenteredOn(canvas, mAddForceFieldButton, mAddForceFieldButton.x, mAddForceFieldButton.y, if(mAddForceFieldButton.hovered) (if(isForceFieldSelected) flatButtonSH else flatButtonH) else (if(isForceFieldSelected) flatButtonS else flatButton), overlayForceField)
      displayButtonCenteredOn(canvas, mAddGravity2DButton, mAddGravity2DButton.x, mAddGravity2DButton.y, if(mAddGravity2DButton.hovered) (if(isGravity2DSelected) flatButtonSH else flatButtonH) else (if(isGravity2DSelected) flatButtonS else flatButton), overlayGravity2D)
      displayButtonCenteredOn(canvas, mCameraButton, mCameraButton.x, mCameraButton.y, if(mCameraButton.hovered) (if(isCameraSelected) flatButtonSH else flatButtonH) else (if(isCameraSelected) flatButtonS else flatButton), overlayCamera)

      /*rectBitmapVert.setAlpha(255)
      circBitmap.setAlpha(255)*/
      
      //mAddRuleButton.setTextAndTopLeft(null, whitePaint, 33*button_size/49, 0, button_size)
      displayButtonCenteredOn(canvas, mAddRuleButton, mAddRuleButton.x, mAddRuleButton.y,
        if(mRuleState == STATE_SELECTING_EVENTS || mRuleState == STATE_SELECTING_EFFECTS) (if(mAddRuleButton.hovered) flatButtonSH else flatButtonS) else (if(mAddRuleButton.hovered) flatButtonH else flatButton),
        if(mRuleState == STATE_SELECTING_EFFECTS) overlayCreateRule else overlayAddRule 
      )
      if(mRuleState == STATE_SELECTING_EFFECTS) {
        mSelectPrevNextButton.visible = true
        displayButtonCenteredOn(canvas, mSelectPrevNextButton, mSelectPrevNextButton.x, mSelectPrevNextButton.y,
          (if(mAddRuleButton.hovered) flatButtonH else flatButton),
          if(modifyPreviousValues) overlayPrevEffects else overlayNextEffects 
        )
      } else {
        mSelectPrevNextButton.visible = false
      }
      
      //displayResizableButtonOn(canvas, mAddRuleButton.mRectDataButton, if(mRuleState == STATE_SELECTING_EVENTS || mRuleState == STATE_SELECTING_EFFECTS || mAddRuleButton.hovered) flatButtonResizableH else flatButtonResizable)
      //canvas.drawText(mAddRuleButton.text, mAddRuleButton.mTextX,  mAddRuleButton.mTextY, whitePaint)
      mChooseExistingRuleButton.setTextAndTopLeft(null, whitePaint, 33*button_size/49, 0, button_size*3)
      mChooseExistingRuleButton.visible = mRuleState == STATE_SELECTING_EVENTS
      if(mChooseExistingRuleButton.visible) {
        displayResizableButtonOn(canvas, mChooseExistingRuleButton.mRectDataButton, if(mChooseExistingRuleButton.hovered) flatButtonResizableH else flatButtonResizable)
        canvas.drawText(mChooseExistingRuleButton.text, mChooseExistingRuleButton.mTextX,  mChooseExistingRuleButton.mTextY, whitePaint)
      }
      
      /*if(selectedRuleString != null) {
        var x = 0
        var ystart = (3*button_size + 30)
        var y = ystart
        val ascent = mCodePaint.ascent()
        val descent = mCodePaint.descent()
        var height = descent - ascent
        var textSize = button_size/3
        mCodePaint.setTextSize(textSize)
        for(line <- selectedRuleStringSplit) {
          canvas.drawText(line, x, y, mCodePaint)
          y += height
        }
        selectedRuleConstants foreach {
          case (line, col, size, expr) =>
            val y1 = ystart + (line - 1)*height + mCodePaint.ascent()
            val y2 = y1 + height
            val x1 = x + col*textSize *0.605f
            val x2 = x + (col + size)*textSize *0.605f
            canvas.drawRect(x1, y1, x2, y2, mCodeConstantPaint)
        }
      }*/
    }
    if(selectedShape != null) {
      // TODO : Synchronize with selectedShape because it can become null ?
      if(modifyPreviousValues) {
        coords(0) = selectedShape.prevCenterX
        coords(1) = selectedShape.prevCenterY
      } else {
        coords(0) = selectedShape.centerX
        coords(1) = selectedShape.centerY
      }
      mMatrix.mapPoints(coords)
      val selectionx = Math.min(Math.max(coords(0), button_size), mWidth - button_size*2.5f)
      val selectiony = Math.min(Math.max(coords(1), button_size*1.5f), mHeight-button_size*1.5f)
      
      val cx = selectionx + mMenuPositionX
      val cy = selectiony + mMenuPositionY
      // Display the shape's menu.
      displayButtonCenteredOn(canvas, mMoveButton, cx, cy, overlayMove)
      displayButtonCenteredOn(canvas, mSizeButton, cx + button_size, cy + button_size, if(mSizeButton.hovered) flatButtonH else flatButton, overlaySize)
      displayButtonCenteredOn(canvas, mTrashButton, cx + button_size*2, cy + button_size, if(mTrashButton.hovered) flatButtonH else flatButton, overlayTrashcan)
      displayButtonCenteredOn(canvas, mPaintButton, cx + button_size*3, cy + button_size, if(mPaintButton.hovered) flatButtonH else flatButton, overlayPaint)
      
      val top_shift = selectedShape match {
        case d: IntegerBox =>
          displayButtonCenteredOn(canvas, mIncrementButton, cx, cy - button_size, if(mIncrementButton.hovered) flatButtonH else flatButton, flatButtonP1)
          displayButtonCenteredOn(canvas, mDecrementButton, cx, cy + button_size, if(mDecrementButton.hovered) flatButtonH else flatButton, flatButtonM1)
          button_size
        case d: TextBox =>
          displayButtonCenteredOn(canvas, mModifyTextButton, cx, cy - button_size, if(mModifyTextButton.hovered) flatButtonH else flatButton, overlayModifyText)
          button_size
        case _ =>
          0
      }
      displayButtonCenteredOn(canvas, mSpeedButton, cx + button_size, cy - top_shift, if(mSpeedButton.hovered) flatButtonH else flatButton, overlaySpeed, if(selectedShape.noVelocity) overlayNone else null)
      displayButtonCenteredOn(canvas, mPinButton, cx + button_size*2, cy - top_shift, if(mPinButton.hovered) flatButtonH else flatButton, overlayNail, if(!selectedShape.noVelocity) overlayNone else null)
      displayButtonCenteredOn(canvas, mVisibilityButton, cx + button_size*3, cy-top_shift, if(mVisibilityButton.hovered) flatButtonH else flatButton, overlayEye, if(!selectedShape.visible) overlayNone else null)
      if(selectedShape.mName != null) {
        mNameButton.setTextAndCenter(selectedShape.mName, whitePaint, 33*button_size/49, cx, cy - top_shift - button_size / 2)
        displayResizableButtonOn(canvas, mNameButton.mRectDataButton, if(mNameButton.hovered) flatButtonResizableH else flatButtonResizable)
        canvas.drawText(selectedShape.mName, mNameButton.mTextX,  mNameButton.mTextY, whitePaint)
      }
    }
  }
  
  /** Displays a list of drawable representing buttons at the given point */
  def displayButtonCenteredOn(canvas: Canvas, button: MenuButton, x: Float, y: Float, list_drawable: Drawable*) = {
    button.x = x
    button.y = y
    list_drawable foreach {
      d => if(d!= null) {
        rectFData.set(x - button_size/2, y - button_size/2, x + button_size/2, y + button_size/2)
        rectFData.round(rectData)
        d.setBounds(rectData)
        d.draw(canvas)
      }
    }
  }
  
  /** Displays a resizable drawable */
  def displayResizableButtonOn(canvas: Canvas, rect: Rect, d: Drawable) = {
    d.setBounds(rect)
    d.draw(canvas)
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
      if(modifyPreviousValues) {
        selectedShapeGameCoords(0) = selectedShape.prev_x
        selectedShapeGameCoords(1) = selectedShape.prev_y
      } else {
        selectedShapeGameCoords(0) = selectedShape.x
        selectedShapeGameCoords(1) = selectedShape.y
      }
    }
    if(mPaintButton.hovered) {
      mDisplacementX = 0
      mDisplacementY = 0
    }
    if(mMoveButton.hovered) {
      mDisplacementX = 0
      mDisplacementY = 0
    }
    return_value
  }
  
  /**
   * Called when the finger is released at these coordinates
   * Handles menu interaction
   **/
  def onFingerUp(x: Float, y: Float) = {
    //Log.d("GameEngine2DView", "Menu Selected at " + x + ", " + y)
    var menuSelected:Boolean = false
    if(selectedShape != null) {
      menuSelected = listMovingMenus.foldLeft(false) { // Priority to the flying menus first
        (hovered, menu) => if(!hovered) {
          if(menu.hovered) {
            menu.onFingerUp(selectedShape, x, y)
            true
          } else false           
        } else true
      }
    }
    if(!menuSelected) { // Test is top left menu activated
      mMenuPositionX = 0
      mMenuPositionY = 0
      // If a shape is created
      if(mAddRectangleButton.hovered || mAddDigitButton.hovered || mAddCircleButton.hovered || mAddTextButton.hovered) {
        var r: Shape = null
        var sameTypeAsSelected = false
        selectedShape match {
          case model:Rectangle if mAddRectangleButton.hovered => 
            r = game.Rectangle(game.screenWidth / 2, game.screenHeight / 2, model.width, model.height)
            sameTypeAsSelected = true
          case model:IntegerBox if mAddDigitButton.hovered => 
            r = game.IntegerBox(game.screenWidth / 2, game.screenHeight / 2, model.width.toInt, model.height.toInt, 0)
            sameTypeAsSelected = true
          case model:Circle if mAddCircleButton.hovered => 
            r = game.Circle(game.screenWidth / 2, game.screenHeight / 2, model.radius)
            sameTypeAsSelected = true
          case model:TextBox if mAddTextButton.hovered =>
            r = game.TextBox(game.screenWidth / 2, game.screenHeight / 2, model.width, model.height, model.text)
            sameTypeAsSelected = true
          /*case model:ForceField if mForceFieldButton.hovered =>
            r = game.ForceField(game.screenWidth / 2, game.screenHeight / 2, model.width, model.height, model.text)
            sameTypeAsSelected = true*/
          case _ =>
            if(mAddRectangleButton.hovered) {
              r = game.Rectangle(game.screenWidth / 2, game.screenHeight / 2, button_size.toInt, button_size.toInt)
            } else if(mAddDigitButton.hovered) {
              r = game.IntegerBox(game.screenWidth / 2, game.screenHeight / 2, button_size.toInt/2, button_size.toInt/2, 0)
            } else if(mAddCircleButton.hovered) {
              r = game.Circle(game.screenWidth / 2, game.screenHeight / 2, 50)
            } else /**if(mAddTextButton.hovered) */{
              r = game.TextBox(game.screenWidth / 2, game.screenHeight / 2, button_size.toInt*2, button_size.toInt/2, "Custom text")
            }/* else {
              r = game.ForceField(game.screenWidth / 2, game.screenHeight / 2)
            }*/
        }
        if(selectedShape != null) {
          r.velocity_x = selectedShape.velocity_x
          r.velocity_y = selectedShape.velocity_y
          r.noVelocity = selectedShape.noVelocity
          r.color = selectedShape.color
        }
        r.storePrevValues()
        game.getArena += r
        
        val oldSelectedShape = selectedShape
        val oldName = if(selectedShape != null) selectedShape.mName else null
        selectShape(r)
        renameSelectedShape(
            if(oldName != null && sameTypeAsSelected) oldSelectedShape.mName
            else if(mAddRectangleButton.hovered) "wall"
            else if(mAddDigitButton.hovered) "score"
            else if(mAddTextButton.hovered) "textBox"
            else if(mAddCircleButton.hovered) "ball"
            else GameShapes.DEFAULT_NAME)
        if(oldName != null && sameTypeAsSelected) CodeGenerator.duplicateRuleContaining(game, oldSelectedShape, r)
        val ident = EIdentShape(r)
        game.context(r.mName) = ident
        

        mAddRectangleButton.hovered = false
        mAddDigitButton.hovered = false
        mAddTextButton.hovered = false
        mAddCircleButton.hovered = false
        
        menuSelected = true
      } else if(mAddRuleButton.hovered) { // The user selected the "Rule editor button"
        mRuleState match {
          case STATE_MODIFYING_GAME => 
            mRuleState = STATE_SELECTING_EVENTS
            modifyPreviousValues = false
            mAddRuleButton.hovered = true
            menuSelected = true
            selectEvent(null)
            selectShape(null)
            //mAddRuleButton.text = res.getString(R.string.select_event)
            Toast.makeText(context, res.getString(R.string.select_event_toast), 2000).show()
          case STATE_SELECTING_EVENTS =>
            Toast.makeText(context, res.getString(R.string.rule_canceled), 2000).show()
            mRuleState = STATE_MODIFYING_GAME
            //mAddRuleButton.text = res.getString(R.string.design_rule)
            mAddRuleButton.hovered = false
            selectEvent(null)
          case STATE_SELECTING_EFFECTS =>
            if(selectedEvent != null) { // Means that the rule has been confirmed.
              CodeGenerator.createRule(context, game, selectedEvent.value, selectedEvent.timestamp, {
                rule =>
                  // Here if the rule changes some number, add the corresponding effects.
                  game.storeState(game.currentTime, true, null)
                  selectRule(rule)
              })
            }
            modifyingGameStateMode()
        }
        true
      } else if(mChooseExistingRuleButton.hovered) {
        // Display the list of existing rules
        val many_rules = game.init_rules.toList
        CustomDialogs.launchRuleChooserDialog(context, res.getString(R.string existing_rule_dialog_title),
            many_rules map (_.toScalaString("", game.context)),
            many_rules,
            { rule => 
              // Let the code apply to the game, and the user edit the output. 
              CodeGenerator.modifyAndInsertRule(context, game, rule, game.currentTime, {rule =>
                //Replay the rule and select its effects.
                // Need to select an event corresponding to the rule.
                val event = findEventForRule(rule)
                if(event != null) {
                  selectEffectsMode()
                  game.storePrevValues()
                  selectRule(rule, event)
                  triggerRule(rule, event)
                } else {
                  modifyingGameStateMode()
                }
              })
            },
            { () => })
      } else if(mAddForceFieldButton.hovered) {
        isForceFieldSelected = !isForceFieldSelected
        if(isForceFieldSelected) {
          selectedCategory = GameShapes.AccelerometerGravity
          mRuleState = STATE_MODIFYING_CATEGORY
        } else {
          selectedCategory = null
          mRuleState = STATE_MODIFYING_GAME
        }
        mAddForceFieldButton.hovered = false
      } else if(mAddGravity2DButton.hovered) {
        isGravity2DSelected = !isGravity2DSelected
        if(isGravity2DSelected) {
          selectedCategory = GameShapes.Gravity2D
          mRuleState = STATE_MODIFYING_CATEGORY
        } else {
          selectedCategory = null
          mRuleState = STATE_MODIFYING_GAME
        }
        mAddGravity2DButton.hovered = false
      } else if(mSelectPrevNextButton.hovered) {
        modifyPreviousValues = !modifyPreviousValues
        mSelectPrevNextButton.hovered = false
      } else if(mCameraButton.hovered) {
        mCameraButton.hovered = false
        menuSelected = true
        isCameraSelected = !isCameraSelected
        if(isCameraSelected) {
          selectShape(game.Camera)
        } else {
          selectShape(null)
        }
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

    if(!menuSelected) {
      // If the previous attemps to select a menu failed,
      // We dissmiss all selection and we select something else depending on the game editor type.
      listMovingMenus foreach {
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
                       selectEffectsMode()
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
                       selectEffectsMode()},
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
                      selectEffectsMode()
                    } else if(i == 1) {
                      game.insertRule(NoCollisionBetweenRule(EIdentShape(d1), EIdentShape(d2)), game.currentTime)
                      modifyingGameStateMode()
                    } else {
                      game.insertRule(NoCollisionEffectBetweenRule(EIdentShape(d1), EIdentShape(d2)), game.currentTime)
                      selectEffectsMode()
                    }
                  },
                  {() =>}
                )
          } else {
            selectEffectsMode()
          }
        }
      } else if(mRuleState == STATE_MODIFYING_CATEGORY) {
        modifySelection(touchCoords(0), touchCoords(1))
      } else {
        chooseSelectedShape(touchCoords(0), touchCoords(1))
      }
    }
  }
  
  /** Switches the current mode to selecting effects */
  def selectEffectsMode(): Unit = {
    mRuleState = STATE_SELECTING_EFFECTS
    Toast.makeText(context, res.getString(R.string.select_effects_toast), 2000).show()
  }
  
  /** Switches the current mode to the global modification of the game */
  def modifyingGameStateMode() {
    mRuleState = STATE_MODIFYING_GAME
    enterEditMode()
    mAddRuleButton.hovered = false
    selectEvent(null)
  }

  /** Select the shape closest to the coordinates */
  def chooseSelectedShape(xTouch: Float, yTouch: Float) = {
    var minDistance = -1.0f
    selectShape(null)
    
    game.getArena foreach { shape =>
      val x = xTouch + (if(modifyPreviousValues) shape.x - shape.prev_x else 0)
      val y = yTouch + (if(modifyPreviousValues) shape.y - shape.prev_y else 0)// - shape.y + shape.prev_y
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
    }
  }
  
  /** Changes the selection of the category. */
  def modifySelection(xTouch: Float, yTouch: Float) = {
    if(selectedCategory != null)  {
      game.getArena foreach { shape =>
        val x = xTouch - shape.x + shape.prev_x
        val y = yTouch - shape.y + shape.prev_y
        if(shape.selectableBy(x, y)) {
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
    if(mMoveButton.hovered) {
      mMenuPositionX = 0
      mMenuPositionY = 0
      if(selectedShape != null) {
        if(modifyPreviousValues) {
          selectedShape.prev_x = selectedShapeGameCoords(0) + relativeX
          selectedShape.prev_y = selectedShapeGameCoords(1) + relativeY
        } else {
          selectedShape.x = selectedShapeGameCoords(0) + relativeX
          selectedShape.y = selectedShapeGameCoords(1) + relativeY
        }
        if(mRuleState == STATE_MODIFYING_GAME) {
          selectedShape.prev_x = selectedShape.x
          selectedShape.prev_y = selectedShape.y
        }
      }
    }
    if(mSpeedButton.hovered) {
      if(selectedShape != null && !selectedShape.noVelocity) {
        if(modifyPreviousValues) {
          selectedShape.prev_velocity_x += shiftX.toFloat / 1000f
          selectedShape.prev_velocity_y += shiftY.toFloat / 1000f
        } else {
          selectedShape.velocity_x += shiftX.toFloat / 1000f
          selectedShape.velocity_y += shiftY.toFloat / 1000f
        }
        if(mRuleState == STATE_MODIFYING_GAME) {
          selectedShape.prev_velocity_x = selectedShape.velocity_x
          selectedShape.prev_velocity_y = selectedShape.velocity_y
        }
      }
    }
    if(mSizeButton.hovered) {
      if(selectedShape != null) {
        selectedShape match {
          case c:GameShapes.Circle =>
            if(modifyPreviousValues) {
              c.prev_radius = Math.max(10, c.prev_radius + shiftX)
            } else {
              c.radius = Math.max(10, c.radius + shiftX)
            }
            if(mRuleState == STATE_MODIFYING_GAME) {
              c.prev_radius = c.radius
            }
          case r:GameShapes.Rectangular =>
            if(modifyPreviousValues) {
              r.prev_width = Math.max(10, r.prev_width + shiftX.toInt)
              r.prev_height = Math.max(10, r.prev_height + shiftY.toInt)
            } else {
              r.width = Math.max(10, r.width + shiftX.toInt)
              r.height = Math.max(10, r.height + shiftY.toInt)
            }
            if(mRuleState == STATE_MODIFYING_GAME) {
              r.prev_width = r.width
              r.prev_height = r.height
            }
          case _ =>
        }
      }
    }
    if(mPaintButton.hovered) {
      if(selectedShape != null) {
        val selectedColor:Int = ((mDisplacementX / (button_size/3)).toInt % 6 + 6) % 6
        if(modifyPreviousValues) {
          selectedShape.prev_color = Color.parseColor(allColors(selectedColor))
        } else {
          selectedShape.color = Color.parseColor(allColors(selectedColor))
        }
        if(mRuleState == STATE_MODIFYING_GAME) {
          selectedShape.prev_color = selectedShape.color
        }
      }
    }
    if(!mMoveButton.hovered && !mSpeedButton.hovered && !mSizeButton.hovered && !mPaintButton.hovered) {
      highLightMenu(xTo, yTo)
    }
  }
  
  /** Highlights the corresponding menu under the finger*/
  def highLightMenu(x: Float, y: Float): Boolean = {
    if(selectedShape != null) {
      listMovingMenus foreach (_.testHovering(x, y, button_size))
    }
    val menu_not_hovered = listMovingMenus.foldLeft(true) { (notHovered, menu ) => notHovered && !menu.hovered }
    if(menu_not_hovered) {
      listStaticMenus foreach (_.testHovering(x, y, button_size))
    }
    var something_highlighted = false
    listMovingMenus foreach { menu => something_highlighted = something_highlighted || menu.hovered }
    listStaticMenus foreach { menu => something_highlighted = something_highlighted || menu.hovered }
    something_highlighted
  }

  /** Renames the selected shape and tries to find an unexisting name*
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
      /*touchCoordsDownOriginal(0) = selectedShape.prev_x
      touchCoordsDownOriginal(1) = selectedShape.prev_y
      mMatrix.mapPoints(touchCoordsDownOriginal)
      touchCoords(0) = selectedShape.x
      touchCoords(1) = selectedShape.y
      mMatrix.mapPoints(touchCoords)*/

      mMenuPositionX = 0 //touchCoords(0) - touchCoordsDownOriginal(0)
      mMenuPositionY = 0 //touchCoords(1) - touchCoordsDownOriginal(1)
    }
  }

  /**
   * Selects the given event by returning to the time this event occured
   * Change the game new values by the rule.
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
        game.updateContextValueChanged(i.x2.toInt, i.y2.toInt)
        i.shape1.asInstanceOf[IntegerBox].prev_value = i.x2.toInt
        i.shape1.asInstanceOf[IntegerBox].value = i.y2.toInt
      case _ =>
    }
    Expression.execute(e.code, game.context)
  }
}
