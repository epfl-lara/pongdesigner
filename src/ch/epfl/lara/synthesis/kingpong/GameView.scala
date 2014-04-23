package ch.epfl.lara.synthesis.kingpong

import scala.util.Try
import scala.collection.mutable.ConcurrentMap
import android.app.Activity
import android.view.SurfaceView
import android.view.MotionEvent
import android.view.SurfaceHolder
import android.view.Surface
import android.view.View
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
import android.graphics.Path
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
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import android.text.style.StyleSpan
import android.graphics.Typeface
import android.text.style.ForegroundColorSpan
import android.widget.ExpandableListView
import android.widget.ExpandableListAdapter
import ch.epfl.lara.synthesis.kingpong.menus._
import ch.epfl.lara.synthesis.kingpong.common.History
import android.widget.Toast
import collection.mutable.{Set => MSet, HashMap => MMap}
import android.graphics.DashPathEffect
import android.content.res.Resources
import android.util.DisplayMetrics
import android.view.WindowManager
import android.graphics.LinearGradient
import android.graphics.Shader
import ch.epfl.lara.synthesis.kingpong.common.Messages._
import net.londatiga.android.QuickAction
import net.londatiga.android.ActionItem
import scala.collection.mutable.ListBuffer
import scala.package$._

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

/**
 * Handler for the time bar to change the time displayed on it.
 */
trait ProgressBarHandler extends SeekBar.OnSeekBarChangeListener  {
  protected var progressBar: SeekBar = null
  def setProgressBar(progressBar: SeekBar): Unit = {
    this.progressBar = progressBar
    if(progressBar != null) {
      progressBar.setMax(common.History.MAX_HISTORY_SIZE)
      progressBar.setProgress(0)
      progressBar.setSecondaryProgress(0)
      progressBar.setOnSeekBarChangeListener(this)
    }
  }
  def setProgressTime(t: Int) = {
    progressBar.setProgress(t)
    progressBar.setSecondaryProgress(t)
  }
  
  def onStartTrackingTouch(seekBar: SeekBar):Unit = {
  }
  def onStopTrackingTouch(seekBar: SeekBar):Unit = {
  }
  def setTimeProgressAbsolute(i: Int) = {
    progressBar.setProgress(Math.max(0, (i - History.MAX_HISTORY_SIZE)))
  }
  def getGame: Game
}

/**
 * Handler for the action bar to add shapes to the world.
 */
trait ActionBarHandler extends common.ContextUtils {
  private var actionBar: ExpandableListView = _
  private var actionBarAdapter: adapters.ActionsAdapter = _
  def menuCallBacks: String => Boolean
  
  var submenusLabels: IndexedSeq[IndexedSeq[String]] = _
  def setActionBar(actionBar: ExpandableListView): Unit = {
    this.actionBar = actionBar
    if(actionBar != null) {
      
      val menuLabels: IndexedSeq[String] = getStringArray(R.array.menu_arrays_hint)
      val menuDrawables = getArray(R.array.menu_arrays_drawable) map getDrawable
      val submenusDrawables = getArray(R.array.menu_arrays) map getDrawableArray
      submenusLabels = getArray(R.array.menu_arrays_strings) map getStringArray map {u: Array[String] => u:IndexedSeq[String]}
      
      val actions: IndexedSeq[String] = menuLabels
      val actionsCollection: Map[String, IndexedSeq[String]] = (menuLabels zip submenusLabels).toMap
      val bitmaps: MMap[String, Drawable] = MMap()
      bitmaps ++= ((menuLabels ++ submenusLabels.flatten) zip (menuDrawables ++ submenusDrawables.flatten))
      val callbacks: String => Boolean = menuCallBacks
      actionBarAdapter = new adapters.ActionsAdapter(context, actions, actionsCollection, bitmaps, callbacks)
      actionBar.setAdapter(actionBarAdapter)
      
      actionBar.setOnGroupClickListener(new ExpandableListView.OnGroupClickListener() {
          def onGroupClick(parent: ExpandableListView, v: View, groupPosition: Int, id: Long) = {
            menuCallBacks(actions(groupPosition))
          }
      });
      
    }
  }
  
  def changeMenuIcon(position: String, drawable: Drawable) = {
    actionBarAdapter.bitmaps(position) = drawable
    actionBarAdapter.notifyDataSetChanged()
  }
}

/**
 * The game view containing menus for editing shapes and the game itself.
 */
class GameView(val context: Context, attrs: AttributeSet)
  extends SurfaceView(context, attrs) 
  with SurfaceHolder.Callback
  with ProgressBarHandler
  with ActionBarHandler
  with common.ContextUtils
  {
  import GameView._
  import expression.Types._
  import common.Implicits._

  private var activity: Activity = null
  private var codeview: EditTextCursorWatcher = null
  
  private var gameViewRender = new GameViewRender(context)
  def grid = gameViewRender.grid
  
  def render(canvas: Canvas) = {
    gameViewRender.render(canvas, this, matrix, matrixI, game, obj_to_highlight, bitmaps, state, mRuleState == STATE_SELECTING_EVENTS, eventEditor, shapeEditor)
  }
  
  lazy val button_size = gameViewRender.button_size
  //implicit val self: Game = this
  
  def menuCallBacks: String => Boolean = { s =>
    s match {
      case Str(R.string.add_rectangle_hint) =>
        game.rectangle(DefaultCategory("rectangle", game))(name="rectangle", x=0, y=0, width=2*grid.step, height=grid.step)
        true
      case Str(R.string.add_circle_hint) =>
        game.circle(DefaultCategory("circle", game))(name="circle", x=0, y=0, radius=grid.step)
        true
      case Str(R.string.add_drawing_object_hint) =>
        val d = game.drawingObject(DefaultCategory("drawingobjects", game))(name="drawingZone", x=0, y=0, width=4*grid.step, height=4*grid.step)
        game.addRule(d.defaultRule(game))
        true
      
      case Str(R.string.menu_add_constraint_hint) =>
        var menuSelected = false
        val res = context.getResources()
        val game = getGame()
        mRuleState match {
          case STATE_MODIFYING_GAME => 
            eventEditor.unselect()
            setModeSelectEvents()
            menuSelected = true
            //mAddRuleButton.text = res.getString(R.string.select_event)
            Toast.makeText(context, res.getString(R.string.select_event_toast), 2000).show()
          case STATE_SELECTING_EVENTS =>
            if(eventEditor.selectedEventTime.nonEmpty || eventEditor.selectedObjects.nonEmpty) {
              Toast.makeText(context, res.getString(R.string.select_effects_toast), 2000).show()
              setModeSelectEffects()
            } else {
              Toast.makeText(context, res.getString(R.string.rule_canceled), 2000).show()
              setModeModifyGame()
            }
          case STATE_SELECTING_EFFECTS =>
            CodeGenerator.createRule(context, getGame(), eventEditor.selectedEventTime, eventEditor.selectedObjects)
            setModeModifyGame()
            true
        case STATE_MODIFYING_CATEGORY =>
          //hovered = false
      }
      menuSelected
      case Str(R.string.menu_fix_hint) => 
        // Allows to select objects to which a rule was applied recently.
        false
      case _ =>
        false
    }
  }

  def snapX(i: Float): Float = grid.snap(i)
  def snapY(i: Float): Float = grid.snap(i)

  def snapX(i: Float, other: Float*): Float = {
    val res = grid.snap(i) - i
    val minDiff = (res /: other) { case (sn, o) => val n = grid.snap(o) - o; if(Math.abs(o) < Math.abs(sn)) o else sn }
    i + minDiff
  }
  def snapY(i: Float, other: Float*): Float = {
    val res = grid.snap(i) - i
    val minDiff = (res /: other) { case (sn, o) => val n = grid.snap(o) - o; if(Math.abs(o) < Math.abs(sn)) o else sn }
    i + minDiff
  }
  
  var gameEngineEditors: List[GameEngineEditor] = _
  def addGameEngineEditor(g: GameEngineEditor) = {
    if(gameEngineEditors == null) {
      gameEngineEditors = List(g)
    } else {
      gameEngineEditors = gameEngineEditors ++ List(g)
    }
  }
  /** Selected objects, events and rules */
  var shapeEditor = new ShapeEditor(this)
  var eventEditor = new EventEditor(this)
  /*var categoryEditor = new CategoryEditor(this)
  var ruleEditor = new RuleEditor(this)
  var numberEditor = new NumberEditor(this)
  var systemEditor = new SystemEditor(this)
  var gameEditor = new GameEditor(this)*/
  
  final val STATE_MODIFYING_GAME = 0 // All effects are applied immediately
  final val STATE_SELECTING_EVENTS = 1 // Only events can be selected 
  final val STATE_SELECTING_EFFECTS = 2 // Effects are applied only on the future state, the previous state remains the same.
  final val STATE_MODIFYING_CATEGORY = 3 // Objects are selected to be included to a given category
  var mRuleState = STATE_MODIFYING_GAME
  def setModeSelectCategory() = {
    mRuleState = STATE_MODIFYING_CATEGORY
    MenuOptions.copy_to_prev = true
    MenuOptions.modify_prev = false
  }
  
  /** Switches the current mode to selecting effects */
  def setModeSelectEffects(): Unit = {
    mRuleState = STATE_SELECTING_EFFECTS
    MenuOptions.copy_to_prev = false
    MenuOptions.modify_prev = false
    game.setInstantProperties(false)
    Toast.makeText(context, Str(R.string.select_effects_toast), 2000).show()
  }
  
  /** Switches the current mode to selecting events. */
  def setModeSelectEvents() = {
    mRuleState = STATE_SELECTING_EVENTS
    gameEngineEditors foreach (_.unselect())
    MenuOptions.modify_prev = false // irrelevant
    MenuOptions.copy_to_prev = true // irrelevant
    game.setInstantProperties(true)
    changeMenuIcon(Str(R.string.menu_add_constraint_hint), bitmaps(R.drawable.menu_rule_maker))
  }
  
  /** Switches the current mode to the global modification of the game */
  def setModeModifyGame(resetView: Boolean = true) {
    mRuleState = STATE_MODIFYING_GAME
    MenuOptions.modify_prev = false
    MenuOptions.copy_to_prev = true
    game.setInstantProperties(true)
    game.restore(game.time)
    changeMenuIcon(Str(R.string.menu_add_constraint_hint), bitmaps(R.drawable.menu_rule_editor))
    eventEditor.unselect()
  }

  var whitePaint = new Paint()
  whitePaint.setColor(0xFFFFFFFF)
  whitePaint.setStyle(Paint.Style.FILL_AND_STROKE)
  whitePaint.setStrokeWidth(1)
  whitePaint.setAntiAlias(true)
 

  /** The game model currently rendered. */
  private var game: Game = null
  def setGame(g: Game) = {
    game = g
    game.setInstantProperties(state == Editing)
  }
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
      val a = (Array(0f, 0f, 0f, 0f) /: game.aliveObjects) { case (a, obj) =>
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
  
  def pickImage(): Unit = {
    this.activity.asInstanceOf[KingPong] ! PickImage()
  }
  
  // Testing section
  var obj_to_highlight: Set[GameObject] = Set.empty
  var codeMapping = Map[Int, Category]()
  var propMapping = Map[Int, Property[_]]()
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
//      if(propMapping != null) {
//        propMapping.get(start) match {
//          case Some(p) => // Activate the menu corresponding to this kind of property;
//            p.name match {
//              case "x" | "y" => evaluate(Block(moveMenu.obj := ObjectLiteral(p.parent),
//                  List(moveMenu.x, moveMenu.y) := fromGame(List(p.parent.x.expr, p.parent.y.expr)),
//                  moveMenu("visible") := true))
//              case _ =>
//            }
//          case _ =>
//        }
//      }
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
    game.setInstantProperties(true)
    layoutResize()
  }

  /** Change the current state to Running. */
  def toRunning(): Unit = if (state == Editing) {
    Log.d("kingpong", "toRunning()")
    // Remove objects that have been created after the date.
    mRuleState = STATE_MODIFYING_GAME
    MenuOptions.modify_prev = false
    MenuOptions.copy_to_prev = true
    changeMenuIcon(Str(R.string.menu_add_constraint_hint), bitmaps(R.drawable.menu_rule_editor))

    eventEditor.unselect()
    game.objects.foreach(obj => { obj.validate(); obj.flush() } )
    game.setInstantProperties(false)
    
    gameEngineEditors foreach (_.onExitEditMode())

    MenuCenter.registeredMenusCenters foreach {
      _.menus.foreach { _.hovered = false}
    }
    
    state = Running
  }

  /** Reset the game to its initial state. */
  def backToBeginning(): Unit = {
    toEditing()
    setProgressTime(0)
    game.reset()
  }

  def reset(newGame: Game): Unit = {
    game = newGame
  }

  def update(): Unit = {
    //super.update()
    state match {
      case Running =>
        game.update()
        setProgressTime(game.time)
      case Editing =>
    }
  }


  
  /** Menu drawing */
  private val res: Resources = context.getResources()
  val bitmaps = new MMap[Int, Drawable]()
  val drawables_to_load:List[Int] =
    List(R.drawable.finger,
        R.drawable.bing,
        R.drawable.bingselected,
        R.drawable.numbers,
        R.drawable.flat_button,
        R.drawable.flat_button_highlighted,
        R.drawable.flat_button_selected,
        R.drawable.flat_button_selected_highlighted,
        R.drawable.flat_button_m1,
        R.drawable.flat_button_p1,
        R.drawable.cross_move,
        R.drawable.move_velocity,
        R.drawable.move_size,
        R.drawable.move_rotate,
        R.drawable.wrench,
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
        R.drawable.flat_button_resizable_highlighted,
        //R.drawable.cursor_square,
        R.drawable.fingerdown_button,
        R.drawable.fingermove_button,
        R.drawable.fingerup_button,
        //R.drawable.menu_rule_apply,
        //R.drawable.no_collision,
        //R.drawable.no_collision_effect,
        R.drawable.collision_effect,
        R.drawable.outscreen,
        R.drawable.fingerdown,
        R.drawable.fingerup,
        R.drawable.fingermove,
        R.drawable.gear,
        R.drawable.back_arrow,
        R.drawable.jpeg,
        R.drawable.copy_menu
        //R.drawable.timebutton3
        )
  drawables_to_load.foreach { id =>
    bitmaps(id) = res.getDrawable(id)
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

  var currentFingerPos: Vec2 = null
  var fingerIsDown = false
  var fingerUpCanceled = false
  var touchDownOriginal = Vec2(0, 0)
  var touchDownOriginalGame = Vec2(0, 0)
  var selectedShapeCoords = Vec2(0 ,0)
  var mDisplacementX: Float = 0
  var mDisplacementY: Float = 0

  def onFingerDown(pos: Vec2): Unit = {
    state match {
      case Running => 
        val res = mapVectorToGame(pos)
        game.onFingerDown(res)
        currentFingerPos = res
        fingerIsDown = true
      case Editing =>
        fingerUpCanceled = false
        val x = pos.x
        val y = pos.y
        var return_value = highLightMenu(x, y)
        val p = mapVectorToGame(pos)
        touchDownOriginalGame = p
        touchDownOriginal = pos
        shapeEditor.selectedShape match {
          case selectedShape: Movable =>
              if(MenuOptions.modify_prev) {
                selectedShapeCoords = selectedShape.center.get
              } else {
                selectedShapeCoords = selectedShape.center.next
              }
              MenuOptions.selected_shape_first_x = selectedShapeCoords.x
              MenuOptions.selected_shape_first_y = selectedShapeCoords.y
              
              selectedShape match {
                case selectedShape:Circle =>
                  MenuOptions.selected_shape_first_radius = selectedShape.radius.getPrevOrNext(MenuOptions.modify_prev)
                case selectedShape:ResizableRectangular =>
                  MenuOptions.selected_shape_first_width = selectedShape.width.getPrevOrNext(MenuOptions.modify_prev)
                  MenuOptions.selected_shape_first_height = selectedShape.height.getPrevOrNext(MenuOptions.modify_prev)
                case _ =>
              }
              if(MoveButton.hovered) {
                mDisplacementX = 0
                mDisplacementY = 0
              }
        }
    }
  }
  
  /** Highlights the corresponding menu under the finger
   * Returns true if a menu has been highlighted.
   **/
  def highLightMenu(x: Float, y: Float): Boolean = {
    var stg_hovered = false
    gameEngineEditors foreach { editor =>
      if(editor.isVisible && !stg_hovered) {
        stg_hovered = editor.testHovering(x, y, button_size)
        if(stg_hovered && Options.Access.showTooltips) {
          editor.fireTooltips(context)
        }
      }
    }
    stg_hovered
  }
  
  /**
   * Selects objects
   */
  var previousSelectedShape: GameObject = null
  def performSelection(res: Vec2) = {
    val objectsTouched = game.abstractObjectFingerAt(res)
    obj_to_highlight = objectsTouched.toSet
    val rulesConcerned = game.getRulesbyObject(objectsTouched)
    updateCodeView(rulesConcerned, objectsTouched)
    shapeEditor.unselect()
    // The selected shape should be the first after the previousSelectedShape,
    // or the closest else.
    var afterPreviousSelectedShape = false
    var shapeToSelect: GameObject = null
    var minDistance = -1f
    def checkShapeToSelect(shape: GameObject) = {
      if(previousSelectedShape != null && !afterPreviousSelectedShape) {
        if(previousSelectedShape == shape) {
           afterPreviousSelectedShape = true
        }
      } else { // We are after the previously selected shape, so we can check the distance to the finger.
        shape match {
          case shape: Movable =>
            val x = res.x + (if(MenuOptions.modify_prev) shape.x.next - shape.x.get else 0)
            val y = res.y + (if(MenuOptions.modify_prev) shape.y.next - shape.y.get else 0)// - shape.y + shape.prev_y
            val dist = shape.distanceSelection(x, y)
            if(dist < minDistance || minDistance == -1) {
              minDistance = dist
              shapeToSelect = shape
            }
          case _ =>
        }
      }
    }
    
    objectsTouched foreach checkShapeToSelect
    if(shapeToSelect == null) {
      if(afterPreviousSelectedShape) {
        objectsTouched foreach checkShapeToSelect
      } else {
        previousSelectedShape = null
        objectsTouched foreach checkShapeToSelect
      }
    }
    if(shapeToSelect != null) {
      shapeEditor.select(shapeToSelect)
      previousSelectedShape = shapeToSelect
    }
  }

  /**
   * Called when a finger is up
   */
  def onFingerUp(pos: Vec2): Unit = state match {
    case Running => 
      val res = mapVectorToGame(pos)
      game.onFingerUp(res)
      fingerIsDown = false
      currentFingerPos = res
    case Editing =>
      // Select an object below if any and display the corresponding code
      val res = mapVectorToGame(pos)

      var menuSelected:Boolean = false
      //numberEditor.unselect()
      ColorMenu.activated = false
      SystemMenu.activated = false
      //if(fingerUpCanceled) return
      //GameMenu.onFingerUp(this, selectedShape, x, y)
      val x = pos.x
      val y = pos.y
      if(shapeEditor.selectedShape != null) {
        menuSelected = ShapeMenu.onFingerUp(this, shapeEditor.selectedShape, x, y)
      }
  
      if(!menuSelected) {
        // If the previous attempts to select a menu failed,
        // We dissmiss all selection and we select something else depending on the game editor type.
        ShapeMenu.cancelHovered()
        ColorMenu.cancelHovered()
        SystemMenu.cancelHovered()
        //RuleMenu.cancelHovered()
        
        val touchCoords = res
        
        if(mRuleState == STATE_SELECTING_EVENTS) {
          
          // Detects objects and events.
          
          // If we are making a rule, we select the events first
          // Sorts selectable events.
          val eventList = ListBuffer[(Event, Int)]()
          game.foreachEvent((a,b) => eventList += ((a, b)))
          val eventListFiltered = eventList.toList.filter(event_time => event_time._1.selectableBy(res.x, res.y))
          // sort by age and then distance
          object EventCompare extends scala.math.Ordering[(Event, Int)] {
            def compare(a:(Event, Int), b:(Event, Int)) = {
              val db = Math.abs(game.time - b._2)
              val da = Math.abs(game.time - a._2)
              if(da < db) {
                -1
              } else if(da == db) {
                val ca = a._1.distanceSquareTo(res.x, res.y)
                val cb = b._1.distanceSquareTo(res.x, res.y)
                if(ca < cb) -1 else if(ca == cb) 0 else 1
              } else 1
            }
          }
          
          val eventListSorted = eventListFiltered.sorted(EventCompare)
          val objectList = game.abstractObjectFingerAt(res).toList
          
          // Disambiguate event selection: make a list and submit it to quickaction review.
          disambiguateMultipleSelection(res, eventListSorted, objectList, eventEditor.selectedEventTime, eventEditor.selectedObjects){ case result =>
            result match {
              case (eventListSelected, objectListSelected) =>
                eventEditor.selectedEventTime = eventListSelected
                eventEditor.selectedObjects = objectListSelected
            }
          }
        } else if(mRuleState == STATE_MODIFYING_CATEGORY /*|| CameraButton.isSelected*/) {
          //categoryEditor.onFingerUp(touchCoords(0), touchCoords(1))
        } else if(mRuleState == STATE_SELECTING_EFFECTS) {
          performSelection(res/*, false*/)
        } else {
          performSelection(res)
        }
      }
      //super.onFingerUp(pos)
  }
  
  /**
   * Type of actions when selecting/deselecting events or objects.
   */
  object ActionType extends Enumeration {
    type ActionType = Value
    val ID_CONTINUE = Value
    implicit def convert(i: ActionType): Int = i.id
  }
  import ActionType._
  def str(i: Int, j: String*) = String.format(res.getString(i), j: _*)
  lazy val drw = (i: Int) => res.getDrawable(i)
  lazy val drw2 = (i: Int, j: Int) => {
    val d1 = res.getDrawable(i)
    val d2 = res.getDrawable(j)
    val big = Bitmap.createBitmap(d1.getIntrinsicWidth(), d1.getIntrinsicHeight(), Bitmap.Config.ARGB_8888)
    val canvas = new Canvas(big);
    d1.setBounds(0, 0, d1.getIntrinsicWidth()-1, d1.getIntrinsicHeight()-1)
    d1.draw(canvas)
    d2.setBounds(0, 0, d1.getIntrinsicWidth()-1, d1.getIntrinsicHeight()-1)
    d2.draw(canvas)
    new BitmapDrawable(big)
  }
  //lazy val continueItem = new ActionItem(ID_CONTINUE, str(R.string.tutorial_continue), drw(R.drawable.menu_right_arrow))
  
  /**
   * Displays a tooltip if there are multiple items to check.
   */
  def disambiguateMultipleSelection(res: Vec2, eventList: List[(Event, Int)], objectList: List[GameObject],
      currentEventSelection: List[(Event, Int)], currentObjectSelection: List[GameObject]
  )(remaining: (List[(Event, Int)], List[GameObject]) => Unit): Unit = {
    val mQuickAction  = new QuickAction(activity, false)
    
    //mQuickAction.addStickyActionItem(changeStateItem)
    var index = 0
    var mapIndex = Map[Int, (Event, Int)]()
    var moveTaken = Set[FingerMove]()
    for(e@(event, time) <- eventList) {
      event match {
        case BeginContact(contact) =>
          if(currentEventSelection contains e) {
            mQuickAction.addActionItem(new ActionItem(index, str(R.string.when_collision, contact.objectA.name.get, contact.objectB.name.get), drw2(R.drawable.event_selected_disambiguate, R.drawable.collision_effect)))
          } else {
            mQuickAction.addActionItem(new ActionItem(index, str(R.string.when_collision, contact.objectA.name.get, contact.objectB.name.get), drw(R.drawable.collision_effect)))
          }
          mapIndex += index -> (e)
          index += 1
        
        case FingerUp(pos, objs) =>
          for(obj <- objs) {
            val e = (FingerUp(pos, Set(obj)), time)
            if(currentEventSelection contains e) {
              mQuickAction.addActionItem(new ActionItem(index, str(R.string.when_finger_up, obj.name.get), drw2(R.drawable.event_selected_disambiguate, R.drawable.fingerup_button)))
            } else {
              mQuickAction.addActionItem(new ActionItem(index, str(R.string.when_finger_up, obj.name.get), drw(R.drawable.fingerup_button)))
            }
            mapIndex += index -> e
            index += 1
          }
        case FingerMove(_, _, _) => // Display only the relevant one.
          val relevantMove = getGame.getFingerMoveEvent(event, time)()
          relevantMove match {
            case Some(movement@FingerMove(from, to, objs)) =>
              if(!moveTaken(movement)) {
                moveTaken += movement
                for(obj <- objs) {
                  val e =  (FingerMove(from, to, Set(obj)), time)
                  if(currentEventSelection contains e) {
                    mQuickAction.addActionItem(new ActionItem(index, str(R.string.when_finger_move, obj.name.get), drw2(R.drawable.event_selected_disambiguate, R.drawable.fingermove_button)))
                  } else {
                    mQuickAction.addActionItem(new ActionItem(index, str(R.string.when_finger_move, obj.name.get), drw(R.drawable.fingermove_button)))
                  }
                  mapIndex += index -> e
                  index += 1
                }
              }
            case _ =>
          }
        case FingerDown(pos, objs) =>
          for(obj <- objs) {
            val e = (FingerDown(pos, Set(obj)), time)
            if(currentEventSelection contains e) {
              mQuickAction.addActionItem(new ActionItem(index, str(R.string.when_finger_down, obj.name.get), drw2(R.drawable.event_selected_disambiguate, R.drawable.fingerdown_button)))
            } else {
              mQuickAction.addActionItem(new ActionItem(index, str(R.string.when_finger_down, obj.name.get), drw(R.drawable.fingerdown_button)))
            }
            mapIndex += index -> e
            index += 1
          }
        case _ => // TODO : Add more actions to disambiguate (position?)
      }
    }
    
    mQuickAction.setOnActionItemClickListener{ (quickAction: QuickAction, pos: Int, actionId: Int) =>
      mapIndex.get(actionId) match {
        case Some(event) =>
          mQuickAction.dismiss()
          if(currentEventSelection contains event){
            remaining(currentEventSelection.filterNot(_ == event), currentObjectSelection)
          } else {
            remaining(event::currentEventSelection, currentObjectSelection)
          }
        case None =>
          mQuickAction.dismiss()
        case _ =>
      }
    }
    val pos = mapVectorFromGame(res)
    val rect = new RectF(pos.x, pos.y, pos.x, pos.y)
    val rectd = new Rect()
    rect.round(rectd)
    mQuickAction.show(rectd, progressBar)
  }
  
  /**
   * Sets an image for the selected shape
   * @param i The Bitmap to apply to the shape.
   */
  def setImageSelectedShape(i: Bitmap) = {
    if(shapeEditor.selectedShape != null) {
      shapeEditor.selectedShape match {
        case selectedShape: Colorable =>
          val finalbitmap = selectedShape match {
            case c: Circle =>
              getRoundedShape(i)
            case _ =>
              i
          }
          val id = Stream.from(0).find(i => !(bitmaps contains i)).get
          bitmaps(id) = new BitmapDrawable(finalbitmap)
          selectedShape.color setNext id
        case _ =>
      }
    }
  }
  
 /**
  * Making image in circular shape
  * Source: http://www.androiddevelopersolution.com/2012/09/crop-image-in-circular-shape-in-android.html
  */
 def getRoundedShape(scaleBitmapImage: Bitmap): Bitmap = {
  val targetSize = Math.min(scaleBitmapImage.getWidth, scaleBitmapImage.getHeight)
  val targetBitmap = Bitmap.createBitmap(targetSize, 
                            targetSize,Bitmap.Config.ARGB_8888);
  
  val canvas = new Canvas(targetBitmap);
  val path = new Path();
  path.addCircle((targetSize.toFloat - 1) / 2,
  (targetSize.toFloat - 1) / 2,
  (targetSize / 2),
          Path.Direction.CCW);
  
  canvas.clipPath(path);
  val sourceBitmap = scaleBitmapImage;
  canvas.drawBitmap(sourceBitmap, 
                                new Rect(0, 0, sourceBitmap.getWidth(), sourceBitmap.getHeight()), 
                                new Rect(0, 0, targetSize, targetSize), null);
  return targetBitmap;
 }
 
  
  /**
   * Updates the code view
   * @param rules The rules to display
   * @param objects The objects to highlight.
   */
  def updateCodeView(rules: Iterable[Expr], objects: Iterable[GameObject]) = {
    val header = PrettyPrinterExtended.printGameObjectDef(objects)
    val all = PrettyPrinterExtended.print(rules, header + "\n")
    val r: CharSequence = all.c
    val mapping = all.map
    val mObjects = mapping.mObjects
    codeMapping = mapping.mPosCategories
    propMapping = mapping.mPropertyPos
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
  
  /**
   * Puts a new matrix for the view.
   * @param m The matrix for the new transformation.
   */
  def push(m: Matrix) = {
    m.invert(matrixI)
    
    gameViewRender.grid = Grid(matrixI, width=mWidth, numSteps=15, stroke_width=1, color=0x88000000)
    if(game != null) game.FINGER_SIZE = matrixI.mapRadius(35)
  }

  /**
   * One Finger moves
   * @param from The first coordinate
   * @param to   The second coordinate
   */
  def onOneFingerMove(from: Vec2, to: Vec2): Unit = state match {
    case Running => 
      val res = mapVectorToGame(to)
      game.onOneFingerMove(mapVectorToGame(from), res)
      currentFingerPos = res
    case Editing =>
      //matrix.postTranslate(to.x - from.x, to.y - from.y)
      //push(matrix)
      //super.onOneFingerMove(from, to)
      mDisplacementX = to.x - touchDownOriginal.x
      mDisplacementY = to.y - touchDownOriginal.y
      val touchCoords = mapVectorToGame(to)
      val touchCoords2 = mapVectorToGame(from)
      val shiftX = touchCoords.x - touchCoords2.x
      val shiftY = touchCoords.y - touchCoords2.y
      val simpleRelativeX = touchCoords.x - touchDownOriginalGame.x
      val simpleRelativeY = touchCoords.y - touchDownOriginalGame.y
      var relativeX = simpleRelativeX
      var relativeY = simpleRelativeY
      // Snap to grid.
      if(Math.abs(relativeX) < 0.05f) { relativeX = 0 }
      if(Math.abs(relativeY) < 0.05f) { relativeY = 0 }
      ShapeMenu.onFingerMove(this, shapeEditor.selectedShape, relativeX, relativeY, shiftX, shiftY, mDisplacementX, mDisplacementY)
      if(ColorMenu.activated) {
        ColorMenu.testHovering(to.x, to.y, button_size)
        ColorMenu.onFingerMove(this, shapeEditor.selectedShape, relativeX, relativeY, shiftX, shiftY, mDisplacementX, mDisplacementY)
      }
      if(SystemMenu.activated) {
        SystemMenu.testHovering(to.x, to.y, button_size)
        SystemMenu.onFingerMove(this, shapeEditor.selectedShape, relativeX, relativeY, shiftX, shiftY, mDisplacementX, mDisplacementY)
      }
      /*if(EventMenu.isActivated) {
        EventMenu.testHovering(to.x, to.y, button_size)
        EventMenu.onFingerMove(this, shapeEditor.selectedShape, relativeX, relativeY, shiftX, shiftY, mDisplacementX, mDisplacementY)
      }*/
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
  def mapVectorFromGame(p: Vec2): Vec2 = {
    val toMap = Array(p.x, p.y)
    matrix.mapPoints(toMap)
    Vec2(toMap(0), toMap(1))
  }
  
  def mapVectorFromGame(in: Array[Float], out: Vec2): Vec2 = {
    matrix.mapPoints(in)
    out.x = in(0)
    out.y = in(1)
    out
  }
  def mapVectorFromGame(in: Vec2, inarray: Array[Float], out: Vec2): Vec2 = {
    inarray(0) = in.x
    inarray(1) = in.y
    matrix.mapPoints(inarray)
    out.x = inarray(0)
    out.y = inarray(1)
    out
  }
  
  /** meters to pixels */
//  def mapVectorFromGame(p: Vec2V): Vec2V = {
//    val toMap = Array(p.x, p.y)
//    matrix.mapPoints(toMap)
//    Vec2V(toMap(0), toMap(1))
//  }
  

  /** pixels to meters */
  def mapVectorToGame(p: Vec2): Vec2 = {
    val toMap = Array(p.x, p.y)
    matrixI.mapPoints(toMap)
    Vec2(toMap(0), toMap(1))
  }
  
  
  /** meters to pixels */
//  def mapVectorToGame(p: Vec2V): Vec2V = {
//    val toMap = Array(p.x, p.y)
//    matrixI.mapPoints(toMap)
//    Vec2V(toMap(0), toMap(1))
//  }

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
          last(pointerIndex) = point

        // A finger moves
        case MotionEvent.ACTION_MOVE =>
          if(me.getPointerCount() == 1 || state == Running) {
            var i = me.getPointerCount()-1
            while(i>=0) {
              val pointerIndex = Math.min(me.getPointerId(i), FINGERS - 1)
              val from = last(pointerIndex)
              val to = Vec2(me.getX(0), me.getY(0))
              //Log.d("GameView", s"Moved from ${from.x}, ${from.y} to ${to.x}, ${to.y}")
              onOneFingerMove(from, to)
              last(pointerIndex) = to
              i -= 1
            }
          } else if (me.getPointerCount() == 2) {
            val pointerIndex1 = Math.min(me.getPointerId(0), FINGERS - 1)
            val pointerIndex2 = Math.min(me.getPointerId(1), FINGERS - 1)
            val from1 = last(pointerIndex1)
            val from2 = last(pointerIndex2)
            val to1 = Vec2(me.getX(0), me.getY(0))
            val to2 = Vec2(me.getX(1), me.getY(1))
            onTwoFingersMove(from1, to1, from2, to2)
            last(pointerIndex1) = to1
            last(pointerIndex2) = to2
          }

        case MotionEvent.ACTION_UP | MotionEvent.ACTION_POINTER_UP =>
          val pointerIndex = (action & MotionEvent.ACTION_POINTER_INDEX_MASK) >> MotionEvent.ACTION_POINTER_INDEX_SHIFT
          val point = Vec2(me.getX(pointerIndex), me.getY(pointerIndex))
          if(last(pointerIndex).x != point.x || last(pointerIndex).y != point.y)
            onOneFingerMove(last(pointerIndex), point)
          onFingerUp(point)
          last(pointerIndex) = point

        case _ => //Do nothing
      }
    }
  }

}