package ch.epfl.lara.synthesis.kingpong.menus

import scala.collection.mutable.HashMap
import ch.epfl.lara.synthesis.kingpong.GameEngine2DView
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.ast._
import ch.epfl.lara.synthesis.kingpong.GameShapes._
import ch.epfl.lara.synthesis.kingpong.R
import android.graphics.Canvas
import android.graphics.drawable.Drawable
import ch.epfl.lara.synthesis.kingpong.GameShapes
import android.widget.Toast
import ch.epfl.lara.synthesis.kingpong.CustomDialogs

object StaticMenu {
  var menus: List[MenuButton] = List(AddRectangleButton, AddCircleButton, AddDigitButton, AddTextButton, AccelerometerButton, Gravity2DButton, CameraButton, AddRuleButton, SelectPrevNextButton, ChooseExistingRuleButton)
  def draw(canvas: Canvas, gameEngine: GameEngine2DView, selectedShape: Shape, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float) = {
    
    AddRectangleButton.setPos(0, 0)
    AddCircleButton.setPos(1, 0)
    AddDigitButton.setPos(2, 0)
    AddTextButton.setPos(3, 0)
    AccelerometerButton.setPos(0, 1)
    Gravity2DButton.setPos(1, 1)
    CameraButton.setPos(2, 1)
    AddRuleButton.setPos(0, 2)
    if(gameEngine.mRuleState == gameEngine.STATE_SELECTING_EFFECTS) {
      SelectPrevNextButton.visible = true
    } else {
      SelectPrevNextButton.visible = false
    }
    SelectPrevNextButton.setPos(1, 2)
    ChooseExistingRuleButton.visible = (gameEngine.mRuleState == gameEngine.STATE_SELECTING_EVENTS)
    ChooseExistingRuleButton.setPos(0, 3)
    
    for(menu <- menus) {
      menu.draw(canvas, gameEngine, selectedShape, bitmaps, cx, cy)
    }
  }
  
  def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float): Boolean = { 
    val res = menus.foldLeft(false) { // Priority to the flying menus first
      (hovered, menu) => if(!hovered) {
        if(menu.hovered) {
          menu.onFingerUp(gameEngine, selectedShape, x, y)
          true
        } else false           
      } else true
    }
    res
  }
  
  def onFingerMove(selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    menus foreach {
      menu =>
        if(menu.hovered) menu.onFingerMove(selectedShape, relativeX, relativeY, shiftX, shiftY, mDisplacementX, mDisplacementY)
    }
  }
  
  def testHovering(x: Float, y: Float, button_size: Float) {
    menus foreach (_.testHovering(x, y, button_size))
  }
}

/**
 * Adds a shape to the shape arena
 */
object AddShape {
  def addShape(gameEngine: GameEngine2DView, game: Game, r: Shape, name: String, duplicate: Boolean) = {
        r.storePrevValues()
        game.getArena += r
        val oldSelectedShape = gameEngine.selectedShape
        val oldName = if(gameEngine.selectedShape != null) gameEngine.selectedShape.mName else null
        gameEngine.selectShape(r)
        gameEngine.renameSelectedShape(name)
        if(duplicate) CodeGenerator.duplicateRuleContaining(game, oldSelectedShape, r)
        val ident = EIdentShape(r)
        game.context(r.mName) = ident
  }
}

/**
 * Menu to add a rectangle
 */
object AddRectangleButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    var r: Shape = null
    val game = gameEngine.getGame()
    var name: String = null
    var duplicate = false
    selectedShape match {
      case model:Rectangle =>
        // TODO : implement the "clone" button
        r = game.Rectangle(game.screenWidth / 2, game.screenHeight / 2, model.width, model.height)
        r.velocity_x = selectedShape.velocity_x
        r.velocity_y = selectedShape.velocity_y
        r.noVelocity = selectedShape.noVelocity
        r.color = selectedShape.color
        name = model.name
        duplicate = true
      case _ =>
        r = game.Rectangle(game.screenWidth / 2, game.screenHeight / 2, button_size.toInt, button_size.toInt)
        name = "Wall"
    }
    AddShape.addShape(gameEngine, game, r, name, duplicate)
    hovered = false
  }

  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: 
       R.drawable.menu_add_rect ::  Nil
}


/**
 * Menu to add a circle
 */
object AddCircleButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    var r: Shape = null
    val game = gameEngine.getGame()
    var name: String = null
    var duplicate = false
    selectedShape match {
      case model:Circle =>
        // TODO : implement the "clone" button
        r = game.Circle(game.screenWidth / 2, game.screenHeight / 2, model.radius)
        r.velocity_x = selectedShape.velocity_x
        r.velocity_y = selectedShape.velocity_y
        r.noVelocity = selectedShape.noVelocity
        r.color = selectedShape.color
        name = model.name
        duplicate = true
      case _ =>
        r = game.Circle(game.screenWidth / 2, game.screenHeight / 2, 50)
        name = "Ball"
    }
    AddShape.addShape(gameEngine, game, r, name, duplicate)
    hovered = false
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: 
       R.drawable.menu_add_circle ::  Nil
}

/**
 * Menu to add a IntegerBox
 */
object AddDigitButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    var r: Shape = null
    val game = gameEngine.getGame()
    var name: String = null
    var duplicate = false
    selectedShape match {
      case model:IntegerBox =>
        // TODO : implement the "clone" button
        r = game.IntegerBox(game.screenWidth / 2, game.screenHeight / 2, model.width.toInt, model.height.toInt, 0)
        r.velocity_x = selectedShape.velocity_x
        r.velocity_y = selectedShape.velocity_y
        r.noVelocity = selectedShape.noVelocity
        r.color = selectedShape.color
        name = model.name
        duplicate = true
      case _ =>
        r = game.IntegerBox(game.screenWidth / 2, game.screenHeight / 2, button_size.toInt/2, button_size.toInt/2, 0)
        name = "Score"
    }
    AddShape.addShape(gameEngine, game, r, name, duplicate)
    hovered = false
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: 
       R.drawable.menu_add_digit ::  Nil
}


/**
 * Menu to add a TextBox
 */
object AddTextButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    var r: Shape = null
    val game = gameEngine.getGame()
    var name: String = null
    var duplicate = false
    selectedShape match {
      case model:TextBox =>
        // TODO : implement the "clone" button
        r = game.TextBox(game.screenWidth / 2, game.screenHeight / 2, model.width, model.height, model.text)
        r.velocity_x = selectedShape.velocity_x
        r.velocity_y = selectedShape.velocity_y
        r.noVelocity = selectedShape.noVelocity
        r.color = selectedShape.color
        name = model.name
        duplicate = true
      case _ =>
        r = game.TextBox(game.screenWidth / 2, game.screenHeight / 2, button_size.toInt*2, button_size.toInt/2, "Custom text")
        name = "Label"
    }
    AddShape.addShape(gameEngine, game, r, name, duplicate)
    hovered = false
  }

  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: 
       R.drawable.menu_add_text ::  Nil
}

/**
 * Menu to add the accelerometer
 */
object AccelerometerButton extends MenuButton {
  import MenuOptions._
  var selected = false
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    selected = !selected
    val game = gameEngine.getGame()
    if(selected) {
      gameEngine.selectedCategory = game.AccelerometerGravity
      gameEngine.setModeSelectCategory()
      if(gameEngine.selectedShape != null) {
        gameEngine.selectedCategory.add(selectedShape)
      }
    } else {
      gameEngine.selectedCategory = null
      gameEngine.setModeModifyGame()
    }
    hovered = false
  }

  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) (if(selected) R.drawable.flat_button_selected_highlighted else R.drawable.flat_button_highlighted) else (if(selected) R.drawable.flat_button_selected else R.drawable.flat_button)) :: R.drawable.menu_add_accelerometer :: Nil
}

/**
 * Menu to add the accelerometer
 */
object Gravity2DButton extends MenuButton {
  import MenuOptions._
  var selected = false
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    selected = !selected
    val game = gameEngine.getGame()
    if(selected) {
      gameEngine.selectedCategory = game.Gravity2D
      gameEngine.setModeSelectCategory()
      if(gameEngine.selectedShape != null) {
        gameEngine.selectedCategory.add(selectedShape)
      }
    } else {
      gameEngine.selectedCategory = null
      gameEngine.setModeModifyGame()
    }
    hovered = false
  }

  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) (if(selected) R.drawable.flat_button_selected_highlighted else R.drawable.flat_button_highlighted) else (if(selected) R.drawable.flat_button_selected else R.drawable.flat_button)) :: R.drawable.menu_add_force_field :: Nil
}

/**
 * Menu to add the camera
 */
object CameraButton extends MenuButton {
  import MenuOptions._
  var selected = false
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    selected = !selected
    val game = gameEngine.getGame()
    if(selected) {
      gameEngine.selectShape(game.Camera)
      gameEngine.selectedCategory = game.Camera
    } else {
      gameEngine.selectShape(null)
      gameEngine.selectedCategory = null
      gameEngine.setModeModifyGame()
    }
    hovered = false
  }

  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) (if(selected) R.drawable.flat_button_selected_highlighted else R.drawable.flat_button_highlighted) else (if(selected) R.drawable.flat_button_selected else R.drawable.flat_button)) :: R.drawable.menu_camera :: Nil
}

/**
 * Button to add rules.
 */
object AddRuleButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    var menuSelected = false
    val res = context.getResources()
    val game = gameEngine.getGame()
    gameEngine.mRuleState match {
      case gameEngine.STATE_MODIFYING_GAME => 
        gameEngine.setModeSelectingEffects()
        hovered = true
        menuSelected = true
        gameEngine.selectEvent(null)
        gameEngine.selectShape(null)
        //mAddRuleButton.text = res.getString(R.string.select_event)
        Toast.makeText(context, res.getString(R.string.select_event_toast), 2000).show()
      case gameEngine.STATE_SELECTING_EVENTS =>
        Toast.makeText(context, res.getString(R.string.rule_canceled), 2000).show()
        gameEngine.setModeModifyGame()
        //mAddRuleButton.text = res.getString(R.string.design_rule)
        hovered = false
        gameEngine.selectEvent(null)
      case gameEngine.STATE_SELECTING_EFFECTS =>
        if(gameEngine.selectedEvent != null) { // Means that the rule has been confirmed.
          CodeGenerator.createRule(context, gameEngine.getGame(), gameEngine.selectedEvent.value, gameEngine.selectedEvent.timestamp, {
            rule =>
              // Here if the rule changes some number, add the corresponding effects.
              gameEngine.getGame().storeState(game.currentTime, true, null)
              gameEngine.selectRule(rule)
          })
        }
        gameEngine.setModeModifyGame()
    }
    menuSelected
  }

  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) = {
    var selected = (gameEngine.mRuleState == gameEngine.STATE_SELECTING_EVENTS || gameEngine.mRuleState == gameEngine.STATE_SELECTING_EFFECTS)
    (if(gameEngine.mRuleState == gameEngine.STATE_SELECTING_EVENTS || gameEngine.mRuleState == gameEngine.STATE_SELECTING_EFFECTS) (if(hovered) R.drawable.flat_button_selected_highlighted else R.drawable.flat_button_highlighted) else (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button)) ::
      (if(gameEngine.mRuleState == gameEngine.STATE_SELECTING_EFFECTS) R.drawable.menu_rule_maker else R.drawable.menu_rule_editor)::Nil
  }
}

/**
 * Button to add rules.
 */
object SelectPrevNextButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    MenuOptions.modify_prev = !MenuOptions.modify_prev
    hovered = false
  }

  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) = {
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) ::
    (if(MenuOptions.modify_prev) R.drawable.prev_effects else R.drawable.next_effects) :: Nil
  }
}

/**
 * Button to choose among existing rules (soon to be replaced by graphical rules)
 */
object ChooseExistingRuleButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    val game = gameEngine.getGame()
    val res = context.getResources()
    val many_rules = game.init_rules.toList
    CustomDialogs.launchRuleChooserDialog(context, res.getString(R.string existing_rule_dialog_title),
        many_rules map (_.toScalaString("", game.context)),
        many_rules,
        { rule => 
          // Let the code apply to the game, and the user edit the output. 
          CodeGenerator.modifyAndInsertRule(context, game, rule, game.currentTime, {rule =>
            //Replay the rule and select its effects.
            // Need to select an event corresponding to the rule.
            val event = gameEngine.findEventForRule(rule)
            if(event != null) {
              gameEngine.setModeSelect()
              game.storePrevValues()
              gameEngine.selectRule(rule, event)
              gameEngine.triggerRule(rule, event)
            } else {
              gameEngine.setModeModifyGame()
            }
          })
        },
        { () => })
    hovered = false
  }

  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) = {
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: 
      R.drawable.existing_rules :: Nil
  }
}

