package ch.epfl.lara.synthesis.kingpong.menus

import ch.epfl.lara.synthesis.kingpong._
import objects._
import menus._
import expression.Trees._
import scala.collection.mutable.ArrayBuffer
import android.graphics.Canvas
import android.widget.Toast
import android.content.Context
import ch.epfl.lara.synthesis.kingpong.expression.CodeGenerator
import ch.epfl.lara.synthesis.kingpong.rules.Events._


/**
 * GameEngineEditor
 * 
 * An editor has the following functionalities:
 * - it can be visible
 * - it allows to test the finger hovering it
 * - it allows to disable editing.
 */  
abstract class GameEngineEditor(gameEngineView: GameView) {
  gameEngineView.addGameEngineEditor(this)
  def onExitEditMode() {
    unselect()
  }
  def unselect()
  
  def isVisible(): Boolean
  
  def testHovering(x: Float, y: Float, button_size: Float): Boolean
  
  def menus: List[CustomMenu]
  
  def fireTooltips(context: Context) = {
    menus foreach {
      menu =>
        val hint = menu.hint(context.getResources())
        if(menu.hovered && Options.Access.showTooltips) {
          Toast.makeText(context, hint, Toast.LENGTH_SHORT).show()
        }
    }
  }
}

/**
 * ShapeEditor
 * 
 * Wrapper to the menu to edit the shapes.
 */
class ShapeEditor(gameEngineView: GameView) extends GameEngineEditor(gameEngineView) {
  
  var selectedShape: GameObject = null
  def unselect() = {
    select(null)
  }
  def isVisible() = selectedShape != null
  /**
   * Select a given shape.
   **/
  def select(shape: GameObject) = {
    selectedShape = shape
    if(selectedShape != null) {
      // Review this code !!
      /*if(CameraButton.selected && CategoryEditor.selectedCategory != null) {
        CategoryEditor.selectedCategory.add(shape)
        selectedShape = gameEngineView.getGame().Camera
      }*/
    }
  }
  
  def menus: List[CustomMenu] = ShapeMenu.menus
  
  def testHovering(x: Float, y: Float, button_size: Float): Boolean = ShapeMenu.testHovering(x, y, button_size)
  
  /** Renames the selected shape and tries to find an unexisting name
   *  Removes the trailing numbers at the end
   **/
  def renameSelectedShape(baseName: String): Unit = {
    if(selectedShape != null) {
      val game = gameEngineView.getGame()
      val newName = game.getNewName(baseName)
      game.rename(selectedShape, newName)
    }
  }
}

/**
 * CategoryEditor
 * 
 * Represents an editor for a selected category.
 */
/*class CategoryEditor(gameEngineView: GameView) extends GameEngineEditor(gameEngineView) {
  var selectedCategory: Category[GameObject] = null
  def unselect() = {
    selectedCategory = null
  }
  
  def draw(canvas: Canvas, gameEngine: GameView) = {
    if(selectedCategory != null) {
      selectedCategory foreach { shape => 
        gameEngine.drawSelectedBackground(canvas, shape)
      }
    }
  }
  
  def isVisible = false
  
  def testHovering(x: Float, y: Float, button_size: Float): Boolean = false
   
  /** Changes the selection of the category. */
  def onFingerUp(xTouch: Float, yTouch: Float) = {
    if(selectedCategory != null)  {
      var found = false
      gameEngineView.getGame().getArena foreach { shape =>
        val x = xTouch - shape.x + shape.prev_x
        val y = yTouch - shape.y + shape.prev_y
        if(shape.selectableBy(x, y) && shape.distanceSelection(x, y) == 0) {
          found = found || selectedCategory.turnOnOff(shape)
        }
      }
      if(!found && CameraButton.isSelected) {
        selectedCategory.reset()
      }
    }
  }
  
  def menus: List[CustomMenu] = Nil
}*/

/**
 * EventEditor
 * 
 * Wrapper to the menu to edit events.
 */
class EventEditor(gameEngineView: GameView) /*extends GameEngineEditor(gameEngineView)*/ {
  var selectedObjects: List[GameObject] = Nil
  var selectedEventTime: List[(Event, Int)] = Nil
  
  def unselect() = {
    selectedEventTime = Nil
    selectedObjects = Nil
  }
  def isVisible() = EventMenu.isActivated && selectedEventTime.nonEmpty
  /**
   * Selects the given event by returning to the time this event occurred
   * Changes the game new values by the rule.
   **/
  def select(i: Event, time: Int, applyRule: Boolean = true) = {
    if(i != null && applyRule) {
      val game = gameEngineView.getGame()
      selectedEventTime = (i, time.toInt)::selectedEventTime
      
      // TODO : Select code portion to modify
      //val ruleToStopBefore = CodeGenerator.getRuleFromEvent(game, i.value) match { case Some(r) => r; case _ => null }
      
      //TODO clear or not clear in this restore ?
      game.restore(time)
      // Apply the rule that corresponds to the event.
      gameEngineView.setProgressBarTime(time)
      /*if(ruleToStopBefore != null) {
        triggerRule(ruleToStopBefore, i.value)
      }*/
    }
  }

  /**
   * Triggers the rule
   **/
  /*def triggerRule(e: ReactiveRule, i: Event) = {
    val game = gameEngineView.getGame()
    i.code match {
      case TOUCHMOVE_EVENT =>
        game.updateContextMoveCoordinates(i.x1, i.y1, i.x2, i.y2)
      case INTEGER_CHANGE_EVENT | INTEGER_EQUAL_EVENT | INTEGER_GREATER_EQUAL_EVENT | INTEGER_LESS_EQUAL_EVENT | INTEGER_POSITIVE_EVENT | INTEGER_NEGATIVE_EVENT =>
        game.updateContextValueChanged(i.y2.toInt)
        //i.shape1.asInstanceOf[IntegerBox].prev_value = i.x2.toInt
        //i.shape1.asInstanceOf[IntegerBox].value = i.y2.toInt
      case _ =>
    }
    e.execute(game.context, false)
  }*/
  
  //def testHovering(x: Float, y: Float, button_size: Float): Boolean = EventMenu.testHovering(x, y, button_size)
  
  //def menus: List[CustomMenu] = EventMenu.menus
}

/**
 * RuleEditor
 * 
 * Wrapper to the menu to edit rules.
 */
/*class RuleEditor(gameEngineView: GameView) extends GameEngineEditor(gameEngineView) {
  var selectedRule: ReactiveRule = null
  var selectedRuleActions = new ArrayBuffer[Action] // Line, character, length, type of expression
  var selectedRuleString: String = null // "testRuleDisplay() {\n  constant = 1\n}"
  var selectedRuleStringSplit: Array[String] = null //selectedRuleString.split("\n")
  
  /** Selects a given rule. Adds a watcher on the integer if the rule selected depends on an integer. */
  def select(rule: ReactiveRule, event: Event = null, close_previous: Boolean = true) = {
    if(selectedRule != null && close_previous) {
      val game = gameEngineView.getGame()
      if(EditRuleButton.selected) {
        game.restorePrev()
      }
      EditRuleButton.selected = false
    }
    selectedRule = rule
    updateSelectedRule()
  }
  
  def unselect() = {
    selectedRule = null
    selectedRuleActions.clear()
    selectedRuleString = null
    selectedRuleStringSplit = null
  }
   def isVisible() = selectedRule != null && gameEngineView.mRuleState != gameEngineView.STATE_SELECTING_EVENTS
   
  /**
   * Updates the current rule
   */
  def updateSelectedRule() {
    if(selectedRule != null) {
      val game = gameEngineView.getGame()
      selectedRuleActions.clear()
      selectedRuleString = selectedRule.toScalaString("", game.context, selectedRuleActions, Some(1), Some(1))
      selectedRuleStringSplit = selectedRuleString.split("\n")
    }
  }
  
  def draw(canvas: Canvas, gameEngine: GameView) = {
    if(selectedRule != null) {
      CodeEditorHandle.drawRule(canvas, gameEngine, selectedRule)
      CodeEditorHandle.drawRuleCode(canvas, gameEngine, selectedRule)
    }
  }
  
  private var coords = Array(0.0f, 0.0f)
  
  def testHovering(x: Float, y: Float, button_size: Float): Boolean = {
    var hovered = false
    coords(0) = x
    coords(1) = y
    gameEngineView.coordsFromScreenToGame(coords)
    if(selectedRule != null) {
      if(selectedRuleActions != null && EditRuleButton.selected) {
        for(action <- selectedRuleActions) {
          hovered = hovered || action.testHovering(coords(0), coords(1))
        }
      }
    }
    if(!hovered) {
      RuleMenu.testHovering(x, y, button_size)
    } else hovered
  }
  
  def menus: List[CustomMenu] = RuleMenu.menus
}*/

/**
 * NumberEditor
 * 
 * Wrapper to the implicit menu to edit numbers on the rules
 */
class NumberEditor(gameEngineView: GameView) extends GameEngineEditor(gameEngineView) {
  var selectedDragNumber: Option[Expr] = None
  var selectedDragNumberInitialValue: Float = 0f
  
  def select(s: Option[Expr], init: Float) = {
    selectedDragNumber = s
    selectedDragNumberInitialValue = init
  }
  
  def unselect() = {
    selectedDragNumber = None
    selectedDragNumberInitialValue = 0
  }
  
  def isVisible = false
  def testHovering(x: Float, y: Float, button_size: Float): Boolean = false
  
  def menus: List[CustomMenu] = Nil
}

/**
 * SystemEditor
 * 
 * Wrapper to the time properties related to the object
 */
class SystemEditor(gameEngineView: GameView) extends GameEngineEditor(gameEngineView) {
  def unselect() = {
  }
  
  def isVisible() = gameEngineView.isInEditMode()
  
  def testHovering(x: Float, y: Float, button_size: Float): Boolean = SystemMenu.testHovering(x, y, button_size)
  
  def menus: List[CustomMenu] = SystemMenu.menus
}
/**
 * GameEditor
 * 
 * Wrapper to the static values of the game like the layout size.
 */
/*class GameEditor(gameEngineView: GameView) extends GameEngineEditor(gameEngineView) {
  def unselect() = {
  }
  
  def isVisible() = gameEngineView.getGame().currentTime == 0
  
  def testHovering(x: Float, y: Float, button_size: Float): Boolean = GameMenu.testHovering(x, y, button_size)
  
  def menus: List[CustomMenu] = GameMenu.menus
}*/