package ch.epfl.lara.synthesis.kingpong.menus

import scala.collection.mutable.HashMap
import ch.epfl.lara.synthesis.kingpong.GameShapes._
import ch.epfl.lara.synthesis.kingpong._
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.drawable.Drawable
import android.content.Context

object RuleMenu {
  var menus: List[CustomMenu] = List(MoveRuleButton, EditRuleButton, TrashRuleButton)
  def draw(canvas: Canvas, gameEngine: GameEngine2DView, selectedShape: Shape, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float): Unit = {
    
    //RenameButtonRule.setText(selectedShape.mName)
    //RenameButtonRule.setPos(gameEngine.whitePaint, 33f/49f, 0, top_shift-1)
    MoveButton.setPos(0, 0)
    EditRuleButton.setPos(0, 1)
    TrashRuleButton.setPos(1, 1)
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
  
  def onFingerMove(gameEngine: GameEngine2DView, selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    menus foreach {
      menu =>
        if(menu.hovered) menu.onFingerMove(gameEngine, selectedShape, relativeX, relativeY, shiftX, shiftY, mDisplacementX, mDisplacementY)
    }
  }
  
  def testHovering(x: Float, y: Float, button_size: Float) {
    menus foreach (_.testHovering(x, y, button_size))
  }
}



/** Buttons that allows to move the rule. */
object MoveRuleButton extends MenuButton {
  import MenuOptions._
  
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    val selectedRule = gameEngine.selectedRule
    if(selectedRule != null) {
      selectedRule.x = Math.floor((selectedRule.x.toFloat + selectedRule.height/2)/selectedRule.height).toInt * selectedRule.height
      selectedRule.y = Math.floor((selectedRule.y.toFloat + selectedRule.width/2)/selectedRule.width).toInt * selectedRule.width
      hovered = false
    }
  }
  
  override def onFingerMove(gameEngine: GameEngine2DView, selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    val selectedRule = gameEngine.selectedRule
    if(selectedRule != null) {
      selectedRule.x = selectedRule.x + shiftX.toInt
      selectedRule.y = selectedRule.y + shiftY.toInt
    }
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) = List(R.drawable.cross_move)
}

/** Sends a rule to trash*/
object TrashRuleButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    val res = context.getResources()
    CustomDialogs.launchOKCancelDialog(context,
        String.format(res.getString(R.string.delete_title), "this rule"),
        res.getString(R.string.confirm_delete), false, { _ => gameEngine.getGame().deleteRule(gameEngine.selectedRule); gameEngine.selectRule(null)}, {_ => ()})
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameEngine2DView, selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: 
       R.drawable.trashcan ::  Nil
}

/** Sends a rule to trash*/
object EditRuleButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    ChooseExistingRuleButton.modifySpecificRule(gameEngine, gameEngine.getGame(), gameEngine.selectedRule)
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameEngine2DView, selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: 
       R.drawable.existing_rules ::  Nil
}
