package ch.epfl.lara.synthesis.kingpong.menus


import scala.collection.mutable.HashMap
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong._
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.drawable.Drawable
import android.content.Context
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression._
//import ch.epfl.lara.synthesis.kingpong.ast.Expr
//import ch.epfl.lara.synthesis.kingpong.ast.Action
//import ch.epfl.lara.synthesis.kingpong.ast.EConstantNumber
import android.graphics.RectF
import android.graphics.Rect
import android.graphics.Paint
import ch.epfl.lara.synthesis.kingpong.objects._

object SystemMenu extends MenuCenter {
  var activated = false
  
   menus = List(TrashButton, FixButton)
  
  def draw(canvas: Canvas, gameEngine: GameView, selectedShape: GameObject, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float): Unit = {
    //RenameButtonRule.setText(selectedShape.mName)
    //RenameButtonRule.setPos(gameEngine.whitePaint, 33f/49f, 0, top_shift-1)
    Menus.spaceMenusOnCircle(menus)
    for(menu <- menus) {
      menu.draw(canvas, gameEngine, selectedShape, bitmaps, cx, cy)
    }
  }
  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    super.onFingerUp(gameEngine, selectedShape, x, y)
  }
}

/** Sends a shape to trash
 *  TODO : This should demonstrate advanced functionality (aka: deletion is a property)
 **/
object TrashButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    if(modify_prev) { // Nothing to be done here
      //selectedShape.deletion_time set gameEngine.time
    } else {
      selectedShape.deletionTime setNext gameEngine.time.toInt
    }
    if(copy_to_prev) {
      selectedShape.deletionTime set selectedShape.deletionTime.next
    }
    
    val res = context.getResources()
    /*CustomDialogs.launchOKCancelDialog(context,
        String.format(res.getString(R.string.delete_title), selectedShape.name),
        res.getString(R.string.confirm_delete), false, { _ => selectedShape.delete(); gameEngine.shapeEditor.unselect()}, {_ => ()})*/
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }
 
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.trashcan ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.trashcan :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.change_trash_hint
}

/** Sends a shape to trash
 *  TODO : This should demonstrate advanced functionality (aka: deletion is a property)
 **/
object FixButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape.properties.foreach { p =>
      p match {
        case p: HistoricalProperty[_] =>
          p.setInit(Literal(p.get))
      }
    }
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }
 
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.back ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.back :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.change_back_hint
}