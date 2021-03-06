package ch.epfl.lara.synthesis.kingpong.menus

import scala.collection.mutable.HashMap

import android.content.Context
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.RectF
import android.graphics.Rect
import android.graphics.Paint
import android.graphics.drawable.Drawable

import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.objects._

object ColorMenu extends MenuCenter {
  var activated = false
  
  var registeredExpr: Option[Expr] = None
  var registeredRule: Option[Expr] = None
  var registeredAction: Option[Action] = None
  
  def draw(canvas: Canvas, gameEngine: GameView, selectedShape: GameObject, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float): Unit = {
    //RenameButtonRule.setText(selectedShape.mName)
    //RenameButtonRule.setPos(gameEngine.whitePaint, 33f/49f, 0, top_shift-1)
    Menus.spaceMenusOnCircle(canvas, cx, cy, menus)
    for(menu <- menus) {
      menu.draw(canvas, gameEngine, selectedShape, bitmaps, cx, cy)
    }
  }
  
  def createMenuFromColorArray(c: Context, i: Int) = {
    val color_array = c.getResources().getStringArray(i)
    menus = color_array.toList map { color: String => 
      val result = new ColorCircleMenu()
      result.setColor(Color.parseColor(color))
      result
    }
    menus = new ImagePickerMenu()::menus
  }
  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    registeredExpr = None
    registeredAction = None// Wait for hovering the corresponding menu.
    super.onFingerUp(gameEngine, selectedShape, x, y)
  }
}


/**
 * Color menus
 */
class ColorCircleMenu extends CustomMenu {
  import MenuOptions._
  private var color: Int = 0
  var rectData = new Rect(0, 0, 0, 0)
  var rectFData = new RectF(0, 0, 0, 0)
  var paint = new Paint()
  paint.setStyle(Paint.Style.FILL_AND_STROKE)
  paint.setAntiAlias(true)

  def draw(canvas: Canvas, gameEngine: GameView, selectedShape: GameObject, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float) = {
    x = cx + button_size * dx
    y = cy + button_size * dy
    if(visible) {
      icons(gameEngine, selectedShape) foreach {
        id => val d = bitmaps.getOrElse(id, null)
        if(d!= null) {
          rectFData.set(x - button_size/2, y - button_size/2, x + button_size/2, y + button_size/2)
          rectFData.round(rectData)
          d.setBounds(rectData)
          d.draw(canvas)
        }
      }
      paint.setColor(color)
      canvas.drawCircle(x, y, button_size/2.3f, paint)
    }
  }
  def setColor(c: Int) = color = c
  def testHovering(atX: Float, atY: Float, button_size: Float): Boolean = {
    val t = button_size/2
    hovered = (x-atX)*(x-atX) + (y-atY)*(y-atY) < t*t 
    hovered
  }
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    hovered = false
  }
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = { 
    if(hovered) {
      if(selectedShape != null) {
        selectedShape match {
          case selectedShape: Colorable =>
            selectedShape.color.setPrevNext(color)
          case _ =>
        }
      } else {
          ColorMenu.registeredExpr match {
            case Some(p@IntegerLiteral(i)) if i > 0x10000/* == Expr.Subtype.COLOR_SUBTYPE*/ => // heuristic to know if it is a color
              //p.value = color
              //if(gameEngine.ruleEditor.selectedRule != null) gameEngine.ruleEditor.selectedRule.execute(gameEngine.getGame().context, false)
            case _ => 
          }
        }
    }
  }
  
  private val hovered_icons = R.drawable.bm_flat_button_highlighted ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons
  
  def hint_id = R.string.change_paint_hint
}

/**
 * Image picker menus
 */
class ImagePickerMenu extends MenuButton {
  import MenuOptions._
  var paint = new Paint()
  paint.setStyle(Paint.Style.FILL_AND_STROKE)
  paint.setAntiAlias(true)

  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    // Do nothing
    if(hovered) {
      gameEngine.pickImage()
      hovered = false
    }
  }
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = { 
    if(hovered) {
      if(selectedShape != null) {
        selectedShape match {
          case selectedShape: Colorable =>
          case _ =>
        }
      } else {
          ColorMenu.registeredExpr match {
            case Some(p@IntegerLiteral(i)) if i > 0x10000/* == Expr.Subtype.COLOR_SUBTYPE*/ => // heuristic to know if it is a color
              //p.value = color
              //if(gameEngine.ruleEditor.selectedRule != null) gameEngine.ruleEditor.selectedRule.execute(gameEngine.getGame().context, false)
            case _ => 
          }
        }
    }
  }
  
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_jpeg ::   Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_jpeg :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons
  
  def hint_id = R.string.change_paint_hint
}

