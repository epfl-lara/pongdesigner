package ch.epfl.lara.synthesis.kingpong.menus

import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.GameShapes._
import scala.collection.immutable.List
import android.graphics.RectF
import android.graphics.Rect
import android.graphics.Paint
import android.graphics.drawable.Drawable
import android.graphics.Canvas
import android.content.Context
import scala.collection.mutable.HashMap
import android.graphics.drawable.NinePatchDrawable

object Menus {
  def recoverDrawables(c: Context) = {
    
  }
}

/*class Menus {
  private var mMenus:List[Menu] = Nil;
  def addMenu(m: Menu) {
    mMenus = m::mMenus
  }
}*/

/**
 * A menu button that can be displayed on the screen
 */
trait CustomMenu {
  // Computed absolute position of the center of the menu
  protected var x: Float = 0f
  protected var y: Float = 0f
  
  def getX() = x
  def getY() = y
  
  // Relative position from the center (0, 0) is the center.
  var dx: Float = 0
  var dy: Float = 0
  def setPos(new_dx: Float, new_dy: Float) = {
    dx = new_dx
    dy = new_dy
  }
  /** Marks the button as hovered if the finger is on it */
  def testHovering(atX: Float, atY: Float, button_size: Float): Boolean
  /** Called whenever a finger up is selected */
  def onFingerUp(gameEngine: GameEngine2DView, selectedShape: GameShapes.Shape, x: Float, y: Float) = {
  }
  def onFingerMove(gameEngine: GameEngine2DView, selectedShape: GameShapes.Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = { 
  }
  def draw(canvas: Canvas, gameEngine: GameEngine2DView, selectedShape: Shape, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float): Unit
  var hovered = false
  var visible = true
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape): List[Int]
}

/**
 * A simple menu button
 **/
abstract class MenuButton extends CustomMenu {
  import MenuOptions._
  var rectData = new Rect(0, 0, 0, 0)
  var rectFData = new RectF(0, 0, 0, 0)
  def draw(canvas: Canvas, gameEngine: GameEngine2DView, selectedShape: Shape, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float) = {
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
    }
  }
  def testHovering(atX: Float, atY: Float, button_size: Float): Boolean = {
    val t = button_size/2
    hovered = visible && atX >= x - t && atX <= x + t && atY >= y - t && atY <= y + t
    hovered
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

  def draw(canvas: Canvas, gameEngine: GameEngine2DView, selectedShape: Shape, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float) = {
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
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: GameShapes.Shape, x: Float, y: Float) = {
    // Do nothing
  }
  override def onFingerMove(gameEngine: GameEngine2DView, selectedShape: GameShapes.Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = { 
    if(hovered) {
      if(MenuOptions.modify_prev) {
        selectedShape.prev_color = color
      } else {
        selectedShape.color = color
      }
      if(copy_to_prev) {
        selectedShape.prev_color = selectedShape.color
      }
    }
  }
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: Nil
}

/**
 * A menu button displaying text
 */
abstract class MenuTextButton extends CustomMenu {
  import MenuOptions._
  var text: String = null
  var mRectDataButton  = new Rect(0, 0, 0, 0)
  var mTextX: Float = 0
  var mTextY: Float = 0
  
  def testHovering(atX: Float, atY: Float, button_size: Float): Boolean = {
    hovered = visible && left < atX && atX < right &&
              top < atY && atY < bottom
    hovered 
  }
  
  def setText(t: String) =  if(t != null) text = t
  
  var paint: Paint = null
  def setPos(new_paint: Paint, size_text: Float, new_dx: Float, new_dy: Float) = {
    if(text != null) {
      new_paint.setTextSize(size_text * button_size)
      new_paint.getTextBounds (text, 0, text.length, rectData)
      paint = new_paint
      dx = new_dx
      dy = new_dy
      
    }
  }
 
  def left = mRectDataButton.left
  def right = mRectDataButton.right
  def top = mRectDataButton.top
  def bottom = mRectDataButton.bottom
  var rectData = new Rect(0, 0, 0, 0)
  var rectFData = new RectF(0, 0, 0, 0)

  def draw(canvas: Canvas, gameEngine: GameEngine2DView, selectedShape: Shape, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float) = {
    if(visible) {
      x = cx + button_size * dx
      y = cy + button_size * dy
      val width = rectData.right - rectData.left
      val height = rectData.bottom - rectData.top
      val left = rectData.left
      val top = rectData.top
      val bottom = rectData.bottom
      rectFData.set(x- 32 - width / 2, y - height/2 - 16, x + width / 2 + 32, y+ height/2 + 16)
      rectFData.round(mRectDataButton)
      
      mTextX = x - width / 2 - left
      mTextY = y + height / 2 - bottom
      
      icons(gameEngine, selectedShape) foreach { id =>
        val d = bitmaps.getOrElse(R.drawable.flat_button_resizable, null).asInstanceOf[NinePatchDrawable]
        d.setBounds(mRectDataButton)
        d.draw(canvas)
      }
      if(paint != null && text != null) {
        canvas.drawText(text, mTextX, mTextY, paint)      
      }
    }
  }
}