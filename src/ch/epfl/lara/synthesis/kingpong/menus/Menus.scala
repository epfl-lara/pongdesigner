package ch.epfl.lara.synthesis.kingpong.menus

import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import scala.collection.immutable.List
import android.graphics.RectF
import android.graphics.Rect
import android.graphics.Paint
import android.graphics.drawable.Drawable
import android.graphics.Canvas
import android.content.Context
import scala.collection.mutable.HashMap
import android.graphics.drawable.NinePatchDrawable
import android.content.res.Resources
import org.jbox2d.common.Vec2

object Menus {
  def recoverDrawables(c: Context) = {
    
  }
  
  def spaceMenusOnCircle(menus: List[CustomMenu]) = {
    val n = menus.size
    var i = 0
    var radius = 1.0//Math.max(1f, n/6f)
    var offsetAngle = 0.0
    for(menu <- menus) {
      val angle = (2*Math.PI * i) / 6 + offsetAngle;
      menu.setPos(radius.toFloat * Math.cos(angle).toFloat, radius.toFloat * Math.sin(angle).toFloat)
      i += 1
      if(i == 6) {
        radius = 1.732
        offsetAngle = 2*Math.PI/12
      }
      if(i == 12) {
        radius = 2.0
        offsetAngle = 0
      }
      if(i == 18) {
        radius = 2.645745
        offsetAngle = 0.333474
      }
      if(i == 24) {
        offsetAngle = 0.713724
      }
      if(i == 30) {
        radius = 3
        offsetAngle = 0
      }
      if(i >= 36) radius = 3+(i-36.0)/6 // Should not happen.
    }
  }
}

object MenuCenter {
  var registeredMenusCenters: List[MenuCenter] = Nil
  def addMenuCenter(c: MenuCenter) = registeredMenusCenters = c::registeredMenusCenters
}

/**
 * Contains the definition of menus tight together.
 */
abstract class MenuCenter {
  MenuCenter.addMenuCenter(this)
  var menus: List[CustomMenu] = Nil
  def draw(canvas: Canvas, gameEngine: GameView, selectedShape: GameObject, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float): Unit
  
  def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float): Boolean = { 
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
  
  def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    menus foreach {
      menu =>
        if(menu.hovered) menu.onFingerMove(gameEngine, selectedShape, relativeX, relativeY, shiftX, shiftY, mDisplacementX, mDisplacementY)
    }
  }
  
  def testHovering(x: Float, y: Float, button_size: Float): Boolean = {
    menus.foldLeft(false){(res, menu) => menu.testHovering(x, y, button_size) || res}
  }
  
  def cancelHovered() = {
    menus foreach((menu: CustomMenu) => menu.hovered = false)
  }
}

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
  def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
  }
  def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = { 
  }
  def draw(canvas: Canvas, gameEngine: GameView, selectedShape: GameObject, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float): Unit
  var hovered = false
  var visible = true
  protected val noneList =  R.drawable.none::Nil
  def icons(gameEngine: GameView, selectedShape: GameObject): List[Int]
  
  protected def hint_id: Int
  private var cached_hint: String = null
  
  def hint(res: Resources): String = {
    if(cached_hint == null) {
      cached_hint = res.getString(hint_id)
    }
    cached_hint
  }
}

trait Selectable {
  private var selected = false
  def toggleSelected() = {
    selected = !selected
    for(s <- exclusionList) {
      s.selected = false
    }
  }
  def exclusionList: List[Selectable]
  def isSelected = selected
  def unselect() = selected = false
}

/**
 * A simple menu button
 **/
abstract class MenuButton extends CustomMenu {
  import MenuOptions._
  var rectData = new Rect(0, 0, 0, 0)
  var rectFData = new RectF(0, 0, 0, 0)
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
    }
  }
  def testHovering(atX: Float, atY: Float, button_size: Float): Boolean = {
    val t = button_size/2
    hovered = visible && atX >= x - t && atX <= x + t && atY >= y - t && atY <= y + t
    hovered
  }
  def Vec2(x: Float, y: Float): Vec2 = new Vec2(x, y)
  implicit class Adder(c: Vec2) {
    def +(other: Vec2): Vec2 = Vec2(c.x + other.x, c.y + other.y)
  }
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

  def draw(canvas: Canvas, gameEngine: GameView, selectedShape: GameObject, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float) = {
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
        val d = bitmaps.getOrElse(id, null).asInstanceOf[NinePatchDrawable]
        if(d != null) {
          d.setBounds(mRectDataButton)
          d.draw(canvas)
        }
      }
      if(paint != null && text != null) {
        canvas.drawText(text, mTextX, mTextY, paint)      
      }
    }
  }
}