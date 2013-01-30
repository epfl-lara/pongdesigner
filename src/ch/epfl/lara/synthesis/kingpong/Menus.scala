package ch.epfl.lara.synthesis.kingpong

import scala.collection.immutable.List
import android.graphics.RectF
import android.graphics.Rect
import android.graphics.Paint

object Menus {
  var rectData = new Rect(0, 0, 0, 0)
  var rectFData = new RectF(0, 0, 0, 0)
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
  /** Marks the button as hovered if the finger is on it */
  def testHovering(atX: Float, atY: Float, button_size: Float): Boolean
  /** Called whenever a finger up is selected */
  def onFingerUp(selectedShape: GameShapes.Shape, x: Float, y: Float) = {
  }
  var hovered = false
  var visible = true
}

/**
 * A simple menu button
 **/
class MenuButton(var x: Float, var y: Float) extends CustomMenu {
  def testHovering(atX: Float, atY: Float, button_size: Float): Boolean = {
    val t = button_size/2
    hovered = visible && atX >= x - t && atX <= x + t && atY >= y - t && atY <= y + t
    hovered
  }
}
/**
 * A menu button displaying text
 */
class MenuTextButton(var text: String) extends CustomMenu {
  import Menus._
  var mRectDataButton  = new Rect(0, 0, 0, 0)
  var mTextX: Float = 0
  var mTextY: Float = 0
  
  def testHovering(atX: Float, atY: Float, button_size: Float): Boolean = {
    hovered = visible && left < atX && atX < right &&
              top < atY && atY < bottom
    hovered 
  }
  
  def setTextAndCenter(t: String, paint: Paint, size: Float, center_x: Float, center_y: Float) = {
    if(t != null) text = t
    paint.setTextSize(size)
    paint.getTextBounds (text, 0, text.length, rectData)
    val width = rectData.right - rectData.left
    val height = rectData.bottom - rectData.top
    val left = rectData.left
    val top = rectData.top
    val bottom = rectData.bottom
    rectFData.set(center_x - 32 - width / 2, center_y - height - 32, center_x + width / 2 + 32, center_y)
    rectFData.round(mRectDataButton)
    
    mTextX = center_x - width / 2 - left
    mTextY = center_y - bottom - 16
  }
  def setTextAndTopLeft(t: String, paint: Paint, size: Float, nleft: Float, ntop: Float) = {
    if(t != null) text = t
    paint.setTextSize(size)
    paint.getTextBounds (text, 0, text.length, rectData)
    
    val width = rectData.right - rectData.left
    val height = rectData.bottom - rectData.top
    val left = rectData.left
    val top = rectData.top
    val bottom = rectData.bottom
    rectFData.set(nleft, ntop, nleft + width + 64, ntop + height + 32)
    rectFData.round(mRectDataButton)

    mTextX = nleft + 32 - left
    mTextY = ntop + height + 16 - bottom
  }
  def left = mRectDataButton.left
  def right = mRectDataButton.right
  def top = mRectDataButton.top
  def bottom = mRectDataButton.bottom
  
}