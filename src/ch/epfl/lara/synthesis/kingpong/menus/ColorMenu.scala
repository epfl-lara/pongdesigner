package ch.epfl.lara.synthesis.kingpong.menus


import scala.collection.mutable.HashMap
import ch.epfl.lara.synthesis.kingpong.GameShapes._
import ch.epfl.lara.synthesis.kingpong._
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.drawable.Drawable
import android.content.Context

object ColorMenu {
  var menus: List[CustomMenu] = List()
  def draw(canvas: Canvas, gameEngine: GameEngine2DView, selectedShape: Shape, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float): Unit = {
    
    //RenameButtonRule.setText(selectedShape.mName)
    //RenameButtonRule.setPos(gameEngine.whitePaint, 33f/49f, 0, top_shift-1)
    val n = menus.size
    var i = 0
    val radius = n/6.0f
    for(menu <- menus) {
      val angle = (2*Math.PI * i) / n;
      menu.setPos(radius * Math.cos(angle).toFloat, radius * Math.sin(angle).toFloat)
      i += 1
    }
    for(menu <- menus) {
      menu.draw(canvas, gameEngine, selectedShape, bitmaps, PaintButton.getX(), PaintButton.getY())
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
  
  def createMenuFromColorArray(c: Context, i: Int) = {
    val color_array = c.getResources().getStringArray(i)
    menus = color_array.toList map { color: String => 
      val result = new ColorCircleMenu()
      result.setColor(Color.parseColor(color))
      result
    }
  }
}