package ch.epfl.lara.synthesis.kingpong.menus

import scala.collection.mutable.HashMap
import ch.epfl.lara.synthesis.kingpong.GameEngine2DView
import ch.epfl.lara.synthesis.kingpong.GameShapes._
import ch.epfl.lara.synthesis.kingpong.R
import android.graphics.Canvas
import android.graphics.drawable.Drawable

object GameMenu {
  var menus: List[CustomMenu] = List(GameLayoutButton)
  var coords = Array[Float](0, 0)
  def draw(canvas: Canvas, gameEngine: GameEngine2DView, selectedShape: Shape, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float): Unit = {
    GameLayoutButton.setPos(0, 0)
    
    coords(0) = gameEngine.getGame().layoutWidth
    coords(1) = gameEngine.getGame().layoutHeight
    gameEngine.mMatrix.mapPoints(coords)
        
    GameLayoutButton.draw(canvas, gameEngine, selectedShape, bitmaps, coords(0), coords(1))

    /*for(menu <- menus) {
      
      
    }*/
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
  
  def testHovering(x: Float, y: Float, button_size: Float) = {
    menus foreach (_.testHovering(x, y, button_size))
  }
}

object GameLayoutButton extends MenuButton {
  import MenuOptions._
  
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameEngine2DView, selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    if(hovered) {
      val prevLayoutWidth = gameEngine.getGame().layoutWidth
      val prevLayoutHeight = gameEngine.getGame().layoutHeight
      gameEngine.getGame().foreachVisibleshape({s: Shape =>
        s match {
          case r: Rectangular =>
            if(Math.round(r.x + r.width - prevLayoutWidth) == 0) {
              r.x += shiftX.toInt
              if(MenuOptions.copy_to_prev) r.prev_x = r.x
            }
            if(Math.round(r.x + r.width + r.height - prevLayoutWidth) == 0) {
              r.width += shiftX.toInt
              if(MenuOptions.copy_to_prev) r.prev_width = r.width
            }
            if(Math.round(r.y + r.height - prevLayoutHeight) == 0) {
              if(r.y == 0) {
                r.height += shiftY.toInt
                if(MenuOptions.copy_to_prev) r.prev_height = r.height
              } else {
                r.y += shiftY.toInt
                if(MenuOptions.copy_to_prev) r.prev_y = r.y
              }
            }
            if(Math.round(r.y + r.height + r.width - prevLayoutHeight) == 0) {
              r.height += shiftY.toInt
              if(MenuOptions.copy_to_prev) r.prev_height = r.height
            }
          case _ =>
        }
      })
      gameEngine.getGame().layoutWidth += shiftX.toInt
      gameEngine.getGame().layoutHeight += shiftY.toInt
    }
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) = List(R.drawable.cursor_square)
}