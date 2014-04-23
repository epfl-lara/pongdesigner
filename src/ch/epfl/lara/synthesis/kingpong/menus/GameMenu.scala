package ch.epfl.lara.synthesis.kingpong.menus

import scala.collection.mutable.HashMap
import ch.epfl.lara.synthesis.kingpong.GameView
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.R
import android.graphics.Canvas
import android.graphics.drawable.Drawable
/*
/**
 * Menu to display game-related features.
 * Currently, the resize layout is implemented this way.
 */
object GameMenu extends MenuCenter {
  menus = List(GameLayoutButton)
  var coords = Array[Float](0, 0)
  def draw(canvas: Canvas, gameEngine: GameView, selectedShape: GameObject, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float): Unit = {
    GameLayoutButton.setPos(0, 0)
    
    coords(0) = gameEngine.getGame().layoutWidth
    coords(1) = gameEngine.getGame().layoutHeight
    gameEngine.mMatrix.mapPoints(coords)
        
    GameLayoutButton.draw(canvas, gameEngine, selectedShape, bitmaps, coords(0), coords(1))
  }
}

object GameLayoutButton extends MenuButton {
  import MenuOptions._
  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    if(hovered) {
      val game = gameEngine.getGame()
      val prevLayoutWidth = game.layoutWidth
      val prevLayoutHeight = game.layoutHeight
      def resizeShape(s: GameObject): Unit = {
        s match {
          case r: Rectangular if !s.isInstanceOf[Camera]=>
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
      }
      game.foreachVisibleshape(resizeShape(_))
      
      if(game.Camera.target == null && game.Camera.x + game.Camera.width == game.layoutWidth) {
        game.Camera.width += shiftX.toInt
      }
      if(game.Camera.target == null && game.Camera.y + game.Camera.height == game.layoutHeight) {
        game.Camera.height += shiftY.toInt
      }
      
      game.layoutWidth += shiftX.toInt
      game.layoutHeight += shiftY.toInt
      
    }
  }
  
  private val own_icon = List(R.drawable.cursor_square)
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = own_icon
  
  def hint_id = R.string.resize_layout_hint
}*/