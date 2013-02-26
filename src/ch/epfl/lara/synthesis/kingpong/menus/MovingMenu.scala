package ch.epfl.lara.synthesis.kingpong.menus

import scala.collection.mutable.HashMap
import ch.epfl.lara.synthesis.kingpong.GameShapes._
import ch.epfl.lara.synthesis.kingpong._
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.drawable.Drawable
import android.content.Context

object MenuOptions {
  /** Option indicating if the changes are on the shape and its previous state as well */
  var copy_to_prev = false
  /** Option indicating if the changes are made on the previous state or the current state. */
  var modify_prev = false
  
  /** Coordinates of the selected shape before moving. */
  var selected_shape_first_x = 0f
  var selected_shape_first_y = 0f
  
  /** Initialized at run time*/
  var allColors: Array[String] = null
  
  /** Initialized at run time*/
  var context: Context = null
  
  /** Size of the button */
  var button_size = 25f
}

/** Change the menu. */
object MovingMenu {
  var menus: List[CustomMenu] = List(MoveButton, PaintButton, PinButton, SizeButton, SpeedButton, TrashButton, VisibilityButton, IncrementButton, DecrementButton, ModifyTextButton, RenameButton)
  def draw(canvas: Canvas, gameEngine: GameEngine2DView, selectedShape: Shape, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float): Int = {
    val top_shift = selectedShape match {
      case d: IntegerBox =>
        IncrementButton.setPos(0, -1)
        DecrementButton.setPos(0, 1)
        ModifyTextButton.visible = false
        IncrementButton.visible = true
        DecrementButton.visible = true
        -1
      case d: TextBox =>
        ModifyTextButton.setPos(0, -1)
        IncrementButton.visible = false
        DecrementButton.visible = false
        ModifyTextButton.visible = true
         -1
      case _ =>
        IncrementButton.visible = false
        DecrementButton.visible = false
        ModifyTextButton.visible = false
        0
    }
    MoveButton.setPos(0, 0)
    SpeedButton.setPos(1, top_shift)
    PinButton.setPos(2, top_shift)
    VisibilityButton.setPos(3, top_shift)
    SizeButton.setPos(1, 1)
    TrashButton.setPos(2, 1)
    PaintButton.setPos(3, 1)
    
    RenameButton.setText(selectedShape.mName)
    RenameButton.setPos(gameEngine.whitePaint, 33f/49f, 0, top_shift-1)

    for(menu <- menus) {
      menu.draw(canvas, gameEngine, selectedShape, bitmaps, cx, cy)
    }
    
    top_shift
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
  
  def onFingerMove(selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    menus foreach {
      menu =>
        if(menu.hovered) menu.onFingerMove(selectedShape, relativeX, relativeY, shiftX, shiftY, mDisplacementX, mDisplacementY)
    }
  }
  
  def testHovering(x: Float, y: Float, button_size: Float) {
    menus foreach (_.testHovering(x, y, button_size))
  }
}


/** Button to modify the text */
object ModifyTextButton extends MenuButton {
 import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    val res = context.getResources()
    selectedShape match {
      case d:TextBox =>
        def updateText(s: String): Unit = {
          if(modify_prev) {
            d.prev_text = s
          } else {
            d.text = s
          }
          if(copy_to_prev) {
            d.prev_text = d.text
          }
        }
        CustomDialogs.launchChoiceDialog(context,
            String.format(res.getString(R.string.modify_text_title), d.text), R.array.text_possibilities,
            updateText(_), {() => })
      case _ => 
    }
    hovered = false
  }
  
  override def onFingerMove(selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: 
       R.drawable.modify_text ::  Nil
}


/** Buttons that allows to move the shape. */
object MoveButton extends MenuButton {
  import MenuOptions._
  
  
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
  if(selectedShape.noVelocity) {
    // Snap to the grid.
      if(modify_prev) {
        if(Math.abs(selectedShape.prev_x - selected_shape_first_x) >= 10) selectedShape.prev_x = Math.floor((selectedShape.prev_x+2.5f)/5).toFloat * 5
        if(Math.abs(selectedShape.prev_y - selected_shape_first_y) >= 10) selectedShape.prev_y = Math.floor((selectedShape.prev_y+2.5f)/5).toFloat * 5   
      } else {
        if(Math.abs(selectedShape.x - selected_shape_first_x) >= 10) selectedShape.x = Math.floor((selectedShape.x+2.5f)/5).toFloat * 5
        if(Math.abs(selectedShape.y - selected_shape_first_y) >= 10) selectedShape.y = Math.floor((selectedShape.y+2.5f)/5).toFloat * 5          
      }
      if(copy_to_prev) {
        selectedShape.prev_x = selectedShape.x
        selectedShape.prev_y = selectedShape.y
      }
    }
    hovered = false
  }
  
  override def onFingerMove(selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    if(selectedShape != null) {
      if(modify_prev) {
        selectedShape.prev_x = selected_shape_first_x + relativeX
        selectedShape.prev_y = selected_shape_first_y + relativeY
      } else {
        selectedShape.x = selected_shape_first_x + relativeX
        selectedShape.y = selected_shape_first_y + relativeY
      }
      if(copy_to_prev) {
        selectedShape.prev_x = selectedShape.x
        selectedShape.prev_y = selectedShape.y
      }
    }
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) = List(R.drawable.cross_move)
}

/** Button to change the speed of the shape */
object SpeedButton extends MenuButton {
  import MenuOptions._  
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    if(selectedShape.noVelocity) selectedShape.noVelocity = false
    hovered = false
  }
  
  override def onFingerMove(selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    if(selectedShape != null && !selectedShape.noVelocity) {
      if(modify_prev) {
        selectedShape.prev_velocity_x += shiftX.toFloat / 1000f
        selectedShape.prev_velocity_y += shiftY.toFloat / 1000f
      } else {
        selectedShape.velocity_x += shiftX.toFloat / 1000f
        selectedShape.velocity_y += shiftY.toFloat / 1000f
      }
      if(copy_to_prev) {
        selectedShape.prev_velocity_x = selectedShape.velocity_x
        selectedShape.prev_velocity_y = selectedShape.velocity_y
      }
    }
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) ::
    R.drawable.move_velocity :: (if(selectedShape.noVelocity) R.drawable.none::Nil else Nil)
}

/** Button to change the speed of the shape */
object SizeButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    selectedShape match {
      case c:Circle =>
        if(modify_prev) {
          c.prev_radius = Math.floor((c.prev_radius + 2.5f)/5).toFloat * 5
        } else {
          c.radius = Math.floor((c.radius+2.5f)/5).toFloat * 5
        }
        if(copy_to_prev) {
          c.prev_radius = c.radius
        }
      case r:Rectangular =>
        if(modify_prev) {
          r.prev_width = (Math.floor((r.prev_width + 2.5f)/5) * 5).toInt
          r.prev_height = (Math.floor((r.prev_height + 2.5f)/5) * 5).toInt
        } else {
          r.width = (Math.floor((r.width + 2.5f)/5) * 5).toInt
          r.height = (Math.floor((r.height + 2.5f)/5) * 5).toInt
        }
        if(copy_to_prev) {
          r.prev_width = r.width
          r.prev_height = r.height
        }
      case _ =>
    }
    hovered = false
  }
  
  override def onFingerMove(selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    if(selectedShape != null) {
      selectedShape match {
        case c:GameShapes.Circle =>
          if(modify_prev) {
            c.prev_radius = Math.max(10, c.prev_radius + shiftX)
          } else {
            c.radius = Math.max(10, c.radius + shiftX)
          }
          if(copy_to_prev) {
            c.prev_radius = c.radius
          }
        case r:GameShapes.Rectangular =>
          if(modify_prev) {
            r.prev_width = Math.max(10, r.prev_width + shiftX.toInt)
            r.prev_height = Math.max(10, r.prev_height + shiftY.toInt)
          } else {
            r.width = Math.max(10, r.width + shiftX.toInt)
            r.height = Math.max(10, r.height + shiftY.toInt)
          }
          if(copy_to_prev) {
            r.prev_width = r.width
            r.prev_height = r.height
          }
        case _ =>
      }
    }
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) = (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: List(R.drawable.move_size)
}

/** Button to change the pin state of the shape */
object PinButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    selectedShape.noVelocity = !selectedShape.noVelocity
    if(selectedShape.noVelocity) {
      if(modify_prev) {
        selectedShape.prev_velocity_x = 0
        selectedShape.prev_velocity_y = 0
      } else {
        selectedShape.velocity_x = 0
        selectedShape.velocity_y = 0
      }
      if(copy_to_prev) {
        selectedShape.prev_velocity_x = selectedShape.velocity_x
        selectedShape.prev_velocity_y = selectedShape.velocity_y
      }
    }
    hovered = false
  }
  
  override def onFingerMove(selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Do nothing
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: 
       R.drawable.nail :: (if(!selectedShape.noVelocity) R.drawable.none::Nil else Nil)

}

/** Button to change the color of the shape */
object PaintButton extends MenuButton {
  import MenuOptions._

  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    // Nothing to declare
    hovered = false
  }
  
  // TODO : Make a better function to be able to choose the color
  override def onFingerMove(selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    if(selectedShape != null) {
      val selectedColor:Int = ((mDisplacementX / (button_size/3)).toInt % 6 + 6) % 6
      if(MenuOptions.modify_prev) {
        selectedShape.prev_color = Color.parseColor(allColors(selectedColor))
      } else {
        selectedShape.color = Color.parseColor(allColors(selectedColor))
      }
      if(copy_to_prev) {
        selectedShape.prev_color = selectedShape.color
      }
    }
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: 
       R.drawable.menu_paint ::  Nil

}

/** Sends a shape to trash*/
object TrashButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    val res = context.getResources()
    CustomDialogs.launchOKCancelDialog(context,
        String.format(res.getString(R.string.delete_title), selectedShape.name),
        res.getString(R.string.confirm_delete), false, { _ => selectedShape.delete(); gameEngine.selectShape(null)}, {_ => ()})
    hovered = false
  }
  
  override def onFingerMove(selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: 
       R.drawable.trashcan ::  Nil
}

/** Changes the visibility of a shape */
object VisibilityButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    if(MenuOptions.modify_prev) {
      selectedShape.prev_visible = !selectedShape.prev_visible
    } else {
      selectedShape.visible = !selectedShape.visible
    }
    if(copy_to_prev) {
      selectedShape.prev_visible = selectedShape.visible
    }
    hovered = false
  }
  
  override def onFingerMove(selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: 
       R.drawable.eye :: (if(!selectedShape.visible) R.drawable.none::Nil else Nil)
}

/** Button to increment a number */
object IncrementButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    selectedShape match {
        case d:IntegerBox =>
          if(MenuOptions.modify_prev) {
            d.prev_value = d.prev_value + 1
          } else {
            d.value = d.value + 1
          }
          if(copy_to_prev) {
            d.prev_value = d.value
          } else {
            /*if(integerBoxWatched != null && integerBoxWatched == d) {
              game.restorePrev(integerBoxWatched)
              integerEvent.x2 = d.prev_value
              integerEvent.y2 = d.value
              triggerRule(selectedRule, integerEvent)
            }*/
          }
        case _ =>
    }
    hovered = false
  }
  
  override def onFingerMove(selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: 
       R.drawable.flat_button_p1 ::  Nil
}

/** Button to decrement a number */
object DecrementButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    selectedShape match {
      case d:IntegerBox =>
        if(MenuOptions.modify_prev) {
          d.prev_value = d.prev_value - 1
        } else {
          d.value = d.value - 1
        }
        if(copy_to_prev) {
          d.prev_value = d.value
        } else {
          /*if(integerBoxWatched != null && integerBoxWatched == d) {
            game.restorePrev(integerBoxWatched)
            integerEvent.x2 = d.prev_value
            integerEvent.y2 = d.value
            triggerRule(selectedRule, integerEvent)
          }*/
        }
      case _ =>
    }
    hovered = false
  }
  
  override def onFingerMove(selectedShape: Shape, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: 
       R.drawable.flat_button_m1 ::  Nil
}

/** Button to rename the shape */
object RenameButton extends MenuTextButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameEngine2DView, selectedShape: Shape, x: Float, y: Float) = {
    val res = context.getResources()
    var array = selectedShape match {
        case r:Rectangle => R.array.rename_rectangles
        case r:TextBox => R.array.rename_textbox
        case r:IntegerBox => R.array.rename_integerbox
        case r:Circle => R.array.rename_circles
        case _ => R.array.rename_circles
      }
      CustomDialogs.launchChoiceDialog(context, String.format(res.getString(R.string.rename_title), selectedShape.mName), array, gameEngine.renameSelectedShape(_), {() => })
      hovered = false
    hovered = false
  }
  
  def icons(gameEngine: GameEngine2DView, selectedShape: Shape) =
    (if(hovered) R.drawable.flat_button_resizable_highlighted else R.drawable.flat_button_resizable) :: 
       R.drawable.flat_button_m1 ::  Nil
}


