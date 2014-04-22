package ch.epfl.lara.synthesis.kingpong.menus

import scala.collection.mutable.HashMap
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.objects._
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.drawable.Drawable
import android.content.Context
import org.jbox2d.dynamics.BodyType
import org.jbox2d.common.Vec2


object MenuOptions {
  /** Option indicating if the changes are on the shape and its previous state as well */
  var copy_to_prev = true
  /** Option indicating if the changes are made on the previous state or the current state. */
  var modify_prev = false
  
  /** Coordinates of the selected shape before moving. */
  var selected_shape_first_x = 0f
  var selected_shape_first_y = 0f
  
  /** Initialized at run time*/
  var context: Context = null
  
  /** Size of the button */
  var button_size = 25f
  
  var smallest_size = 0.05f
  def smallest_size2 = 2*smallest_size
}

/** Change the menu. */
object ShapeMenu extends MenuCenter {
  val basicMenu = List(MoveButton, PaintButton, PinButton, SizeButton, SpeedButton, VisibilityButton, IncrementButton, DecrementButton, ModifyTextButton, RenameButton, SystemButton, RotateButton)
  menus = basicMenu
  
  def draw(canvas: Canvas, gameEngine: GameView, selectedShape: GameObject, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float) = {
    for(m <- menus) { m.visible = true }
    val top_shift = selectedShape match {
      case d: Box[_] if d.className == "Box[Int]" =>
        IncrementButton.setPos(0, -1)
        DecrementButton.setPos(0, 1)
        ModifyTextButton.visible = false
        IncrementButton.visible = true
        DecrementButton.visible = true
        -1
      case d: Box[_] if d.className == "Box[String]" =>
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
    selectedShape match {
      /*case c: Camera =>
        PinButton.visible = false
        SpeedButton.visible = false
        PaintButton.visible = false
        TrashButton.visible = false
        VisibilityButton.visible = false*/
      case _ =>
        PinButton.visible = true
        SpeedButton.visible = true
        PaintButton.visible = true
        TrashButton.visible = true
        VisibilityButton.visible = true
    }
    MoveButton.setPos(0, 0)
    SpeedButton.setPos(1, top_shift)
    PinButton.setPos(2, top_shift)
    VisibilityButton.setPos(3, top_shift)
    SizeButton.setPos(1, 1)
    PaintButton.setPos(2, 1)
    SystemButton.setPos(3, 1)
    RotateButton.setPos(-1, 1)
    
    RenameButton.setText(selectedShape.name.get)
    RenameButton.setPos(gameEngine.whitePaint, 33f/49f, 0, top_shift-1)

    if(MoveButton.hovered) {
      for(m <- menus) { m.visible = false }
      MoveButton.visible = true
    } else if(SpeedButton.hovered) {
      for(m <- menus) { m.visible = false }
      //SpeedButton.visible = true
    } else if(SizeButton.hovered) {
      for(m <- menus) { m.visible = false }
      SizeButton.visible = true
    } else if(RotateButton.hovered) {
      for(m <- menus) { m.visible = false }
      RotateButton.visible = true
    } else if(PaintButton.hovered) {
      for(m <- menus) { m.visible = false }
      PaintButton.visible = true
    } else if(SystemButton.hovered) {
      for(m <- menus) { m.visible = false }
      SystemButton.visible = true
    }
    
    
    for(menu <- menus) {
      menu.draw(canvas, gameEngine, selectedShape, bitmaps, cx, cy)
    }
  }
}


/** Button to modify the text */
object ModifyTextButton extends MenuButton {
 import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    val res = context.getResources()
    selectedShape match {
      case d: Box[String] if d.className == "Box[String]" =>
        def updateText(s: String): Unit = {
          if(modify_prev) {
            d.value.set(s)
          } else {
            d.value.setNext(s)
          }
          if(copy_to_prev) {
            d.value.set(d.value.get)
          } else {
            //gameEngine.updateSelectedRule()
          }
        }
        CustomDialogs.launchChoiceDialogWithCustomchoice(context,
            String.format(res.getString(R.string.modify_text_title), d.value.next), R.array.text_possibilities,
            updateText(_), {() => }, if(modify_prev) d.value.get else d.value.next)
      case _ => 
    }
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }
  
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.modify_text ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.modify_text :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.change_text_hint
}


/** Buttons that allows to move the shape. */
object MoveButton extends MenuButton {
  import MenuOptions._
  
  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    if(selectedShape.noVelocity) {
      selectedShape match {
        case selectedShape: Movable =>
          if(modify_prev) {
            if(Math.abs(selectedShape.x.get - selected_shape_first_x) >= 10) selectedShape.x set Math.floor((selectedShape.x.get+smallest_size)/smallest_size2).toFloat * smallest_size2
            if(Math.abs(selectedShape.y.get - selected_shape_first_y) >= 10) selectedShape.y set Math.floor((selectedShape.y.get+smallest_size)/smallest_size2).toFloat * smallest_size2
          } else {
            if(Math.abs(selectedShape.x.next - selected_shape_first_x) >= 10) selectedShape.x setNext Math.floor((selectedShape.x.next+smallest_size)/smallest_size2).toFloat * smallest_size2
            if(Math.abs(selectedShape.y.next - selected_shape_first_y) >= 10) selectedShape.y setNext Math.floor((selectedShape.y.next+smallest_size)/smallest_size2).toFloat * smallest_size2  
          }
          if(copy_to_prev) {
            selectedShape.x set selectedShape.x.next
            selectedShape.y set selectedShape.y.next
          }
        case _ =>
      }
    }
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    if(selectedShape != null) {
      selectedShape match {
        case selectedShape: Movable =>
          if(modify_prev) {
            selectedShape.x set gameEngine.snapX(selected_shape_first_x + relativeX)
            selectedShape.y set gameEngine.snapY(selected_shape_first_y + relativeY)
          } else {
            selectedShape.x setNext gameEngine.snapX(selected_shape_first_x + relativeX)
            selectedShape.y setNext gameEngine.snapY(selected_shape_first_y + relativeY)
          }
          if(copy_to_prev) {
            selectedShape.x set selectedShape.x.next
            selectedShape.y set selectedShape.y.next
          }
        case _ =>
      }
    }
  }
  
  val private_icon = List(R.drawable.cross_move)
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = private_icon
  
  def hint_id = R.string.change_position_hint
}

/** Button to change the speed of the shape */
object SpeedButton extends MenuButton {
  import MenuOptions._  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    if(selectedShape.noVelocity) selectedShape.noVelocity = false
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    if(selectedShape != null && !selectedShape.noVelocity) {
      selectedShape match {
        case selectedShape: PhysicalObject =>
          if(modify_prev) {
            selectedShape.velocity set (selectedShape.velocity.get + Vec2(shiftX.toFloat, shiftY.toFloat))
          } else {
            selectedShape.velocity setNext (selectedShape.velocity.next + Vec2(shiftX.toFloat, shiftY.toFloat))
          }
          if(copy_to_prev) {
            selectedShape.velocity set selectedShape.velocity.next
          }
        case _ =>
      }
    }
  }
  
  private val velocityNoneList =  R.drawable.move_velocity :: noneList
  
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.move_velocity ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.move_velocity :: Nil
  private val hovered_icons_none = R.drawable.flat_button_highlighted :: velocityNoneList
  private val normal_icons_none = R.drawable.flat_button :: velocityNoneList
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) if(selectedShape.noVelocity) hovered_icons_none else hovered_icons else if(selectedShape.noVelocity) normal_icons_none else normal_icons)
  
  def hint_id = R.string.change_speed_hint
}

/** Button to change the speed of the shape */
object SizeButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape match {
      case c:Circle =>
        if(modify_prev) {
          c.radius set Math.floor((c.radius.get + smallest_size)/smallest_size2).toFloat * smallest_size2
        } else {
          c.radius setNext Math.floor((c.radius.next+smallest_size)/smallest_size2).toFloat * smallest_size2
        }
        if(copy_to_prev) {
          c.radius set c.radius.next
        }
      case r:ResizableRectangular =>
        if(modify_prev) {
          r.width set (Math.floor((r.width.get + smallest_size)/smallest_size2) * smallest_size2).toInt
          r.height set (Math.floor((r.height.get + smallest_size)/smallest_size2) * smallest_size2).toInt
        } else {
          r.width setNext (Math.floor((r.width.next + smallest_size)/smallest_size2) * smallest_size2).toInt
          r.height setNext (Math.floor((r.height.next + smallest_size)/smallest_size2) * smallest_size2).toInt
        }
        if(copy_to_prev) {
          r.width set r.width.next
          r.height set r.height.next
        }
      case _ =>
    }
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    if(selectedShape != null) {
      selectedShape match {
        case c:Circle =>
          if(modify_prev) {
            c.radius set Math.max(smallest_size, c.radius.get + shiftX)
          } else {
            c.radius setNext Math.max(smallest_size, c.radius.next + shiftX)
          }
          if(copy_to_prev) {
            c.radius set c.radius.next
          }
        case r:ResizableRectangular =>
          if(modify_prev) {
            r.width set Math.max(smallest_size, r.width.get + shiftX)
            r.height set Math.max(smallest_size, r.height.get + shiftY)
          } else {
            r.width setNext Math.max(smallest_size, r.width.next + shiftX)
            r.height setNext Math.max(smallest_size, r.height.next + shiftY)
          }
          if(copy_to_prev) {
            r.width set r.width.next
            r.height set r.height.next
          }
        case _ =>
      }
    }
  }
  
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.move_size ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.move_size :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.change_size_hint
}

/** Button to change the pin state of the shape */
object PinButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape.noVelocity = !selectedShape.noVelocity
    if(selectedShape.noVelocity) {
      selectedShape match {
        case selectedShape: PhysicalObject =>
          if(modify_prev) {
            selectedShape.velocity set Vec2(0,0)
          } else {
            selectedShape.velocity setNext Vec2(0,0)
          }
          if(copy_to_prev) {
            selectedShape.velocity set selectedShape.velocity.next
          }
        case _ =>
      }
    }
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Do nothing
  }
  
  private val nailNoneList = R.drawable.nail :: noneList
       
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.nail ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.nail :: Nil
  private val hovered_icons_none = R.drawable.flat_button_highlighted :: nailNoneList
  private val normal_icons_none = R.drawable.flat_button :: nailNoneList
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) if(!selectedShape.noVelocity) hovered_icons_none else hovered_icons else if(!selectedShape.noVelocity) normal_icons_none else normal_icons)
  
  def hint_id = R.string.change_nail_hint
}

/** Button to change the color of the shape */
object PaintButton extends MenuButton {
  import MenuOptions._

  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    // Nothing to declare
    ColorMenu.onFingerUp(gameEngine, selectedShape, x, y)
    hovered = false
    ColorMenu.activated = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    if(selectedShape != null) {
      ColorMenu.activated = true
      ColorMenu.onFingerMove(gameEngine, selectedShape, relativeX, relativeY, shiftX, shiftY, mDisplacementX, mDisplacementY)
    }
  }
  
  override def testHovering(atX: Float, atY: Float, button_size: Float): Boolean = {
    super.testHovering(atX, atY, button_size)
    if(hovered) {
      ColorMenu.activated = true
    }
    hovered
  }
  
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.menu_paint ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.menu_paint :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.change_paint_hint
}

object SystemButton extends MenuButton {
  import MenuOptions._

  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    // Nothing to declare
    SystemMenu.onFingerUp(gameEngine, selectedShape, x, y)
    hovered = false
    SystemMenu.activated = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    if(selectedShape != null) {
      SystemMenu.activated = true
      SystemMenu.onFingerMove(gameEngine, selectedShape, relativeX, relativeY, shiftX, shiftY, mDisplacementX, mDisplacementY)
    }
  }
  
  override def testHovering(atX: Float, atY: Float, button_size: Float): Boolean = {
    super.testHovering(atX, atY, button_size)
    if(hovered) {
      SystemMenu.activated = true
    }
    hovered
  }
  
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.gear ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.gear :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.system_property_hint
}

/** Changes the visibility of a shape */
object VisibilityButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape match {
      case selectedShape: Visiblable =>
        if(MenuOptions.modify_prev) {
          selectedShape.visible set !selectedShape.visible.get
        } else {
          selectedShape.visible setNext !selectedShape.visible.next
        }
        if(copy_to_prev) {
          selectedShape.visible set selectedShape.visible.next
        }
        hovered = false
      case _ =>
    }

  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }

  private val eyeNoneList = R.drawable.eye :: noneList
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.eye ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.eye :: Nil
  private val hovered_icons_none = R.drawable.flat_button_highlighted :: eyeNoneList
  private val normal_icons_none = R.drawable.flat_button :: eyeNoneList
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) if(!selectedShape.visible.next) hovered_icons_none else hovered_icons else if(!selectedShape.visible.next) normal_icons_none else normal_icons)
  
  def hint_id = R.string.change_visible_hint
}

/** Button to increment a number */
object IncrementButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape match {
        case d:Box[Int] if d.className == "Box[Int]" =>
          val bothShouldChange = false //(gameEngine.selectedEvent != null && gameEngine.selectedEvent.value.shape1 == selectedShape)
          if(MenuOptions.modify_prev && !bothShouldChange) {
            d.value set d.value.get + 1
          } else {
            d.value setNext d.value.next + 1
          }
          if(copy_to_prev || bothShouldChange) {
            d.value set d.value.next
          } else {
            /*if(EditRuleButton.selected && MenuOptions.modify_prev) {
              
            }*/
          }
          //d.setChanged("value", (d.value.get != d.value))
        case _ =>
    }
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }
  
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.flat_button_p1 ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.flat_button_p1 :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.change_increment_hint
}

/** Button to decrement a number */
object DecrementButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape match {
      case d:Box[Int] if d.className == "Box[Int]" =>
        val bothShouldChange = false //(gameEngine.selectedEvent != null && gameEngine.selectedEvent.value.shape1 == selectedShape)
        if(MenuOptions.modify_prev && !bothShouldChange) {
          d.value.set(d.value.get - 1)
        } else {
          d.value.setNext(d.value.get - 1)
        }
        if(copy_to_prev || bothShouldChange) {
          d.value.set(d.value.next)
        } else {
        }
      case _ =>
    }
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }
  
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.flat_button_m1 ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.flat_button_m1 :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.change_decrement_hint
}

/** Button to rename the shape */
object RenameButton extends MenuTextButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    val res = context.getResources()
    var array = selectedShape match {
        case r:Rectangle => R.array.rename_rectangles
        case r:Box[String] if r.className == "Box[String]" => R.array.rename_textbox
        case r:Box[Int] if r.className ==  "Box[Int]" => R.array.rename_integerbox
        case r:Circle => R.array.rename_circles
        case _ => R.array.rename_circles
      }
      CustomDialogs.launchChoiceDialogWithCustomchoice(context, String.format(res.getString(R.string.rename_title), selectedShape.name.get), array, gameEngine.shapeEditor.renameSelectedShape(_), {() => })
      hovered = false
    hovered = false
  }

  private val hovered_icons = R.drawable.flat_button_resizable_highlighted ::  Nil
  private val normal_icons = R.drawable.flat_button_resizable :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.change_name_hint
}


/** Button to change the speed of the shape */
object RotateButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape match {
      case c:Rotationable =>
        if(modify_prev) {
          c.angle set Math.floor((c.angle.get + 7.5f)/15).toFloat * 15
        } else {
          c.angle setNext Math.floor((c.angle.next+7.5f)/15).toFloat * 15
        }
        if(copy_to_prev) {
          c.angle set c.angle.next
        }
      case _ =>
    }
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    if(selectedShape != null) {
      selectedShape match {
        case c:Rotationable =>
          if(modify_prev) {
            c.angle set (Math.atan2(relativeX, -relativeY).toFloat)
          } else {
            c.angle setNext (Math.atan2(relativeX, -relativeY).toFloat)
          }
          if(copy_to_prev) {
            c.angle set c.angle.next
          }
        case _ =>
      }
    }
  }
  
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.move_rotate ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.move_rotate :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.change_rotate_hint
}
