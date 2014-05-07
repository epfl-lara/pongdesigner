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
import scala.collection.mutable.ArrayBuffer


object MenuOptions {
  /** Option indicating if the changes are on the shape and its previous state as well */
  var copy_to_prev = true
  /** Option indicating if the changes are made on the previous state or the current state. */
  var modify_prev = false
  
  /** Coordinates of the selected shape before moving. */
  var selected_shape_first_x = 0f
  var selected_shape_first_y = 0f
  var selected_shape_first_radius = 0f
  var selected_shape_first_width = 0f
  var selected_shape_first_height = 0f
  var selected_shape_first_angle = 0f
  
  /** Initialized at run time*/
  var context: Context = null
  
  /** Size of the button */
  var button_size = 25f
  
  var smallest_size = 0.05f
  def smallest_size2 = 2*smallest_size
}

/** Change the menu. */
object ShapeMenu extends MenuCenter {
  val basicMenu = List(MoveButton, PaintButton, PinButton, SizeButton, SpeedButton, VisibilityButton, IncrementButton, DecrementButton, ModifyTextButton, RenameButton, SystemButton, RotateButton, ModifyLanguageButton)
  menus = basicMenu
  
  def draw(canvas: Canvas, gameEngine: GameView, selectedShape: GameObject, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float) = {
    for(m <- menus) { m.visible = true }
    ModifyLanguageButton.visible = false
    IncrementButton.visible = false
    DecrementButton.visible = false
    ModifyTextButton.visible = false
    val top_shift = selectedShape match {
      case d: IntBox =>
        IncrementButton.setPos(0, -1)
        DecrementButton.setPos(0, 1)
        IncrementButton.visible = true
        DecrementButton.visible = true
        -1
      case d: StringBox =>
        ModifyTextButton.setPos(0, -1)
        ModifyTextButton.visible = true
         -1
      case d: SoundTTS =>
        ModifyTextButton.setPos(0, -1)
        ModifyLanguageButton.setPos(-1, -1)
        ModifyTextButton.visible = true
        ModifyLanguageButton.visible = true
        -1
      case _ =>
        0
    }
    selectedShape match {
      case c: Rotationable =>
        RotateButton.visible = true
      case _ =>
        RotateButton.visible = false
    }
    selectedShape match {
      case c: PhysicalObject =>
        SpeedButton.visible = true
        PinButton.visible = true
      case _ =>
        SpeedButton.visible = false
        PinButton.visible = false
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
      case d: StringBox =>
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
            String.format(res.getString(R.string.modify_text_title), d.value.next), R.array.string_possibilities,
            updateText(_), {() => }, if(modify_prev) d.value.get else d.value.next)
      case d: SoundTTS =>
        def updateText(s: String): Unit = {
          if(modify_prev) {
            d.text.set(s)
          } else {
            d.text.setNext(s)
          }
          if(copy_to_prev) {
            d.text.set(d.text.get)
          } else {
            //gameEngine.updateSelectedRule()
          }
        }
        CustomDialogs.launchChoiceDialogWithCustomchoice(context,
            String.format(res.getString(R.string.modify_text_title), d.text.next), R.array.text_possibilities,
            updateText(_), {() => }, if(modify_prev) d.text.get else d.text.next)
      case _ => 
    }
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    // Nothing
  }
  
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.modify_text ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.modify_text :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.change_text_hint
}


/** Button to modify the language */
object ModifyLanguageButton extends MenuTextButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    val res = context.getResources()
    selectedShape match {
      case d: SoundTTS =>
        def updateText(s: String): Unit = {
          if(modify_prev) {
            d.language.set(s)
          } else {
            d.language.setNext(s)
          }
          if(copy_to_prev) {
            d.language.set(d.language.get)
          } else {
            //gameEngine.updateSelectedRule()
          }
        }
        CustomDialogs.launchChoiceDialogWithCustomchoice(context,
            String.format(res.getString(R.string.modify_text_title), d.language.next), R.array.language_possibilities,
            updateText(_), {() => }, if(modify_prev) d.language.get else d.language.next)
      case _ => 
    }
    hovered = false
  }

  private val hovered_icons = R.drawable.flat_button_resizable_highlighted ::  Nil
  private val normal_icons = R.drawable.flat_button_resizable :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.change_language_hint
}


/** Buttons that allows to move the shape. */
object MoveButton extends MenuButton {
  import MenuOptions._
  
  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    if(selectedShape.noVelocity) {
      selectedShape match {
        case selectedShape: Movable =>
          /*if(modify_prev) {
            if(Math.abs(selectedShape.x.get - selected_shape_first_x) >= 10) selectedShape.x set gameEngine.snapX(selectedShape.x.get)
            if(Math.abs(selectedShape.y.get - selected_shape_first_y) >= 10) selectedShape.y set gameEngine.snapY(selectedShape.x.get)
          } else {
            if(Math.abs(selectedShape.x.next - selected_shape_first_x) >= 10) selectedShape.x setNext Math.floor((selectedShape.x.next+smallest_size)/smallest_size2).toFloat * smallest_size2
            if(Math.abs(selectedShape.y.next - selected_shape_first_y) >= 10) selectedShape.y setNext Math.floor((selectedShape.y.next+smallest_size)/smallest_size2).toFloat * smallest_size2  
          }
          if(copy_to_prev) {
            selectedShape.x set selectedShape.x.next
            selectedShape.y set selectedShape.y.next
          }*/
        case _ =>
      }
    }
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    if(selectedShape != null) {
      selectedShape match {
        case selectedShape: Movable =>
          selectedShape.x.setPrevOrNext(modify_prev,
              gameEngine.snapX(selected_shape_first_x + relativeX/*, selectedShape.left.getPrevOrNext(modify_prev) + shiftX, selectedShape.right.getPrevOrNext(modify_prev) + shiftX*/)(points=selected_shape_first_x))
          selectedShape.y.setPrevOrNext(modify_prev,
              gameEngine.snapY(selected_shape_first_y + relativeY/*, selectedShape.top.getPrevOrNext(modify_prev) + shiftY, selectedShape.bottom.getPrevOrNext(modify_prev) + shiftY*/)(points=selected_shape_first_y))
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
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
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
    /*selectedShape match {
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
    }*/
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    if(selectedShape != null) {
      selectedShape match {
        case c:Circle =>
          val dx1 = toX - relativeX - selected_shape_first_x
          val dy1 = toY - relativeY - selected_shape_first_y
          val dx2 = toX - selected_shape_first_x
          val dy2 = toY - selected_shape_first_y
          val dr = Math.sqrt((dx2*dx2+dy2*dy2)).toFloat-Math.sqrt((dx1*dx1+dy1*dy1)).toFloat
          val newRadius =selected_shape_first_radius + dr
          val rx = c.x.getPrevOrNext(modify_prev)
          val ry = c.y.getPrevOrNext(modify_prev)
          c.radius.setPrevOrNext(modify_prev,  Math.max(smallest_size, gameEngine.snapX(rx+newRadius, rx+newRadius-selected_shape_first_radius)(points=rx+c.radius.getPrevOrNext(modify_prev))-rx))
          if(copy_to_prev) {
            c.radius set c.radius.next
          }
        case r:ResizableRectangular =>
          val newWidth = selected_shape_first_width + relativeX*2
          val newHeight = selected_shape_first_height + relativeY*2
          val rx = r.x.getPrevOrNext(modify_prev)
          val ry = r.y.getPrevOrNext(modify_prev)
          r.width.setPrevOrNext(modify_prev,  Math.max(smallest_size, 2*(gameEngine.snapX(rx+newWidth/2, rx+newWidth/2 - selected_shape_first_width)(points=rx+selected_shape_first_width/2)-rx)))
          r.height.setPrevOrNext(modify_prev, Math.max(smallest_size, 2*(gameEngine.snapY(ry+newHeight/2, ry+newHeight/2 - selected_shape_first_height)(points=ry+selected_shape_first_height/2)-ry)))
          if(copy_to_prev) {
            r.width set r.width.next
            r.height set r.height.next
          }
        case r:Array2D =>
          val newWidth = selected_shape_first_width + relativeX*2
          val newHeight = selected_shape_first_height + relativeY*2
          val numCols = r.numColumns.getPrevOrNext(modify_prev)
          val numRows = r.numRows.getPrevOrNext(modify_prev)
          
          if(newWidth/ (numCols +1) > Array2D.CELL_WIDTH) {
            r.numColumns.setPrevOrNext(modify_prev, numCols + 1)
            // Add a column
            val newCells = ArrayBuffer.tabulate(numRows) { row => Cell(r, numCols, row) }
            r.cells += newCells
            for(cell <- newCells) gameEngine.getGame.add(cell)
            
          } else if(newWidth/ (numCols - 1) < Array2D.CELL_WIDTH) {
            // remove a column
            r.numColumns.setPrevOrNext(modify_prev, numCols - 1)
            val deletedColumn = r.cells.remove(r.cells.length - 1)
            for(cell <- deletedColumn) gameEngine.getGame.remove(cell)
          }
          if(newHeight/ (numRows + 1) > Array2D.CELL_HEIGHT) {
            r.numRows.setPrevOrNext(modify_prev, numRows + 1)
            // add a line
            for((column, i) <- r.cells.zipWithIndex) {
              val newCell = Cell(r, i, numRows)
              column += newCell
              gameEngine.getGame.add(newCell)
            }
          } else if(newHeight/ (numRows - 1) < Array2D.CELL_HEIGHT) {
            r.numRows.setPrevOrNext(modify_prev, numRows - 1)
            // remove a row
            val deletedRow = for(column <- r.cells) yield column.remove(column.length - 1)
            // Do something with the deleted row
            for(cell <- deletedRow) gameEngine.getGame.remove(cell)
          }
          if(copy_to_prev) {
            r.numRows set r.numRows.next
            r.numColumns set r.numColumns.next
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
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
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
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    if(selectedShape != null) {
      ColorMenu.activated = true
      ColorMenu.onFingerMove(gameEngine, selectedShape, relativeX, relativeY, shiftX, shiftY, toX, toY)
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
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    if(selectedShape != null) {
      SystemMenu.activated = true
      SystemMenu.onFingerMove(gameEngine, selectedShape, relativeX, relativeY, shiftX, shiftY, toX, toY)
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
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
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
        case d: IntBox =>
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
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
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
      case d: IntBox =>
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
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
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
    val array = selectedShape match {
        case _: Rectangle => R.array.rename_rectangles
        case _: StringBox => R.array.rename_textbox
        case _: IntBox => R.array.rename_integerbox
        case _: Circle => R.array.rename_circles
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
        /*if(modify_prev) {
          c.angle set Math.toRadians(Math.floor((Math.toDegrees(c.angle.get) + 7.5f)/15) * 15).toFloat
        } else {
          c.angle setNext Math.toRadians(Math.floor((Math.toDegrees(c.angle.next) + 7.5f)/15) * 15).toFloat
        }*/
        if(copy_to_prev) {
          c.angle set c.angle.next
        }
      case _ =>
    }
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    if(selectedShape != null) {
      selectedShape match {
        case c:Rotationable with Positionable =>
          val dx = toX - c.x.getPrevOrNext(modify_prev)
          val dy = toY - c.y.getPrevOrNext(modify_prev)
          val dxPrev = dx - relativeX
          val dyPrev = dy - relativeY
          val crossProduct = dxPrev * dy - dyPrev * dx
          val dotProduct = dxPrev * dx + dyPrev * dy
          val norms = Math.sqrt((dx*dx+dy*dy)*(dxPrev*dxPrev+dyPrev*dyPrev)).toFloat
          val sinAngle = crossProduct/norms
          val cosAngle = dotProduct/norms
          def snap(angle: Float) = Math.toRadians(Math.floor((Math.toDegrees(angle) + 7.5f)/15) * 15).toFloat
          
          c.angle.setPrevOrNext(modify_prev, snap(selected_shape_first_angle + (Math.atan2(sinAngle, cosAngle).toFloat)))
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
