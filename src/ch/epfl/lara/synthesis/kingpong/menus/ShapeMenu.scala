package ch.epfl.lara.synthesis.kingpong.menus

import scala.collection.mutable.{ArrayBuffer, HashMap}

import android.content.Context
import android.graphics.Canvas
import android.graphics.drawable.Drawable

import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.view.RenderConstants

object MenuOptions {
  /** Option indicating if the changes are on the shape and its previous state as well */
  //var copy_to_prev = true
  /** Option indicating if the changes are made on the previous state or the current state. */
  //var modify_prev = false
  
  case class Policy(value: Int) extends AnyVal {
    @inline def modifiesCurrent: Boolean = (value & 1) == 1
    @inline def modifiesNext: Boolean = (value & 2) == 2
  }

  final val MODIFY_CURRENT = Policy(1)
  final val MODIFY_NEXT = Policy(2)
  final val MODIFY_BOTH = Policy(3)
  
  implicit var modify_policy: Policy = MODIFY_BOTH // Not typed but performance is good.
  
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
  menus = List(MoveButton, PaintButton, PinButton, SizeButton, ArraySizeButton, SpeedButton, VisibilityButton,
               IncrementButton, DecrementButton, ModifyTextButton, RenameButton, SystemButton, RotateButton,
               ModifyLanguageButton, BooleanButton)

  def draw(canvas: Canvas, gameEngine: GameView, selectedShape: GameObject, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float) = {
    for(m <- menus) { m.visible = true }
    ModifyLanguageButton.visible = false
    IncrementButton.visible = false
    DecrementButton.visible = false
    ModifyTextButton.visible = false
    BooleanButton.visible = false
    val top_shift = selectedShape match {
      case d: IntBox =>
        IncrementButton.setPos(0, -1)
        DecrementButton.setPos(0, 1)
        IncrementButton.visible = true
        DecrementButton.visible = true
        -1
      case d: Booleanable =>
        BooleanButton.setPos(0, -1)
        BooleanButton.visible = true
        if(d.isInstanceOf[BooleanBox]) - 1 else { // Gravity
          BooleanButton.setPos(1, 0)
          0
        }
      case d: StringBox =>
        ModifyTextButton.setPos(0, -1)
        ModifyTextButton.visible = true
         -1
      case d: SoundTTS =>
        ModifyTextButton.setPos(0, -1)
        //ModifyLanguageButton.setPos(-1, -1)
        ModifyLanguageButton.setPos(RenderConstants.whitePaint, 33f/49f, -1, -1)
        ModifyTextButton.visible = true
        ModifyLanguageButton.visible = true
        ModifyLanguageButton.setText(d.language.next)
        -1
      case _ =>
        0
    }

    ArraySizeButton.visible = selectedShape.isInstanceOf[Array2D]
    RotateButton.visible = selectedShape.isInstanceOf[Rotationable]
    PaintButton.visible  = selectedShape.isInstanceOf[Colorable]
    SpeedButton.visible  = selectedShape.isInstanceOf[PhysicalObject]
    PinButton.visible    = selectedShape.isInstanceOf[PhysicalObject]

    MoveButton.setPos(0, 0)
    SpeedButton.setPos(1, top_shift)
    PinButton.setPos(2, top_shift)
    VisibilityButton.setPos(3, top_shift)
    ArraySizeButton.setPos(1, top_shift)
    SizeButton.setPos(1, 1)
    PaintButton.setPos(2, 1)
    SystemButton.setPos(3, 1)
    RotateButton.setPos(-1, 1)
    
    RenameButton.setText(selectedShape.name.next)
    RenameButton.setPos(RenderConstants.whitePaint, 33f/49f, 0, top_shift-1)

    if(MoveButton.hovered) {
      for(m <- menus) { m.visible = false }
      MoveButton.visible = true
    } else if(SpeedButton.hovered) {
      for(m <- menus) { m.visible = false }
      //SpeedButton.visible = true
    } else if(SizeButton.hovered) {
      for(m <- menus) { m.visible = false }
      SizeButton.visible = true
    } else if(ArraySizeButton.hovered) {
      for(m <- menus) { m.visible = false }
      ArraySizeButton.visible = true
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
      case d: ValueTextable =>
        val array = if(d.isInstanceOf[StringBox]) R.array.string_possibilities else R.array.text_possibilities
        def updateText(s: String): Unit = {
          d.value.setPrevNext(s)         
        }
        CustomDialogs.launchChoiceDialogWithCustomchoice(context,
            String.format(res.getString(R.string.modify_text_title), d.value.getPrevNext), array,
            updateText(_), {() => }, d.value.getPrevNext)  
      case _ => 
    }
    hovered = false
  }
  
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_modify_text ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_modify_text :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons
  
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
          d.language.setPrevNext(s)
        }
        CustomDialogs.launchChoiceDialogWithCustomchoice(context,
            String.format(res.getString(R.string.modify_text_title), d.language.getPrevNext), R.array.language_possibilities,
            updateText(_), {() => }, d.language.getPrevNext)
      case _ => 
    }
    hovered = false
  }

  private val hovered_icons = R.drawable.bm_flat_button_resizable_highlighted ::  Nil
  private val normal_icons = R.drawable.bm_flat_button_resizable :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons
  
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
          selectedShape.x.setPrevNext(
              gameEngine.snapX(selected_shape_first_x + relativeX/*, selectedShape.left.getPrevNext + shiftX, selectedShape.right.getPrevNext + shiftX*/)(snap_i=selected_shape_first_x))
          selectedShape.y.setPrevNext(
              gameEngine.snapY(selected_shape_first_y + relativeY/*, selectedShape.top.getPrevNext + shiftY, selectedShape.bottom.getPrevNext + shiftY*/)(snap_i=selected_shape_first_y))
        case _ =>
      }
    }
  }
  
  val private_icon = List(R.drawable.bm_cross_move)
  
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
          selectedShape.velocity setPrevNext (selectedShape.velocity.getPrevNext + Vec2(shiftX.toFloat, shiftY.toFloat))
        case _ =>
      }
    }
  }
  
  private val velocityNoneList =  R.drawable.bm_move_velocity :: noneList
  
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_move_velocity ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_move_velocity :: Nil
  private val hovered_icons_none = R.drawable.bm_flat_button_highlighted :: velocityNoneList
  private val normal_icons_none = R.drawable.bm_flat_button :: velocityNoneList
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) if(selectedShape.noVelocity) hovered_icons_none else hovered_icons else if(selectedShape.noVelocity) normal_icons_none else normal_icons)
  
  def hint_id = R.string.change_speed_hint
}

/** Button to change the speed of the shape */
object SizeButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    selectedShape match {
      case c: ResizableCircular =>
        val dx1 = toX - relativeX - selected_shape_first_x
        val dy1 = toY - relativeY - selected_shape_first_y
        val dx2 = toX - selected_shape_first_x
        val dy2 = toY - selected_shape_first_y
        val dr = Math.sqrt(dx2*dx2+dy2*dy2).toFloat - Math.sqrt(dx1*dx1+dy1*dy1).toFloat
        val newRadius = selected_shape_first_radius + dr
        val rx = c.x.getPrevNext
        val ry = c.y.getPrevNext
        c.radius.setPrevNext(Math.max(smallest_size, gameEngine.snapX(rx+newRadius, rx+newRadius-selected_shape_first_radius)(snap_i=rx+c.radius.getPrevNext)-rx))

      case r: ResizableRectangular =>
        val newWidth = selected_shape_first_width + relativeX*2
        val newHeight = selected_shape_first_height + relativeY*2
        val rx = r.x.getPrevNext
        val ry = r.y.getPrevNext
        r.width.setPrevNext(Math.max(smallest_size, 2*(gameEngine.snapX(rx+newWidth/2, rx+newWidth/2 - selected_shape_first_width)(snap_i=rx+selected_shape_first_width/2)-rx)))
        r.height.setPrevNext(Math.max(smallest_size, 2*(gameEngine.snapY(ry+newHeight/2, ry+newHeight/2 - selected_shape_first_height)(snap_i=ry+selected_shape_first_height/2)-ry)))

      case array: Array2D =>
        val rx = array.x.getPrevNext
        val ry = array.y.getPrevNext
        val newWidth = selected_shape_first_width + relativeX*2
        val newHeight = selected_shape_first_height + relativeY*2
        val numCols = array.numColumns.getPrevNext
        val numRows = array.numRows.getPrevNext
        if(newWidth > 0 && newHeight > 0) {
          val (rWidth, rHeight) = gameEngine.snapRatio((newWidth, newHeight), (selected_shape_first_width, selected_shape_first_height), (numCols, numRows))()
          // The ratio is kept. Now we try to align the right side or the bottom side with the grid.
          val cRight = rx+rWidth/2
          val cBottom = ry+rHeight/2
          val newRight =  gameEngine.snapX(cRight, cRight - selected_shape_first_width)(snap_i=rx+selected_shape_first_width/2)
          val newBottom = gameEngine.snapX(cBottom, cBottom - selected_shape_first_height)(snap_i=ry+selected_shape_first_height/2)

          val (kWidth, kHeight) = if(Math.abs(newRight - cRight) < Math.abs(newBottom - cBottom)) {
            val nWidth = 2*(newRight-rx)
            (nWidth, rHeight*nWidth/rWidth)
          } else {
            val nHeight = 2*(newBottom-ry)
            (rWidth*nHeight/rHeight, nHeight)
          }

          if (kWidth > 0)
            array.cellWidth.setPrevNext(kWidth / numCols)
          if (kHeight > 0)
            array.cellHeight.setPrevNext(kHeight / numRows)
        }

      case _ =>
    }
  }
  
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_move_size ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_move_size :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons
  
  def hint_id = R.string.change_size_hint
}

/** Button to change the number of rows and columns of the array */
object ArraySizeButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    hovered = false
  }

  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    selectedShape match {
      case array: Array2D =>
        val newWidth = selected_shape_first_width + relativeX*2
        val newHeight = selected_shape_first_height + relativeY*2
        val numCols = array.numColumns.getPrevNext
        val numRows = array.numRows.getPrevNext
        val cellWidth = array.cellWidth.getPrevNext
        val cellHeight = array.cellHeight.getPrevNext

        // Add a column
        if(newWidth/ (numCols + 1) > cellWidth) {
          array.numColumns.setPrevNext(numCols + 1)
          val newCells = ArrayBuffer.tabulate(numRows) { row => 
            val res = Cell(array, numCols, row)
            res.creationTime.set(gameEngine.getGame.time)
            res
          }
          array.cells += newCells
          for(cell <- newCells) {
            gameEngine.getGame.add(cell)
            cell.setCategory(array.cells(0)(0).category)
          }

        // Remove a column
        } else if(newWidth/ (numCols - 1) < cellWidth && numCols > 1) {
          array.numColumns.setPrevNext(numCols - 1)
          val deletedColumn = array.cells.remove(array.cells.length - 1)
          for(cell <- deletedColumn) {
            cell.deletionTime.set(gameEngine.getGame.time)
            //gameEngine.getGame.remove(cell)
          }
        }

        // Add a line
        if(newHeight/ (numRows + 1) > cellHeight) {
          array.numRows.setPrevNext(numRows + 1)
          for((column, i) <- array.cells.zipWithIndex) {
            val newCell = Cell(array, i, numRows)
            newCell.creationTime.set(gameEngine.getGame.time)
            column += newCell
            gameEngine.getGame.add(newCell)
            newCell.setCategory(array.cells(0)(0).category)
          }

        // Remove a row
        } else if(newHeight/ (numRows - 1) < cellHeight && numRows > 1) {
          array.numRows.setPrevNext(numRows - 1)
          val deletedRow = for(column <- array.cells) yield column.remove(column.length - 1)
          for(cell <- deletedRow) {
            cell.deletionTime.set(gameEngine.getGame.time)
            //gameEngine.getGame.remove(cell)
          }
        }
      case _ =>
    }
  }

  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_array_resize ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_array_resize :: Nil

  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons

  def hint_id = R.string.change_arraysize_hint
}

/** Button to change the pin state of the shape */
object PinButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape.noVelocity = !selectedShape.noVelocity
    if(selectedShape.noVelocity) {
      selectedShape match {
        case selectedShape: PhysicalObject =>
            selectedShape.velocity setPrevNext Vec2(0,0)
        case _ =>
      }
    }
    hovered = false
  }
  
  private val nailNoneList = R.drawable.bm_nail :: noneList
       
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_nail ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_nail :: Nil
  private val hovered_icons_none = R.drawable.bm_flat_button_highlighted :: nailNoneList
  private val normal_icons_none = R.drawable.bm_flat_button :: nailNoneList
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) if(!selectedShape.noVelocity) hovered_icons_none else hovered_icons else if(!selectedShape.noVelocity) normal_icons_none else normal_icons)
  
  def hint_id = R.string.change_nail_hint
}

/** Button to change the color of the shape */
object PaintButton extends MenuButton {

  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
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
  
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_menu_paint ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_menu_paint :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons
  
  def hint_id = R.string.change_paint_hint
}

object SystemButton extends MenuButton {

  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
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
  
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_gear ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_gear :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons
  
  def hint_id = R.string.system_property_hint
}

/** Changes the visibility of a shape */
object VisibilityButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = selectedShape match {
    case selectedShape: Visiblable =>
      selectedShape.visible setPrevNext !selectedShape.visible.getPrevNext
      hovered = false
    case _ =>
  }

  private val eyeNoneList = R.drawable.bm_eye :: noneList
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_eye ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_eye :: Nil
  private val hovered_icons_none = R.drawable.bm_flat_button_highlighted :: eyeNoneList
  private val normal_icons_none = R.drawable.bm_flat_button :: eyeNoneList
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = {
    if(hovered) {
      if (!selectedShape.visible.next) hovered_icons_none else hovered_icons
    } else if(!selectedShape.visible.next) {
      normal_icons_none
    } else {
      normal_icons
    }
  }

  def hint_id = R.string.change_visible_hint
}

/** Button to increment a number */
object IncrementButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape match {
        case d: IntBox =>
          d.value setPrevNext d.value.getPrevNext + 1
        case _ =>
    }
    hovered = false
  }
  
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_flat_button_p1 ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_flat_button_p1 :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons
  
  def hint_id = R.string.change_increment_hint
}

/** Button to increment a boolean */
object BooleanButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape match {
        case d: Booleanable =>
          d.value setPrevNext !d.value.getPrevNext
        case _ =>
    }
    hovered = false
  }
  
  private val hovered_icons_on = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_boolean_on ::  Nil
  private val hovered_icons_off = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_boolean_off ::  Nil
  private val normal_icons_on = R.drawable.bm_flat_button :: R.drawable.bm_boolean_on :: Nil
  private val normal_icons_off = R.drawable.bm_flat_button :: R.drawable.bm_boolean_off :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = {
    val on = selectedShape match {
      case selectedShape: Booleanable => 
        selectedShape.value.getPrevNext
      case _ => false
    }
    if(hovered) (if(on) hovered_icons_on else hovered_icons_off)  else (if(on) normal_icons_on else normal_icons_off)
  }
  
  def hint_id = R.string.change_boolean_hint
}


/** Button to decrement a number */
object DecrementButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape match {
      case d: IntBox =>
        d.value.setPrevNext(d.value.getPrevNext - 1)
      case _ =>
    }
    hovered = false
  }
  
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_flat_button_m1 ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_flat_button_m1 :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons
  
  def hint_id = R.string.change_decrement_hint
}

/** Button to rename the shape */
object RenameButton extends MenuTextButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    val array = selectedShape match {
      case _: Rectangle => R.array.rename_rectangles
      case _: StringBox => R.array.rename_textbox
      case _: IntBox => R.array.rename_integerbox
      case _: Circle => R.array.rename_circles
      case _ => R.array.rename_circles
    }
    val titleString = String.format(context.getResources().getString(R.string.rename_title), selectedShape.name.get)
    CustomDialogs.launchChoiceDialogWithCustomchoice(context, titleString, array, gameEngine.shapeEditor.renameSelectedShape(_), () => ())
    hovered = false
  }

  private val hovered_icons = R.drawable.bm_flat_button_resizable_highlighted ::  Nil
  private val normal_icons = R.drawable.bm_flat_button_resizable :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons
  
  def hint_id = R.string.change_name_hint
}


/** Button to change the rotation of the shape */
object RotateButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape match {
      case c:Rotationable =>
      case _ =>
    }
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    if(selectedShape != null) {
      selectedShape match {
        case c:Rotationable with Positionable =>
          val dx = toX - c.x.getPrevNext
          val dy = toY - c.y.getPrevNext
          val dxPrev = dx - relativeX
          val dyPrev = dy - relativeY
          val crossProduct = dxPrev * dy - dyPrev * dx
          val dotProduct = dxPrev * dx + dyPrev * dy
          val norms = Math.sqrt((dx*dx+dy*dy)*(dxPrev*dxPrev+dyPrev*dyPrev)).toFloat
          val sinAngle = crossProduct/norms
          val cosAngle = dotProduct/norms
          def snap(angle: Float) = Math.toRadians(Math.floor((Math.toDegrees(angle) + 7.5f)/15) * 15).toFloat
          
          c.angle.setPrevNext(snap(selected_shape_first_angle + Math.atan2(sinAngle, cosAngle).toFloat))
        case _ =>
      }
    }
  }
  
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_move_rotate ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_move_rotate :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons
  
  def hint_id = R.string.change_rotate_hint
}
