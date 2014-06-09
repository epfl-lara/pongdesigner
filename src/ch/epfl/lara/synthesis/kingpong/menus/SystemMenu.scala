package ch.epfl.lara.synthesis.kingpong.menus

import scala.collection.mutable.HashMap

import android.graphics.Canvas
import android.graphics.drawable.Drawable

import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._

object SystemMenu extends MenuCenter {
  var activated = false
  
  menus = List(TrashButton, FixButton, SetTimeButton, CopyButton, CutButton)
  
  def draw(canvas: Canvas, gameEngine: GameView, selectedShape: GameObject, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float): Unit = {
    SetTimeButton.visible = selectedShape.isInstanceOf[SoundTTS]
    CutButton.visible   = selectedShape.isInstanceOf[Rectangle]
    TrashButton.visible = !selectedShape.isInstanceOf[Cell]
    CopyButton.visible  = !selectedShape.isInstanceOf[Cell]

    Menus.spaceMenusOnCircle(canvas, cx, cy, menus)
    
    for(menu <- menus) {
      menu.draw(canvas, gameEngine, selectedShape, bitmaps, cx, cy)
    }
  }
  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    super.onFingerUp(gameEngine, selectedShape, x, y)
  }
}

/** Sends a shape to trash
 *  TODO : This should demonstrate advanced functionality (aka: deletion is a property)
 **/
object TrashButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape.deletionTime setPrevNext gameEngine.getGame().time
    hovered = false
  }
  
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_trashcan ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_trashcan :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons
  
  def hint_id = R.string.change_trash_hint
}

/** Sets the time of a SoundTTS.
 **/
object SetTimeButton extends MenuButton {
  import MenuOptions._

  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape match {
	    case selectedShape: SoundTTS =>
	      selectedShape.time setPrevNext gameEngine.getGame().time
	  }
    hovered = false
  }
  
  private val selected_hovered_icons = R.drawable.bm_flat_button_selected_highlighted :: R.drawable.bm_reload ::  Nil
  private val selected_icons = R.drawable.bm_flat_button_selected :: R.drawable.bm_reload :: Nil
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_reload ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_reload :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = {
    val selected = selectedShape match { case s: SoundTTS => s.time.next == gameEngine.getGame().time case _ => false }
    if (hovered) {
      if (selected) selected_hovered_icons else hovered_icons
    } else  {
      if (selected) selected_icons else normal_icons
    }
  }
  
  def hint_id = R.string.change_time_hint
}

/** Puts shape properties to the init state.
 **/
object FixButton extends MenuButton {
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape.historicalProperties foreach { p =>
      p.setInit(p.getExpr)
    }
    hovered = false
  }
  
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_back_arrow ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_back_arrow :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons
  
  def hint_id = R.string.change_back_hint
}

/** Copy an object
 *  TODO : This should demonstrate advanced functionality (aka: creation/duplication)
 **/
object CopyButton extends MenuButton {
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    val freshName = gameEngine.getGame.getNewName(selectedShape.name.get)
    val fresh = selectedShape.getCopy(freshName)
    gameEngine.getGame.add(fresh)
      
    hovered = false
  }
  
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_copy_menu ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_copy_menu :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons
  
  def hint_id = R.string.change_copy_hint
}

object CutButton extends MenuButton {
  import common.Implicits._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    selectedShape match {
      case rect: Rectangle =>
        CustomDialogs.launchChoiceDialogWithCustomchoice(gameEngine.context, gameEngine.str(R.string.shredding_title, selectedShape.name.get), R.array.shredding_values,
            { (res: String) =>
              val rows = res intOrElse 1
              val cols = res int2OrElse 1
              if(rows != 1 || cols != 1) {
	              val pieces = gameEngine.cutRectangle(rect, rows, cols)
				        rect.deletionTime.setPrevNext(gameEngine.getGame.time)
				//        gameEngine.getGame.remove(rect)
				        pieces foreach gameEngine.getGame.add
				        pieces.foreach(_.creationTime.setPrevNext(gameEngine.getGame.time))
              }
            }, () => {}, "3 x 3")
        

      case _ =>
    }

    hovered = false
  }

  //TODO change the icon menu
  private val hovered_icons = R.drawable.bm_flat_button_highlighted :: R.drawable.bm_shred ::  Nil
  private val normal_icons = R.drawable.bm_flat_button :: R.drawable.bm_shred :: Nil

  def icons(gameEngine: GameView, selectedShape: GameObject) = if(hovered) hovered_icons else normal_icons

  def hint_id = R.string.change_cut_hint
}
