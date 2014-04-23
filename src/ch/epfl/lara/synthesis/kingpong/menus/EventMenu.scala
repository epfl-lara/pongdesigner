package ch.epfl.lara.synthesis.kingpong.menus

import scala.collection.mutable.HashMap
import ch.epfl.lara.synthesis.kingpong._
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.drawable.Drawable
import android.content.Context
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.rules.Events._

/**
 * This menu appears when an event is selected to describe a rule, when it can be ambiguous.
 */
object EventMenu /*extends MenuCenter*/ {
  private var activated = false
  var shape: GameObject = null
  def isActivated = activated
  def activate(b: Boolean, newShape: GameObject = null) = {
    activated = b
    if(activated) shape = newShape
  }
  /*val menu1 = List(FingerDownEventMenu, FingerUpEventMenu, FingerMoveEventMenu)
  val menu2 = List(NormalCollisionButton)//, NoCollisionButRecordedEffectsButton, NoCollisionButton)
  menus = menu1
  def draw(canvas: Canvas, gameEngine: GameView, selectedShape: GameObject, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float): Unit = {
    /*gameEngine.eventEditor.selectedEventTime match {
      case Some(BeginContact(c)) =>
        menus = menu2
      case _ =>
        menus = menu1
    }*/
    Menus.spaceMenusOnCircle(canvas, menus)
    for(menu <- menus) {
      menu.draw(canvas, gameEngine, selectedShape, bitmaps, cx, cy)
    }
  }*/
}

/*
/** Buttons that allows to choose the finger down event */
object FingerDownEventMenu extends MenuButton {
  import MenuOptions._
  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    hovered = false
    /*gameEngine.eventEditor.selectedEvent match {
      case e@Some(_:FingerUp | _:FingerMove) =>
        val event = gameEngine.getGame.getFingerDownEvent(e.get, gameEngine.eventEditor.selectedTime.toInt)()
        event match {
          case Some(event) =>
            //event.obj += EventMenu.shape
            if(event.obj.isEmpty) 
            gameEngine.eventEditor.select(event, gameEngine.eventEditor.selectedTime)
          case None =>
        }
      case _ =>
    }*/
    gameEngine.setModeSelectEffects()
    EventMenu.activate(false)
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    // Nothing
  }
  
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.fingerdown_button ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.fingerdown_button :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.event_fingerdown_hint
}


/** Buttons that allows to choose the finger move event. */
object FingerMoveEventMenu extends MenuButton {
  import MenuOptions._
  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    hovered = false
    
    /*gameEngine.eventEditor.selectedEvent match {
      case e@Some(_:FingerUp | _:FingerMove) =>
        val event = gameEngine.getGame.getFingerMoveEvent(e.get, gameEngine.eventEditor.selectedTime.toInt)()
        event match {
          case Some(event) =>
            //event.obj += EventMenu.shape
            if(event.obj.isEmpty) 
            gameEngine.eventEditor.select(event, gameEngine.eventEditor.selectedTime)
          case None =>
        }
      case _ =>
    }*/
    gameEngine.setModeSelectEffects()
    EventMenu.activate(false)
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    // Nothing
  }
  
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.fingermove_button ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.fingermove_button :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.event_fingermove_hint
}


/** Buttons that allows to choose the finger up event. */
object FingerUpEventMenu extends MenuButton {
  import MenuOptions._
  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    hovered = false
    /*gameEngine.eventEditor.selectedEvent match {
      case e@Some(_:FingerUp | _:FingerMove) =>
        val event = gameEngine.getGame.getFingerUpEvent(e.get, gameEngine.eventEditor.selectedTime.toInt)()
        event match {
          case Some(event) =>
            //event.obj += EventMenu.shape
            if(event.obj.isEmpty) 
            gameEngine.eventEditor.select(event, gameEngine.eventEditor.selectedTime)
          case None =>
        }
      case _ =>
    }*/
    gameEngine.setModeSelectEffects()
    EventMenu.activate(false)
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    // Nothing
  }
  
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.fingerup_button ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.fingerup_button :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.event_fingerup_hint
}

/** Buttons that allows to choose the normal collision event */
object NormalCollisionButton extends MenuButton {
  import MenuOptions._
  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    hovered = false
    gameEngine.setModeSelectEffects()
    EventMenu.activate(false)
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    // Nothing
  }

  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.collision_effect ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.collision_effect :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.event_collide_hint
}


/** Buttons that allows to choose that no collision occurs but the collision is recorded */
/*object NoCollisionButRecordedEffectsButton extends MenuButton {
  import MenuOptions._
  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    hovered = false
    val game = gameEngine.getGame()
    val new_rule = NoCollisionEffectBetweenRule(EIdentShape(gameEngine.eventEditor.selectedEvent.value.shape1), EIdentShape(gameEngine.eventEditor.selectedEvent.value.shape2))
    game.giveRuleNewCoordinates(new_rule)
    game.insertRule(new_rule, game.currentTime)
    gameEngine.setModeSelectEffects()
    EventMenu.activate(false)
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    // Nothing
  }
  
  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.no_collision_effect ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.no_collision_effect :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.event_overlap_hint
}*/


/** Buttons that allows to choose that no collision occurs */
/*object NoCollisionButton extends MenuButton {
  import MenuOptions._
  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    hovered = false
    val game = gameEngine.getGame()
    val new_rule = NoCollisionBetweenRule(EIdentShape(gameEngine.eventEditor.selectedEvent.value.shape1), EIdentShape(gameEngine.eventEditor.selectedEvent.value.shape2))
    game.giveRuleNewCoordinates(new_rule)
    game.insertRule(new_rule, game.currentTime)
    gameEngine.setModeModifyGame()

    EventMenu.activate(false)
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, toX: Float, toY: Float) = {
    // Nothing
  }

  private val hovered_icons = R.drawable.flat_button_highlighted :: R.drawable.no_collision ::  Nil
  private val normal_icons = R.drawable.flat_button :: R.drawable.no_collision :: Nil
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) hovered_icons else normal_icons)
  
  def hint_id = R.string.event_nocollide_hint
}*/

*/