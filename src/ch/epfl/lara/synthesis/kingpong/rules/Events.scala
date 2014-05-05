package ch.epfl.lara.synthesis.kingpong.rules

import org.jbox2d.collision.WorldManifold

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.Options

object Events {

  private implicit val tmpManifold = new WorldManifold()
  
  sealed trait Event {
    def obj = Set[GameObject]()
    def selectableBy(x: Float, y: Float) = false
    def distanceSquareTo(x: Float, y: Float) = Float.PositiveInfinity
    def isNumerical = false
    def isCrossing = false
    def isFinger = false
    def isContact = false
  }
  
  sealed trait SelectableEvent extends Event {
    def p: Vec2
    override def selectableBy(x: Float, y: Float) = {
      (p.x - x) * (p.x - x) + (p.y - y) * (p.y - y) < Options.Event.selectableAreaRadius * Options.Event.selectableAreaRadius
    }
    override def distanceSquareTo(x: Float, y: Float): Float = {
      (p.x - x) * (p.x - x) + (p.y - y) * (p.y - y)
    }
  }
  object SelectableEvent {
    def unapply(e: Event): Option[(Float, Float)] = e match {
      case e: SelectableEvent => Some((e.p.x, e.p.y))
      case _ => None
    }
  }
  
  object FingerRelated {
    def unapply(e: Event): Option[(Vec2, Set[GameObject])] = e match {
      case FingerDown(p, obj) => Some((p, obj))
      case FingerUp(p, obj) => Some((p, obj))
      case FingerMove(p, _, obj) => Some((p, obj))
      case _ => None
    }
  }

  case class FingerDown(p: Vec2, override val obj: Set[GameObject]) extends Event with SelectableEvent {
    override def isFinger = true
  }
  case class FingerUp(p: Vec2, override val obj: Set[GameObject]) extends Event with SelectableEvent{
    override def isFinger = true
  }
  case class FingerMove(from: Vec2, to: Vec2, override val obj: Set[GameObject]) extends Event with SelectableEvent{
    override def isFinger = true
    def p = from
  }

  case class BeginContact(p: Vec2, objectA: GameObject, objectB: GameObject) extends Event {
    override def isContact = true
  }
  case class CurrentContact(p: Vec2, objectA: GameObject, objectB: GameObject) extends Event {
    override def isContact = true
  }
  case class EndContact(p: Vec2, objectA: GameObject, objectB: GameObject) extends Event {
    override def isContact = true
  }

  case class AccelerometerChanged(vector: Vec2) extends Event
  
  case class AssignmentEvent(p: Vec2, a: AssignableProperty[_], assignStatement: Expr) extends Event
}
