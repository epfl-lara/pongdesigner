package ch.epfl.lara.synthesis.kingpong.rules

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import scala.collection.mutable.{Set => MSet}
import ch.epfl.lara.synthesis.kingpong.Options

object Events {

  sealed trait Event {
    def obj = MSet[GameObject]()
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
  
  sealed trait PhysicalContactEvent extends SelectableEvent {
    def contact: Contact
    def p: Vec2 = contact.getManifold().localPoint
  }
  
  object FingerRelated {
    def unapply(e: Event): Option[(Vec2, MSet[GameObject])] = e match {
      case FingerDown(p, obj) => Some((p, obj))
      case FingerUp(p, obj) => Some((p, obj))
      case FingerMove(p, _, obj) => Some((p, obj))
      case _ => None
    }
  }

  case class FingerDown(p: Vec2, override val obj: MSet[GameObject]) extends Event with SelectableEvent {
    override def isFinger = true
  }
  case class FingerUp(p: Vec2, override val obj: MSet[GameObject]) extends Event with SelectableEvent{
    override def isFinger = true
  }
  case class FingerMove(from: Vec2, to: Vec2, override val obj: MSet[GameObject]) extends Event with SelectableEvent{
    override def isFinger = true
    def p = from
  }

  case class BeginContact(contact: Contact) extends Event with PhysicalContactEvent {
    override def isContact = true
  }
  case class CurrentContact(contact: Contact) extends Event with PhysicalContactEvent {
    override def isContact = true
  }
  case class EndContact(contact: Contact) extends Event with PhysicalContactEvent {
    override def isContact = true
  }

  case class AccelerometerChanged(vector: Vec2) extends Event
}
