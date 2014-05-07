package ch.epfl.lara.synthesis.kingpong.rules

import org.jbox2d.collision.WorldManifold

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.{EventsHistory, Options}
import scala.annotation.tailrec

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

  case class BeginContact(p: Vec2, objectA: GameObject, objectB: GameObject) extends Event with SelectableEvent {
    override def isContact = true
  }
  case class CurrentContact(p: Vec2, objectA: GameObject, objectB: GameObject) extends Event with SelectableEvent {
    override def isContact = true
  }
  case class EndContact(p: Vec2, objectA: GameObject, objectB: GameObject) extends Event with SelectableEvent {
    override def isContact = true
  }

  case class AccelerometerChanged(vector: Vec2) extends Event
  
  case class AssignmentEvent(p: Vec2, a: AssignableProperty[_], assignStatement: Expr) extends Event with SelectableEvent

  object ::> {def unapply[A] (l: Seq[A]): Option[(Seq[A],A)] = if(l.nonEmpty) Some( (l.init, l.last) ) else None }
  object <:: {def unapply[A] (l: Seq[A]): Option[(A, Seq[A])] = if(l.nonEmpty) Some( (l.head, l.tail) ) else None }

  /**
   * Retrieves a corresponding finger down event from a given finger event.
   * Events are "linked"
   */
  def getFingerDownEvent(event: Event, time: Int, minTime: Int, history: EventsHistory): Option[FingerDown] = {
    @tailrec
    def rec(e: Event, time: Int)(events: Seq[Event] = history.events(time).reverse): Option[FingerDown] = e match {
      case FingerUp(v, _) =>
        events match {
          case (e @ FingerMove(_, `v`, _)) <:: q => rec(e, time)(q)
          case (e @ FingerDown(`v`, _))    <:: q => Some(e)
          case _ <:: q                           => rec(e, time)(q)
          case Nil if time > minTime             => rec(e, time - 1)()
          case Nil                               => None
        }

      case FingerMove(v, _, _) =>
        events match {
          case (e @ FingerMove(_, `v`, _)) <:: q => rec(e, time)(q)
          case (e @ FingerDown(`v`, _))    <:: _ => Some(e)
          case _ <:: q                           => rec(e, time)(q)
          case Nil if time > minTime             => rec(e, time - 1)()
          case Nil                               => None
        }

      case e: FingerDown => Some(e)
      case _ => None
    }
    rec(event, time)()
  }

  /**
   * Retrieves a corresponding finger Up event from a given finger event.
   * Events are "linked"
   */
  def getFingerUpEvent(event: Event, time: Int, maxTime: Int, history: EventsHistory): Option[FingerUp] = {
    @tailrec
    def rec(e: Event, time: Int)(events: Seq[Event] = history.events(time)): Option[FingerUp] = e match {
      case FingerDown(v, _) =>
        events match {
          case (e @ FingerMove(`v`, _, _)) <:: q => rec(e, time)(q)
          case (e @ FingerUp(`v`, _))      <:: _ => Some(e)
          case _ <:: q                           => rec(e, time)(q)
          case Nil if time > 0                   => rec(e, time - 1)() //TODO why not time + 1 ?? (and time < maxTime)
          case Nil                               => None
        }

      case FingerMove(a, b, _) =>
        events match {
          case (e @ FingerMove(`b`, _, _)) <:: q => rec(e, time)(q)
          case (e @ FingerUp(`a`, _))      <:: _ => Some(e)         //TODO why not `b` ??
          case _ <::q                            => rec(e, time)(q)
          case Nil if time < maxTime             => rec(e, time + 1)()
          case Nil                               => None
        }

      case e: FingerUp => Some(e)
      case _ => None
    }
    rec(event, time)()
  }

  /**
   * Retrieves a corresponding finger Move event from a given finger event.
   * Collect all objects below the line.
   * Events are "linked"
   */
  def getFingerMoveEvent(event: Event, time: Int, minTime: Int, maxTime: Int, history: EventsHistory): Option[FingerMove] = {
    @tailrec
    def rec(e: Event, time: Int, events: Seq[Event], start: Option[Vec2], end: Option[Vec2], objects: Set[GameObject]): Option[FingerMove] = e match {
      case FingerUp(v, o) =>
        start match {
          case Some(a) => Some(FingerMove(a, v, objects ++ o))
          case None    => // We are looking for the previous finger down event
            events match {
              case q ::> (e @ FingerMove(_, `v`, o2)) => rec(e, time, q, None, Some(v), objects ++ o ++ o2)
              case _ ::> (e @ FingerDown(`v`, o2))    => Some(FingerMove(v, v, objects ++ o ++ o2))
              case q ::> _                            => rec(e, time, q, None, Some(v), objects ++ o)
              case Nil if time > minTime              => rec(e, time - 1, history.events(time - 1), None, end, objects)
              case Nil                                => None
            }
        }

      case FingerDown(v, o) =>
        end match {
          case Some(b) => Some(FingerMove(v, b, objects ++ o))
          case None    => // We are looking for the next finger up event
            events match {
              case (e @ FingerMove(`v`, _, o2)) <:: q => rec(e, time, q, Some(v), None, objects ++ o ++ o2)
              case FingerUp(`v`, o2)            <:: _ => Some(FingerMove(v, v, objects ++ o ++ o2))
              case _ <:: q                            => rec(e, time, q, Some(v), None, objects ++ o)
              case Nil if time < maxTime              => rec(e, time + 1, history.events(time + 1), start, None, objects)
              case Nil                                => None
            }
        }

      case FingerMove(a, b, o) =>
        start match {
          case Some(a) => // We look for the end.
            events match {
              case (e @ FingerMove(`b`, _, o2)) <:: q => rec(e, time, q, start, None, objects ++ o ++ o2)
              case FingerUp(`b`, o2)            <:: _ => Some(FingerMove(a, b, objects ++ o ++ o2))
              case _ <:: q                            => rec(e, time, q, start, None, objects ++ o)
              case Nil if time < maxTime              => rec(e, time + 1, history.events(time + 1), start, None, objects)
              case Nil                                => None
            }
          case None => // We first look for the start.
            end match {
              case Some(b) =>
                events match {
                  case q ::> (e @ FingerMove(_, `a`, o2)) => rec(e, time, q, None, Some(b), objects ++ o ++ o2)
                  case q ::> FingerDown(`a`, o2)          => Some(FingerMove(a, b, objects ++ o ++ o2))
                  case q ::> _                            => rec(e, time, q, None, Some(b), objects ++ o)
                  case Nil if time > minTime              => rec(e, time - 1, history.events(time - 1), None, end, objects)
                  case Nil                                => None
                }
              case None => // By default, we look for the start first, then for the end.
                events match {
                  case q ::> (e @ FingerMove(_, `a`, o2)) => rec(e, time, q, None, None, Set())
                  case q ::> (e @ FingerDown(`a`, o2))    => rec(e, time, history.events(time), Some(a), None, Set())
                  case q ::> _                            => rec(e, time, q, None, None, Set())
                  case Nil if time > minTime              => rec(e, time - 1, history.events(time - 1), None, None, Set())
                  case Nil => None
                }
            }
        }
      case _ => None
    }
    rec(event, time, history.events(time), None, None, Set.empty)
  }

}
