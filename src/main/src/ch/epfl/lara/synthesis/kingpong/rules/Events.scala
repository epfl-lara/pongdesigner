package ch.epfl.lara.synthesis.kingpong.rules

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import scala.collection.mutable.{Set => MSet}

object Events {

  sealed trait Event

  case class FingerDown(p: Vec2, obj: MSet[GameObject]) extends Event
  case class FingerUp(p: Vec2, obj: MSet[GameObject]) extends Event
  case class FingerMove(from: Vec2, to: Vec2, obj: MSet[GameObject]) extends Event

  case class BeginContact(contact: Contact) extends Event
  case class CurrentContact(contact: Contact) extends Event
  case class EndContact(contact: Contact) extends Event

  case class AccelerometerChanged(vector: Vec2) extends Event
  
  case class GameObjectCreated(o: GameObject) extends Event
  case class GameObjectDeleted(o: GameObject) extends Event
}
