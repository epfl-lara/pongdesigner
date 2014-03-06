package ch.epfl.lara.synthesis.kingpong.rules

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.rules.Events._
import scala.collection.mutable.{Set => MSet}
import ch.epfl.lara.synthesis.kingpong.expression.Value

trait Context extends Any {

  def events: Iterable[Event]
  def addEvent(e: Event): Unit
  
  def time: Long
  def addAssignment(a: Assign, p: PropertyRef)
  
  
  def fingerDowns(f: FingerDown => Boolean): Iterable[FingerDown] = events collect {
    case e: FingerDown if f(e) => e
  }
  
  def existsFingerDown(f: FingerDown => Boolean): Boolean = events exists {
    case e: FingerDown => f(e)
    case _ => false
  }

  def fingerUps(f: FingerUp => Boolean): Iterable[FingerUp] = events collect {
    case e: FingerUp if f(e) => e
  }
  
  def existsFingerUp(f: FingerUp => Boolean): Boolean = events exists {
    case e: FingerUp => f(e)
    case _ => false
  }

  def fingerMoves(f: FingerMove => Boolean): Iterable[FingerMove] = events collect {
    case e: FingerMove if f(e) => e
  }
  
  def existsFingerMove(f: FingerMove => Boolean): Boolean = events exists {
    case e: FingerMove => f(e)
    case _ => false
  }

  def beginContacts(f: BeginContact => Boolean): Iterable[BeginContact] = events collect {
    case e: BeginContact if f(e) => e
  }
  
  def existsBeginContact(f: BeginContact => Boolean): Boolean = events exists {
    case e: BeginContact => f(e)
    case _ => false
  }

  def currentContacts(f: CurrentContact => Boolean): Iterable[CurrentContact] = events collect {
    case e: CurrentContact if f(e) => e
  }
  
  def existsCurrentContact(f: CurrentContact => Boolean): Boolean = events exists {
    case e: CurrentContact => f(e)
    case _ => false
  }

  def endContacts(f: EndContact => Boolean): Iterable[EndContact] = events collect {
    case e: EndContact if f(e) => e
  }
  
  def existsEndContact(f: EndContact => Boolean): Boolean = events exists {
    case e: EndContact => f(e)
    case _ => false
  }
  
  def get(value: String): Option[Value]
  def set(value: String, v: Value)
  def addMethod(name: String, methodDecl: MethodDecl): Unit
   def getMethod(name: String): MethodDecl
  
  def getOrElse(value: String, orElse: => Value): Value = get(value) match {
    case Some(v) => v
    case None => orElse
  }
  def getOrElseUpdate(value: String, orElse: => Value): Value = get(value) match {
    case Some(v) => v
    case None => set(value, orElse)
      orElse
  }
  
  def getNewName(s: String): String
  
  def add(c: GameObject)
}
