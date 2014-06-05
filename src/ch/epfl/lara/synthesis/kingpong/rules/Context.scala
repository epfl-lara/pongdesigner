package ch.epfl.lara.synthesis.kingpong.rules

import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.rules.Events._

trait Context extends Any {

  def events: Iterable[Event]
  def time: Int  
  def getNewName(s: String): String
  def add(c: GameObject)
  
  def addAssignmentHistory(p: Positionable, a: AssignableProperty[_], path: Expr): Unit
  
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
  
  def existsContact(f: ContactEvent => Boolean): Boolean = events exists {
    case e: ContactEvent => f(e)
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
}

trait EmptyContext extends Context {
  def events: Iterable[Event] = Seq.empty
  def time: Int = 0 
  def getNewName(s: String): String = s + "FRESH"
  def add(c: GameObject) = ()
  def addAssignmentHistory(p: Positionable, a: AssignableProperty[_], path: Expr): Unit = ()
}
