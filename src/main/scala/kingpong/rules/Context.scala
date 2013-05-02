package ch.epfl.lara.synthesis.kingpong.rules

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.rules.Events._

trait Context extends Any {

  def events: Iterable[Event]

  def fingerDowns(f: FingerDown => Boolean): Iterable[FingerDown] = events collect {
    case e: FingerDown if f(e) => e
  }

  def fingerUps(f: FingerUp => Boolean): Iterable[FingerUp] = events collect {
    case e: FingerUp if f(e) => e
  }

  def fingerMoves(f: FingerMove => Boolean): Iterable[FingerMove] = events collect {
    case e: FingerMove if f(e) => e
  }

  def beginContacts(f: BeginContact => Boolean): Iterable[BeginContact] = events collect {
    case e: BeginContact if f(e) => e
  }

  def currentContacts(f: CurrentContact => Boolean): Iterable[CurrentContact] = events collect {
    case e: CurrentContact if f(e) => e
  }

  def endContacts(f: EndContact => Boolean): Iterable[EndContact] = events collect {
    case e: EndContact if f(e) => e
  }
}
