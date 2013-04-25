package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._

object Trees {

  sealed trait Tree

  /** Statement, can have side-effect. */
  sealed trait Stat extends Tree

  case class Assign(prop: PropertyRef, rhs: Expr) extends Stat
  case class Block(stats: Seq[Stat]) extends Stat
  case class If(expr: Expr, s1: Stat, s2: Stat) extends Stat
  case object NOP extends Stat

  /** Expressions, without side-effect. */
  sealed trait Expr extends Tree with Typed {

    def +(e: Expr): Expr = Plus(this, e)
    def -(e: Expr): Expr = Minus(this, e)
    def *(e: Expr): Expr = Times(this, e)
    def /(e: Expr): Expr = Div(this, e)
    def &&(e: Expr): Expr = And(this, e)
    def ||(e: Expr): Expr = Or(this, e)
    def =:=(e: Expr): Expr = Equals(this, e)
    def <(e: Expr): Expr = LessThan(this, e)
    def unary_! : Expr = Not(this)

  }

  case class IntegerLiteral(value: Int) extends Expr
  case class FloatLiteral(value: Float) extends Expr
  case class StringLiteral(value: String) extends Expr
  case class BooleanLiteral(value: Boolean) extends Expr
  case class Vec2Literal(value: Vec2) extends Expr
  case object UnitLiteral extends Expr 

  case class Plus(lhs: Expr, rhs: Expr) extends Expr
  case class Minus(lhs: Expr, rhs: Expr) extends Expr
  case class Times(lhs: Expr, rhs: Expr) extends Expr
  case class Div(lhs: Expr, rhs: Expr) extends Expr

  case class And(lhs: Expr, rhs: Expr) extends Expr
  case class Or(lhs: Expr, rhs: Expr) extends Expr
  case class Equals(lhs: Expr, rhs: Expr) extends Expr
  case class LessThan(lhs: Expr, rhs: Expr) extends Expr
  case class Not(expr: Expr) extends Expr

  case class FingerMoveOver(c: Category) extends Expr
  case class FingerDownOver(c: Category) extends Expr
  case class FingerUpOver(c: Category) extends Expr
  case class Collision(c1: Category, c2: Category) extends Expr

  trait PropertyRef extends Expr { self =>
    
    private[kingpong] def getPongType: Type
    private[kingpong] def setPongValue(v: Value): self.type
    private[kingpong] def getPongValue: Value

    def :=(expr: Expr): Stat = Assign(this, expr)
  }

  case class SinglePropertyRef[T](property: Property[T]) extends PropertyRef {
    
    private[kingpong] def getPongType = property.getPongType

    private[kingpong] def setPongValue(v: Value) = {
      property.setPongValue(v)
      this
    }

    private[kingpong] def getPongValue = property.getPongValue
  }

  case class CategoryPropertyRef(properties: Set[Property[_]]) extends PropertyRef {
    
    //TODO how to handle a get on an empty category ?
    require(properties.nonEmpty)

    private[kingpong] def getPongType = properties.head.getPongType

    private[kingpong] def setPongValue(v: Value) = {
      properties foreach {_.setPongValue(v)}
      this
    }

    private[kingpong] def getPongValue = properties.head.getPongValue
  }

}
