package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Types._

object Trees {

  sealed trait Tree

  /** Statement, can have side-effect. */
  sealed trait Stat extends Tree

  case class Assign(prop: PropertyRef, rhs: Expr) extends Stat

  case class Block(exprs: Seq[Stat]) extends Stat
  case class If(expr: Expr, s1: Stat, s2: Stat) extends Stat

  /** Expressions, without side-effect. */
  sealed trait Expr extends Tree with Typed 

  case class IntegerLiteral(value: Int) extends Expr
  case class FloatLiteral(value: Float) extends Expr
  case class StringLiteral(value: String) extends Expr
  case class BooleanLiteral(value: Boolean) extends Expr
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
