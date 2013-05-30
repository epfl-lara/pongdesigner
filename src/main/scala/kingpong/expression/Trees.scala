package ch.epfl.lara.synthesis.kingpong.expression

// remove the warning 
import language.existentials

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
    def %(e: Expr): Expr = Mod(this, e)
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
  case class Vec2Literal(x: Float, y: Float) extends Expr
  case object UnitLiteral extends Expr 

  case class Plus(lhs: Expr, rhs: Expr) extends Expr
  case class Minus(lhs: Expr, rhs: Expr) extends Expr
  case class Times(lhs: Expr, rhs: Expr) extends Expr
  case class Div(lhs: Expr, rhs: Expr) extends Expr
  case class Mod(lhs: Expr, rhs: Expr) extends Expr

  case class And(lhs: Expr, rhs: Expr) extends Expr
  case class Or(lhs: Expr, rhs: Expr) extends Expr
  case class Equals(lhs: Expr, rhs: Expr) extends Expr
  case class LessThan(lhs: Expr, rhs: Expr) extends Expr
  case class Not(expr: Expr) extends Expr

  case class FingerMoveOver(o: GameObject) extends Expr
  case class FingerDownOver(o: GameObject) extends Expr
  case class FingerUpOver(o: GameObject) extends Expr
  case class Collision(o1: GameObject, o2: GameObject) extends Expr

  case class PropertyRef(property: Property[_]) extends Expr {
    
    /** Return the internal type of this property. */
    private[kingpong] def getPongType: Type = property.getPongType
    
    /** Set the next value if this property. */
    private[kingpong] def setNext(v: Value) = {
      property.setNext(v)
      this
    }

    /** Get the current value of this property. */
    private[kingpong] def get: Value = property.getPongValue

    def :=(expr: Expr): Stat = Assign(this, expr)
    def assign(expr: Expr): Stat = this := expr
  }

}
