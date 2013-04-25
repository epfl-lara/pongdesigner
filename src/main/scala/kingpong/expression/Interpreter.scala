package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._

sealed trait Value extends Any {
  def as[T : PongType]: T = implicitly[PongType[T]].toScalaValue(this)
}
  
case class IntV(value: Int) extends AnyVal with Value
case class FloatV(value: Float) extends AnyVal with Value
case class StringV(value: String) extends AnyVal with Value
case class BooleanV(value: Boolean) extends AnyVal with Value
case class Vec2V(value: Vec2) extends AnyVal with Value
case object UnitV extends Value

object NumericV {
  def unapply(value: Value): Option[Float] = value match {
    case IntV(i) => Some(i.toFloat)
    case FloatV(f) => Some(f)
    case _ => None
  }
}

case class InterpreterException(msg: String) extends Exception(msg)

trait Interpreter {
  
  def eval(stat: Stat): Unit = stat match {
    case Block(stats) => 
      stats map eval

    case If(c, s1, s2) => 
      eval(c) match {
        case BooleanV(true)  => eval(s1)
        case BooleanV(false) => eval(s2)
        case v => throw InterpreterException(s"The If condition $c is not a boolean but is $v.")
      }

    case Assign(prop, rhs) =>
      prop.setPongValue(eval(rhs))

    case NOP => //Do nothing
  }

  def eval(expr: Expr): Value = expr match {

    case ref: PropertyRef => ref.getPongValue
    case IntegerLiteral(v) => IntV(v)
    case FloatLiteral(v) => FloatV(v)
    case StringLiteral(v) => StringV(v)
    case BooleanLiteral(v) => BooleanV(v)
    case Vec2Literal(v) => Vec2V(v)
    case UnitLiteral => UnitV
    
    case Plus(lhs, rhs) =>  
      (eval(lhs), eval(rhs)) match {
        case (IntV(v1), IntV(v2)) => IntV(v1 + v2)
        case (NumericV(v1), NumericV(v2)) => FloatV(v1 + v2)
        case _ => throw InterpreterException(s"The two expressions $lhs and $rhs cannot be added.")
      }
      
    case Minus(lhs, rhs) => 
      (eval(lhs), eval(rhs)) match {
        case (IntV(v1), IntV(v2)) => IntV(v1 - v2)
        case (NumericV(v1), NumericV(v2)) => FloatV(v1 - v2)
        case _ => throw InterpreterException(s"The two expressions $lhs and $rhs cannot be subtracted.")
      }
      
    case Times(lhs, rhs) => 
      (eval(lhs), eval(rhs)) match {
        case (IntV(v1), IntV(v2)) => IntV(v1 * v2)
        case (NumericV(v1), NumericV(v2)) => FloatV(v1 * v2)
        case _ => throw InterpreterException(s"The two expressions $lhs and $rhs cannot be multiplied.")
      }
      
    case Div(lhs, rhs) => 
      (eval(lhs), eval(rhs)) match {
        case (IntV(v1), IntV(v2)) => FloatV(v1 / v2) //TODO do we want an division on integers or on floats ? 
        case (NumericV(v1), NumericV(v2)) => FloatV(v1 / v2)
        case _ => throw InterpreterException(s"The two expressions $lhs and $rhs cannot be divided.")
      }
  
    case And(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (BooleanV(v1), BooleanV(v2)) => BooleanV(v1 && v2)
        case _ => throw InterpreterException(s"An AND is not possible between $lhs and $rhs.")
      }
      
    case Or(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (BooleanV(v1), BooleanV(v2)) => BooleanV(v1 || v2)
        case _ => throw InterpreterException(s"An OR is not possible between $lhs and $rhs.")
      }
      
    case Equals(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericV(v1), NumericV(v2)) => BooleanV(v1 equals v2)
        case (StringV(v1), StringV(v2)) => BooleanV(v1 equals v2)
        case (BooleanV(v1), BooleanV(v2)) => BooleanV(v1 equals v2)
        case (Vec2V(v1), Vec2V(v2)) => BooleanV(v1 equals v2)
        case (UnitV, UnitV) => BooleanV(true)
        case _ => BooleanV(false)
      }
      
    case LessThan(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericV(v1), NumericV(v2)) => BooleanV(v1 < v2)
        case _ => throw InterpreterException(s"A LessThan is not possible between $lhs and $rhs.")
      }
      
    case Not(e) =>
      eval(e) match {
        case BooleanV(v) => BooleanV(!v)
        case _ => throw InterpreterException(s"A Not is not possible on $e.")
      }



    //TODO implement finger expression, we need a context containing finger position.
    case FingerMoveOver(c) => ???
    case FingerDownOver(c) => ???
    case FingerUpOver(c) => ???
    case Collision(c1, c2) => ???

  }
  
}