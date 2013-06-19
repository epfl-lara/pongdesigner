package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.rules.Context
import ch.epfl.lara.synthesis.kingpong.rules.Events._

sealed trait Value extends Any {
  def as[T : PongType]: T = implicitly[PongType[T]].toScalaValue(this)
}
  
case class IntV(value: Int) extends AnyVal with Value
case class FloatV(value: Float) extends AnyVal with Value
case class StringV(value: String) extends AnyVal with Value
case class BooleanV(value: Boolean) extends AnyVal with Value
case class Vec2V(x: Float, y: Float) extends Value
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
  
  def eval(stat: Stat)(implicit context: Context): Unit = stat match {
    case Block(stats) => 
      stats map eval

    case If(c, s1, s2) => 
      eval(c) match {
        case BooleanV(true)  => eval(s1)
        case BooleanV(false) => eval(s2)
        case v => throw InterpreterException(s"The If condition $c is not a boolean but is $v.")
      }

    case Assign(prop, rhs) =>
      prop.setNext(eval(rhs))

    case NOP => //Do nothing
  }

  def eval(expr: Expr)(implicit context: Context): Value = expr match {

    case ref: PropertyRef => ref.get
    case IntegerLiteral(v) => IntV(v)
    case FloatLiteral(v) => FloatV(v)
    case StringLiteral(v) => StringV(v)
    case BooleanLiteral(v) => BooleanV(v)
    case Vec2Literal(x, y) => Vec2V(x, y)
    case UnitLiteral => UnitV
    
    case Plus(lhs, rhs) => eval(lhs) match {
      case IntV(v1) => eval(rhs) match {
        case IntV(v2) => IntV(v1 + v2)
        case FloatV(v2) => FloatV(v1 + v2)
        case _ => error(expr)
      }
      case FloatV(v1) => eval(rhs) match {
        case IntV(v2) => FloatV(v1 + v2)
        case FloatV(v2) => FloatV(v1 + v2)
        case _ => error(expr)
      }
      case Vec2V(x1, y1) => eval(rhs) match {
        case Vec2V(x2, y2) => Vec2V(x1 + x2, y1 + y2)
        case _ => error(expr)
      }
      case _ => error(expr)
    }
      
    case Minus(lhs, rhs) => eval(lhs) match {
      case IntV(v1) => eval(rhs) match {
        case IntV(v2) => IntV(v1 - v2)
        case FloatV(v2) => FloatV(v1 - v2)
        case _ => error(expr)
      }
      case FloatV(v1) => eval(rhs) match {
        case IntV(v2) => FloatV(v1 - v2)
        case FloatV(v2) => FloatV(v1 - v2)
        case _ => error(expr)
      }
      case Vec2V(x1, y1) => eval(rhs) match {
        case Vec2V(x2, y2) => Vec2V(x1 - x2, y1 - y2)
        case _ => error(expr)
      }
      case _ => error(expr)
    }
      
    case Times(lhs, rhs) => eval(lhs) match {
      case IntV(v1) => eval(rhs) match {
        case IntV(v2) => IntV(v1 * v2)
        case FloatV(v2) => FloatV(v1 * v2)
        case Vec2V(x, y) => Vec2V(v1 * x, v1 * y)
        case _ => error(expr)
      }
      case FloatV(v1) => eval(rhs) match {
        case IntV(v2) => FloatV(v1 * v2)
        case FloatV(v2) => FloatV(v1 * v2)
        case Vec2V(x, y) => Vec2V(v1 * x, v1 * y)
        case _ => error(expr)
      }
      case Vec2V(x, y) => eval(rhs) match {
        case IntV(v) => Vec2V(v * x, v * y)
        case FloatV(v) => Vec2V(v * x, v * y)
        case _ => error(expr)
      }
      case _ => error(expr)
    }
      
    case Div(lhs, rhs) => eval(lhs) match {
      case IntV(v1) => eval(rhs) match {
        case IntV(v2)=>  FloatV(v1 / v2)
        case FloatV(v2) => FloatV(v1 / v2)
        case _ => error(expr)
      }
      case FloatV(v1) => eval(rhs) match {
        case IntV(v2) => FloatV(v1 / v2)
        case FloatV(v2) => FloatV(v1 / v2)
        case _ => error(expr)
      }
      case Vec2V(x, y) => eval(rhs) match {
        case IntV(v) => Vec2V(x / v, y / v)
        case FloatV(v) => Vec2V(x / v, y / v)
        case _ => error(expr)
      }
      case _ => error(expr)
    }

    case Mod(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (IntV(v1), IntV(v2)) => IntV(v1 % v2)
        case (NumericV(v1), NumericV(v2)) => FloatV(v1 % v2)
        case _ => throw InterpreterException(s"The mod operation cannot be applied on the two expressions $lhs and $rhs.")
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
        case (Vec2V(x1, y1), Vec2V(x2, y2)) => BooleanV((x1 equals x2) && (y1 equals y2))
        case (UnitV, UnitV) => BooleanV(true)
        case _ => BooleanV(false)
      }
      
    case LessThan(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericV(v1), NumericV(v2)) => BooleanV(v1 < v2)
        case _ => throw InterpreterException(s"A LessThan is not possible between $lhs and $rhs.")
      }

    case LessEq(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericV(v1), NumericV(v2)) => BooleanV(v1 <= v2)
        case _ => throw InterpreterException(s"A LessEq is not possible between $lhs and $rhs.")
      }

    case GreaterThan(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericV(v1), NumericV(v2)) => BooleanV(v1 > v2)
        case _ => throw InterpreterException(s"A GreaterThan is not possible between $lhs and $rhs.")
      }

    case GreaterEq(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericV(v1), NumericV(v2)) => BooleanV(v1 >= v2)
        case _ => throw InterpreterException(s"A GreaterEq is not possible between $lhs and $rhs.")
      }
      
    case Not(e) =>
      eval(e) match {
        case BooleanV(v) => BooleanV(!v)
        case _ => throw InterpreterException(s"A Not is not possible on $e.")
      }

    case FingerMoveOver(o) => 
      BooleanV(context.fingerMoves(_.obj.exists(_ == o)).nonEmpty)

    case FingerDownOver(o) => 
      BooleanV(context.fingerDowns(_.obj.exists(_ == o)).nonEmpty)

    case FingerUpOver(o) => 
      BooleanV(context.fingerUps(_.obj.exists(_ == o)).nonEmpty)

    case Collision(o1, o2) => 
      BooleanV(context.beginContacts{ c =>
        (c.contact.objectA == o1 && c.contact.objectB == o2) ||
        (c.contact.objectA == o2 && c.contact.objectB == o1)
      }.nonEmpty)

  }
  
  private def error(expr: Expr): Nothing = {
    throw InterpreterException(s"A TypeCheck error occurs during interpretation of $expr.")
  }
}