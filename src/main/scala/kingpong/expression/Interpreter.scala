package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._


sealed trait Value[+T] extends Any {
  def value: T
}
  
case class IntV(value: Int) extends AnyVal with Value[Int]
case class FloatV(value: Float) extends AnyVal with Value[Float]
case class StringV(value: String) extends AnyVal with Value[String]
case class BooleanV(value: Boolean) extends AnyVal with Value[Boolean]
case object UnitV extends Value[Unit] {
  def value = Unit
}

object NumericV {
  def unapply(value: Value[Any]): Option[Float] = value match {
    case IntV(i) => Some(i.toFloat)
    case FloatV(f) => Some(f)
    case _ => None
  }
}

case class InterpreterException(msg: String) extends Exception(msg)

trait Interpreter {
  
  def evaluate[T : PongType](expr: Expr): T = implicitly[PongType[T]].toScalaValue(eval(expr))

  def eval(expr: Expr): Value[Any] = expr match {
    case Block(Nil)    => UnitV
    case Block(stats)  => 
      val values = stats map eval
      values.last
      
    case If(c, e1, e2) => 
      eval(c) match {
        case BooleanV(true)  => eval(e1)
        case BooleanV(false) => eval(e2)
        case v => throw InterpreterException(s"The If condition $c is not a boolean but is $v.")
      }

    case ref: PropertyRef => ref.getPongValue
    case IntegerLiteral(v) => IntV(v)
    case FloatLiteral(v) => FloatV(v)
    case StringLiteral(v) => StringV(v)
    case BooleanLiteral(v) => BooleanV(v)
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

    case Assign(ref, rhs) =>
      val v = eval(rhs)
      val tpe = anyToType(v.value)
      if (tpe == ref.getPongType) {
        ref.setPongValue(v)
      } 
      else {
        throw InterpreterException(s"Expression $rhs has wrong type $tpe, expected ${ref.getPongType}.")
      }
      
      UnitV
  }

  def anyToValue(any: Any): Value[Any] = any match {
    case v: Int => IntV(v)
    case v: Float => FloatV(v)
    case v: String => StringV(v)
    case v: Boolean => BooleanV(v)
    case v: Unit => UnitV
    case v => throw InterpreterException(s"The value $v has an unknown type.")
  }
  
}