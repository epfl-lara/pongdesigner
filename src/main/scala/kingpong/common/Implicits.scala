package ch.epfl.lara.synthesis.kingpong.common

// Remove implicit warnings
import language.implicitConversions

import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._

object Implicits {
  implicit def NumericIsExpr[T: Numeric](n: T): Expr = FloatLiteral(implicitly[Numeric[T]].toFloat(n))
  implicit def FloatIsExpr(f: Float): Expr = FloatLiteral(f)
  implicit def IntegerIsExpr(i: Int): Expr = IntegerLiteral(i)
  implicit def StringIsExpr(s: String): Expr = StringLiteral(s)
  implicit def BooleanIsExpr(b: Boolean): Expr = BooleanLiteral(b)
  implicit def Vec2IsExpr(v: Vec2): Expr = Vec2Literal(v.x, v.y)

  implicit object IntegerIsPongType extends PongType[Int] {
    def getPongType = TInt
    def toPongValue(v: Any) = v match {
      case f: Float if f.isWhole => IntV(f.toInt)
      case i: Int => IntV(i)
      case _ => throw InterpreterException(s"The value $v is incompatible with Int.")
    }
    def toScalaValue(v: Value) = v match {
      case FloatV(f) if f.isWhole => f.toInt
      case IntV(i)   => i
      case _ => throw InterpreterException(s"The value $v is incompatible with Int.")
    }
  }

  implicit object FloatIsPongType extends PongType[Float] {
    def getPongType = TFloat
    def toPongValue(v: Any) = v match {
      case f: Float => FloatV(f)
      case i: Int => FloatV(i.toFloat)
      case _ => throw InterpreterException(s"The value $v is incompatible with Float.")
    }
    def toScalaValue(v: Value) = v match {
      case FloatV(f) => f
      case IntV(i)   => i.toFloat
      case _ => throw InterpreterException(s"The value $v is incompatible with Float.")
    }
  }

  implicit object BooleanIsPongType extends PongType[Boolean] {
    def getPongType = TBoolean
    def toPongValue(v: Any) = v match {
      case b: Boolean => BooleanV(b)
      case _ => throw InterpreterException(s"The value $v is incompatible with Boolean.")
    }
    def toScalaValue(v: Value) = v match {
      case BooleanV(b) => b
      case _ => throw InterpreterException(s"The value $v is incompatible with Boolean.")
    }
  }

  implicit object StringIsPongType extends PongType[String] {
    def getPongType = TBoolean
    def toPongValue(v: Any) = v match {
      case s: String => StringV(s)
      case _ => throw InterpreterException(s"The value $v is incompatible with String.")
    }
    def toScalaValue(v: Value) = v match {
      case StringV(s) => s
      case _ => throw InterpreterException(s"The value $v is incompatible with String.")
    }
  }

  implicit object Vec2IsPongType extends PongType[Vec2] {
    def getPongType = TVec2
    def toPongValue(v: Any) = v match {
      case vec: Vec2 => Vec2V(vec.x, vec.y)
      case _ => throw InterpreterException(s"The value $v is incompatible with Vec2.")
    }
    def toScalaValue(v: Value) = v match {
      case Vec2V(x, y) => Vec2(x, y)
      case _ => throw InterpreterException(s"The value $v is incompatible with Vec2.")
    }
  }

}