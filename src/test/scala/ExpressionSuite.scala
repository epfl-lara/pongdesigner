package ch.epfl.lara.synthesis.kingpong.test

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._

class ExpressionSuite extends FunSuite with BeforeAndAfter {
 
  implicit var interpreter: Interpreter with TypeChecker = _

  before {
    interpreter = new Interpreter with TypeChecker {}
  }

  test("Implicits to build expressions are working.") {
    var e: Expr = 1
    assert(e === IntegerLiteral(1))
    e = 1.2f
    assert(e === FloatLiteral(1.2f))
    e = 1.2d
    assert(e === FloatLiteral(1.2f))
    e = "test"
    assert(e === StringLiteral("test"))
    e = true
    assert(e === BooleanLiteral(true))
  }

  test("Typecheck and Interpreter for expressions work.") {
    var e: Expr = Plus(1, 3)
    interpreter.typeCheck(e)
    assert(e.getType === TInt)
    assert(interpreter.eval(e) === IntV(4))
    assert(interpreter.eval(e).as[Int] === 4)

    e = Plus(1, 3.5)
    interpreter.typeCheck(e)
    assert(e.getType === TFloat)
    assert(interpreter.eval(e) === FloatV(4.5f))
    assert(interpreter.eval(e).as[Float] === 4.5f)

    e = Times(1, true)
    intercept[TypeCheckException] {
      interpreter.typeCheck(e)
    }
    intercept[InterpreterException] {
      interpreter.eval(e)
    }

    e = Equals(true, 2.4)
    interpreter.typeCheck(e)
    assert(e.getType === TBoolean)
    assert(interpreter.eval(e) === BooleanV(false))
    assert(interpreter.eval(e).as[Boolean] === false)

    e = Div(Times(Minus(Plus(2, 20), 1), 4), 2)
    interpreter.typeCheck(e)
    assert(e.getType === TFloat)
    assert(interpreter.eval(e) === FloatV(42f))
  }

  test("Typecheck and Interpreter for statements work.") {
    val p = new SimpleProperty[Int]("test_prop", 1).reset
    var s: Stat = If(Equals(p.ref, 1), 
      Assign(p.ref, Plus(p.ref, 1)),
      Assign(p.ref, Minus(p.ref, 1)))

    interpreter.typeCheck(s)
    interpreter.eval(s)
    assert(p.get === 2)
  }
}