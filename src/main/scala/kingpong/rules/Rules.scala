package ch.epfl.lara.synthesis.kingpong.rules

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Trees._

object Rules {


  sealed trait Rule {
    def cond: Expr
    def action: Stat

    def evaluate(interpreter: Interpreter)(implicit context: Context): Unit
  }


  class Whenever(val cond: Expr, val action: Stat) extends Rule {
    def evaluate(interpreter: Interpreter)(implicit context: Context) {
      if (interpreter.eval(cond).as[Boolean]) {
        interpreter.eval(action)
      }
    }
  }

  class On(val cond: Expr, val action: Stat) extends Rule {
    private var lastEval: Boolean = false

    def evaluate(interpreter: Interpreter)(implicit context: Context) {
      val b = interpreter.eval(cond).as[Boolean]

      if (!lastEval && b) {
        interpreter.eval(action)
        lastEval = true
      } 
      else if (!b) {
        lastEval = false
      }
    }
  }

  class Once(val cond: Expr, val action: Stat) extends Rule {
    private var evaluated: Boolean = false

    def evaluate(interpreter: Interpreter)(implicit context: Context) {
      // evaluate the action (and the condition) only if never evaluated
      if (!evaluated && interpreter.eval(cond).as[Boolean]) {
        interpreter.eval(action)
        evaluated = true
      } 
    }
  }

}
