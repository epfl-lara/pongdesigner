package ch.epfl.lara.synthesis.kingpong.test;

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.ComfusySolver._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TypeChecker
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.rules.Context
import scala.collection.mutable.Map

class ComfusySolvertest extends FlatSpec with ShouldMatchers {
  
  import ComfusySolver._
  
  implicit val context = Map[ComfusySolver.Name, Expr]()
  def variable(s: Name): ComExpr = Sum(0, List((s, 1)))
  
  "Comfusy Solver" should "solve trivial examples" in {
    ComfusySolver.solveCanonized(Nil, Conjunct(Nil, Nil)) should equal ((True, Nil))
    ComfusySolver.solveCanonized(List("x"), Conjunct(Nil, Sum(0, List(("x", 1)))::Nil)) should equal ((True, Sum(0, Nil)::Nil))
    ComfusySolver.solveCanonized(List("x"), Conjunct(Nil, Sum(2, List(("x", 1)))::Nil)) should equal ((True, Sum(-2, Nil)::Nil))
    ComfusySolver.solveCanonized(List("x"), Conjunct(Nil, Sum(2, List(("x", -1)))::Nil)) should equal ((True, Sum(2, Nil)::Nil))
    ComfusySolver.solveCanonized(List("x"), Conjunct(Nil, Sum(2, List(("x", -1),("y", 3)))::Nil)) should equal ((True, Sum(2, List(("y", 3)))::Nil))
    ComfusySolver.solveCanonized(List("x"), Conjunct(Nil, Sum(2, List(("x", 0.5f),("y", 3)))::Nil)) should equal ((True, Sum(-4f, List(("y", -6)))::Nil))
  }
  "Comfusy Solver" should "solve inequalities" in {
    val inequality = Conjunct(Sum(2, List(("x", 1)))::Nil, Nil)
    val inequality2 = Conjunct(Sum(-5, List(("x", -2)))::Nil, Nil)
    val inequalityTotal = inequality && inequality2
    val X = variable("x")
    ComfusySolver.solveCanonized(List("x"), inequality) should equal ((True, List(IfExpr(inequality, X, Sum(-2, Nil)))))
    ComfusySolver.solveCanonized(List("x"), inequalityTotal) should equal ((True, List(IfExpr(inequality, IfExpr(inequality2, X, Sum(-2.5, Nil)), Sum(-2, Nil)))))
  }
  "Comfusy Solver" should "solve double examples" in {
    ComfusySolver.solveCanonized(Nil, Conjunct(Nil, Nil)) should equal ((True, Nil))
    ComfusySolver.solveCanonized(List("x", "y"), Conjunct(Nil, Sum(-2, List(("x", 1), ("y", 1)))::Sum(-1, List(("x", 1), ("y", -1)))::Nil)) should equal ((True, Sum(1.5, Nil)::Sum(0.5, Nil)::Nil))
  }
}