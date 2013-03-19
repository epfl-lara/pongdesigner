package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Types._

object Trees {

  sealed trait Tree

  //case class WhileStat(expr: ExprTree, stat: StatTree) extends StatTree
  //case class PrintlnStat(expr: ExprTree) extends StatTree
  //case class AssignStat(id: Identifier, rhs: ExprTree) extends StatTree
  //case class AssignTabStat(id: Identifier, index: ExprTree, rhs: ExprTree) extends StatTree

  sealed trait Expr extends Tree with Typed 

  case class Block(stats: List[Expr]) extends Expr
  case class If(expr: Expr, t1: Expr, t2: Expr) extends Expr

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

  case class Assign(lhs: PropertyRef, rhs: Expr) extends Expr


  trait PropertyRef {  
  
    private[kingpong] type E
    private[kingpong] def tpe: PongType[E]
    private[kingpong] def property: Property[E]

    def :=(expr: Expr): Expr = Assign(this, expr)
  }

  object PropertyRef {
    def apply[T : PongType](p: Property[T]) = new PropertyRef {
      private[kingpong] type E = T
      private[kingpong] def property = p
      private[kingpong] def tpe = implicitly[PongType[T]]
    }

    private[kingpong] def unapply(ref: PropertyRef): Option[Property[_]] = Some(ref.property)

  }

  /*
  case class Parenthesis(expr: ExprTree) extends ExprTree
  
  case class Length(expr: ExprTree) extends ExprTree
  
  case class TabPos(tab: ExprTree, index: ExprTree) extends ExprTree
  case class NewTab(size: ExprTree) extends ExprTree
  case class New(id: Identifier) extends ExprTree

  case class MethodCall(parent: ExprTree, id: Identifier, args: List[ExprTree]) extends ExprTree with Symbolic[MethodSymbol]
  case class This() extends ExprTree with Symbolic[ClassSymbol]
  */
}