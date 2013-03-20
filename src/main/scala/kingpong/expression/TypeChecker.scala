package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._

trait TypeChecker {
  
  case class TypeCheckException(msg: String) extends Exception(msg)
  
  def typeCheck(expr: Expr): Expr = expr match {
    case Block(Nil)    => expr.setType(TUnit)
    case Block(stats)  => 
      stats foreach typeCheck
      val t = stats.last.getType
      expr.setType(t)
      
    case If(c, e1, e2) => 
      typeCheck(c, TBoolean)
      val t1 = typeCheck(e1).getType
      val t2 = typeCheck(e2).getType
      if (t1 != t2) {
        expr.setType(TError)
        throw TypeCheckException(s"The IF branches $e1 and $e2 have two different types $t1 and $t2.")
      }
      expr.setType(t1)
      

    case ref: PropertyRef => ref.setType(ref.getPongType)
    case IntegerLiteral(_) => expr.setType(TInt)
    case FloatLiteral(_) => expr.setType(TFloat)
    case StringLiteral(_) => expr.setType(TString)
    case BooleanLiteral(_) => expr.setType(TBoolean)
    case UnitLiteral => expr.setType(TUnit)
    
    case Plus(lhs, rhs) =>  
      (typeCheck(lhs, TInt, TFloat), typeCheck(rhs, TInt, TFloat)) match {
        case (TInt, TInt) => expr.setType(TInt)
        case _ => expr.setType(TFloat)
      }
      
    case Minus(lhs, rhs) => 
      (typeCheck(lhs, TInt, TFloat), typeCheck(rhs, TInt, TFloat)) match {
        case (TInt, TInt) => expr.setType(TInt)
        case _ => expr.setType(TFloat)
      }
      
    case Times(lhs, rhs) => 
      (typeCheck(lhs, TInt, TFloat), typeCheck(rhs, TInt, TFloat)) match {
        case (TInt, TInt) => expr.setType(TInt)
        case _ => expr.setType(TFloat)
      }
      
    case Div(lhs, rhs) => 
      typeCheck(lhs, TInt, TFloat)
      typeCheck(rhs, TInt, TFloat)
      expr.setType(TFloat)
  
    case And(lhs, rhs) =>
      typeCheck(lhs, TBoolean)
      typeCheck(rhs, TBoolean)
      expr.setType(TBoolean)
      
    case Or(lhs, rhs) =>
      typeCheck(lhs, TBoolean)
      typeCheck(rhs, TBoolean)
      expr.setType(TBoolean)
      
    case Equals(lhs, rhs) =>
      typeCheck(lhs)
      typeCheck(rhs)
      expr.setType(TBoolean)
      
    case LessThan(lhs, rhs) =>
      typeCheck(lhs, TInt, TFloat)
      typeCheck(rhs, TInt, TFloat)
      expr.setType(TBoolean)
      
    case Not(e) =>
      typeCheck(e, TBoolean)
      expr.setType(TBoolean)

    case Assign(ref, rhs) =>
      typeCheck(rhs, ref.getPongType)
      expr.setType(TUnit)
  }
  
  def typeCheck(expr: Expr, exp: Type*): Type = {
    val t = typeCheck(expr).getType
    if (!exp.toSeq.contains(t)) {
      expr.setType(TError)
      throw TypeCheckException(s"The expression $expr has type $t, expected $exp.")
    }
    t
  }
  
}