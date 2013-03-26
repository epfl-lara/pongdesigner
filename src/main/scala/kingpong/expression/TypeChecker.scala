package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.rules.Rules._

case class TypeCheckException(msg: String) extends Exception(msg)

trait TypeChecker {
  
  def typeCheck(rule: Rule): Rule = {
    typeCheck(rule.cond, TBoolean)
    typeCheck(rule.action)
    rule
  } 

  def typeCheck(stat: Stat): Stat = stat match {
    case Block(stats)  => 
      stats foreach typeCheck
      stat

    case If(c, s1, s2) => 
      typeCheck(c, TBoolean)
      typeCheck(s1)
      typeCheck(s2)
      stat

    case Assign(prop, rhs) =>
      typeCheck(rhs, prop.getPongType)
      stat

    case NOP => stat//Do nothing, it typechecks
  }

  def typeCheck(expr: Expr): Expr = expr match {

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

    case FingerMoveOver(c) => expr.setType(TBoolean)
    case FingerDownOver(c) => expr.setType(TBoolean)
    case FingerUpOver(c) => expr.setType(TBoolean)
    case Collision(c1, c2) => expr.setType(TBoolean)
  }
  
  def typeCheck(expr: Expr, exp: Type*): Type = {
    val t = typeCheck(expr).getType
    if (exp.forall(!_.accept(t))) {
      expr.setType(TError)
      throw TypeCheckException(s"The expression $expr has type $t, expected $exp.")
    }
    t
  }
  
}