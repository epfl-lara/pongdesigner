package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.rules.Rules._
import android.util.Log

case class TypeCheckException(msg: String) extends Exception(msg)

trait TypeChecker {
  
  def typeCheck(iterator: RuleIterator): RuleIterator = {
    iterator.typeCheck(this)
    iterator
  }

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
      //Log.d("Assign", s"$prop")
      prop match {
        case prop:PropertyRef => typeCheck(rhs, prop.getPongType)
        case prop:PropertyIndirect =>
          val t = typeCheck(prop).getType
          typeCheck(rhs, t)
      }
      stat
      
    case Copy(name, o, block) =>
      typeCheck(o, TObject)
      // To typecheck the rest, we replaces occurences of name with the previous object o.
      block.setBinding(name, o.obj)
      typeCheck(block)
      stat
      
    case Reset(prop) =>
      stat
      
    case NOP => stat//Do nothing, it typechecks
  }

  def typeCheck(expr: Expr): Expr = expr match {
    case c@Choose(prop, constraint) =>
      // At this point, the objects have been determined.
      if(c.evaluatedProgram == null) {
        // Any setBindings have already been called.
        c.evaluatedProgram = c.solve(c.constraint)
      }
      typeCheck(c.evaluatedProgram)
    case IfFunc(cond, ifTrue, ifFalse) =>
      typeCheck(cond, TBoolean)
      val t= typeCheck(ifFalse).getType
      typeCheck(ifTrue, t)
      expr.setType(t)
    case ref: PropertyRef => ref.setType(ref.getPongType)
    case ref: PropertyIndirect =>
      val t = typeCheck(ref.expr).getType
      ref.setType(t)
    case IntegerLiteral(_) => expr.setType(TInt)
    case FloatLiteral(_) => expr.setType(TFloat)
    case StringLiteral(_) => expr.setType(TString)
    case BooleanLiteral(_) => expr.setType(TBoolean)
    case Vec2Literal(_, _) => expr.setType(TVec2)
    case UnitLiteral => expr.setType(TUnit)
    case FingerCoordX1 => expr.setType(TFloat)
    case FingerCoordX2 => expr.setType(TFloat)
    case FingerCoordY1 => expr.setType(TFloat)
    case FingerCoordY2 => expr.setType(TFloat)
    case GameObjectRef(_, _) => expr.setType(TObject)
    case On(_) => expr.setType(TBoolean)
    case Once(_) => expr.setType(TBoolean)
    case Val(_) => expr.setType(TFloat)
    case Vec2Expr(lhs, rhs) =>
      (typeCheck(lhs, TInt, TFloat): @unchecked) match {
        case TInt => (typeCheck(rhs, TInt, TFloat): @unchecked) match {
          case TInt => expr.setType(TVec2)
          case TFloat => expr.setType(TVec2)
        }
        case TFloat => 
          typeCheck(rhs, TInt, TFloat)
          expr.setType(TVec2)
      }
    case Plus(lhs, rhs) =>
      (typeCheck(lhs, TInt, TFloat, TVec2): @unchecked) match {
        case TInt => (typeCheck(rhs, TInt, TFloat): @unchecked) match {
          case TInt => expr.setType(TInt)
          case TFloat => expr.setType(TFloat)
        }
        case TFloat => 
          typeCheck(rhs, TInt, TFloat)
          expr.setType(TFloat)
        case TVec2 =>
          typeCheck(rhs, TVec2)
          expr.setType(TVec2)
      }

    case Minus(lhs, rhs) =>
      (typeCheck(lhs, TInt, TFloat, TVec2): @unchecked) match {
        case TInt => (typeCheck(rhs, TInt, TFloat): @unchecked) match {
          case TInt => expr.setType(TInt)
          case TFloat => expr.setType(TFloat)
        }
        case TFloat =>
          typeCheck(rhs, TInt, TFloat)
          expr.setType(TFloat)
        case TVec2 =>
          typeCheck(rhs, TVec2)
          expr.setType(TVec2)
      }
      
    case Times(lhs, rhs) =>
      (typeCheck(lhs, TInt, TFloat, TVec2): @unchecked) match {
        case TInt => (typeCheck(rhs, TInt, TFloat, TVec2): @unchecked) match {
          case TInt => expr.setType(TInt)
          case TFloat => expr.setType(TFloat)
          case TVec2 => expr.setType(TVec2)
        }
        case TFloat => (typeCheck(rhs, TInt, TFloat, TVec2): @unchecked) match {
          case TInt | TFloat => expr.setType(TFloat)
          case TVec2 => expr.setType(TVec2)
        }
        case TVec2 =>
          typeCheck(rhs, TInt, TFloat)
          expr.setType(TVec2)
      }
      
    case Div(lhs, rhs) =>
      (typeCheck(lhs, TInt, TFloat, TVec2): @unchecked) match {
        case TInt | TFloat =>
          typeCheck(rhs, TInt, TFloat)
          expr.setType(TFloat)
        case TVec2 =>
          typeCheck(rhs, TInt, TFloat)
          expr.setType(TVec2)
      }
  
    case Mod(lhs, rhs) => 
      (typeCheck(lhs, TInt, TFloat): @unchecked) match {
        case TInt => (typeCheck(rhs, TInt, TFloat): @unchecked) match {
          case TInt => expr.setType(TInt)
          case TFloat => expr.setType(TFloat)
        }
        case TFloat =>
          typeCheck(rhs, TInt, TFloat)
          expr.setType(TFloat)
      }

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

    case LessEq(lhs, rhs) =>
      typeCheck(lhs, TInt, TFloat)
      typeCheck(rhs, TInt, TFloat)
      expr.setType(TBoolean)

    case GreaterThan(lhs, rhs) =>
      typeCheck(lhs, TInt, TFloat)
      typeCheck(rhs, TInt, TFloat)
      expr.setType(TBoolean)

    case GreaterEq(lhs, rhs) =>
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