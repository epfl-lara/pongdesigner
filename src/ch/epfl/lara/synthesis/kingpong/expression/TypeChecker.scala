package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.rules.Rules._
import android.util.Log
import ch.epfl.lara.synthesis.kingpong.objects.Category
import ch.epfl.lara.synthesis.kingpong.rules.Context

case class TypeCheckException(msg: String) extends Exception(msg)

trait TypeChecker {

  def typeCheck(stat: Stat)(implicit context: Context): Stat = stat match {
    case ParExpr(stats) =>
      stats foreach typeCheck
      stat
      
    case i @ Foreach1(cat, name, rule) =>
      //MIKAEL why inverse the typechecking only for this case ?
      i.typeCheck(this)
      i
      
    case Block(stats)  => 
      stats foreach typeCheck
      stat

    case If(c, s1, s2) => 
      typeCheck(c, TBoolean)
      typeCheck(s1)
      if (s2.isDefined) typeCheck(s2.get)
      stat

    case Assign(props, rhs) =>
      //Log.d("Assign", s"$prop")
      props match {
        case Nil => 
          typeCheck(rhs, TUnit)
        case a::Nil => 
          val t = typeCheck(a).getType
          typeCheck(rhs, t)
        case _ => 
          val t = TTuple(props.map(typeCheck(_).getType))
          typeCheck(rhs, t)
      }
      stat
      
    case Copy(name, o, block) =>
      typeCheck(o, TObject)
      // To typecheck the rest, we replace occurrences of name with the previous object o.
      block.setBinding(name, o.obj)
      typeCheck(block)
      stat
      
    case Delete(name, ref) =>
      typeCheck(ref, TObject)
      stat
      
    case Reset(prop) =>
      stat
      
    case NOP => 
      stat
  }

  def typeCheck(expr: Expr)(implicit context: Context): Expr = expr match {
    case MethodCall(name, args) =>
      val m = context.getMethod(name)
      (args zip m.args) foreach {
        case (arg, Formal(t, _)) => typeCheck(arg, t)
      }
      expr.setType(m.retType)
      
    case NValue(v, index) =>
      if (index > 1 || index < 0) {
        throw new TypeCheckException(s"Inconsistent index in NValue $expr: $index")
      }
      typeCheck(v, TVec2)
      expr.setType(TFloat)
      
    case Count(category: Category) =>
      expr.setType(TInt)
      
    case VecExpr(exprs) =>
      val types = exprs.map(typeCheck(_).getType)
      types match {
        case List(TFloat | TInt, TFloat | TInt) => expr.setType(TVec2)
        case _ => expr.setType(TTuple(types))
      }
      
    case c @ Choose(prop, _) =>
      // At this point, the objects have been determined.
      if(c.evaluatedProgram == null) {
        // Any setBindings have already been called.
        c.evaluatedProgram = ComfusySolver.solve(c.prop, c.getContraintForSolving)
      }
      typeCheck(c.evaluatedProgram)
      
    case IfFunc(cond, ifTrue, ifFalse) =>
      typeCheck(cond, TBoolean)
      val t = typeCheck(ifFalse).getType
      typeCheck(ifTrue, t)
      expr.setType(t)
      
    case ref: PropertyRef => 
      ref.setType(ref.getPongType)
      
    case ref: PropertyIndirect =>
      if(ref.expr != null) {
        val t = typeCheck(ref.expr).getType
        ref.setType(t)
      } else if(ref.obj != null) {
        val t = typeCheck(ref.obj.get(ref.prop)).getType
        ref.setType(t)
      } else {
        typeCheck(ref.indirectObject, TObject)
        ref.prop match {
          case "x" | "y" | "radius" | "angle" | "width" | "height" => expr.setType(TFloat)
          case "velocity" => expr.setType(TVec2)
          case "color" => expr.setType(TInt)
          case "picture" => expr.setType(TString)
          case "visible" => expr.setType(TBoolean)
          case "value" => expr.setType(TInt)
          case _ => //MIKAEL really nothing here ? 
        }
        ref
      }
      
    case IntegerLiteral(_) => expr.setType(TInt)
    case FloatLiteral(_) => expr.setType(TFloat)
    case StringLiteral(_) => expr.setType(TString)
    case ObjectLiteral(_) => expr.setType(TObject)
    case BooleanLiteral(_) => expr.setType(TBoolean)
    case Vec2Literal(_, _) => expr.setType(TVec2)
    case UnitLiteral => expr.setType(TUnit)
    case FingerCoordX1 => expr.setType(TFloat)
    case FingerCoordX2 => expr.setType(TFloat)
    case FingerCoordY1 => expr.setType(TFloat)
    case FingerCoordY2 => expr.setType(TFloat)
    case GameObjectRef(_) => expr.setType(TObject)
    case On(_) => expr.setType(TBoolean)
    case Once(_) => expr.setType(TBoolean)
    case Val(_) => expr.setType(TFloat)
    
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
  
  def typeCheck(expr: Expr, exp: Type*)(implicit context: Context): Type = {
    val t = typeCheck(expr).getType
    if (exp.forall(!_.accept(t))) {
      expr.setType(TError)
      throw TypeCheckException(s"The expression $expr has type $t, expected $exp.")
    }
    t
  }
  
}