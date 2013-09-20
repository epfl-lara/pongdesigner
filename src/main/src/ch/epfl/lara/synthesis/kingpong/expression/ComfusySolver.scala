package ch.epfl.lara.synthesis.kingpong.expression

import Trees._
import scala.collection.mutable.Map

object ComfusySolver {
  import ch.epfl.lara.synthesis.kingpong.common.Implicits._
  
  type Name = String
  type Coef = Float
  
  def name(g: (Name, Coef)): Name = g._1
  def coef(g: (Name, Coef)): Coef = g._2
  
  implicit def string2name(s: String): Name = s
  implicit def float2coef(s: Float): Coef = s
  
  sealed trait ComExpr {}
  
  /* Sorted list of names and coefs*/
  case class Sum(const: Expr, e: List[(Name, Coef)]) extends ComExpr {
    def merge(l1: List[(Name, Coef)], l2: List[(Name, Coef)])(implicit f: (Coef, Coef) => Coef): List[(Name, Coef)] = (l1, l2) match {
      case (Nil, Nil) => Nil
      case (l, Nil) => l
      case (Nil, l) => l
      case (a::b, c::d) if name(a) == name(c) => val merged = f(coef(a), coef(c))
        if(merged == 0) merge(b, d) else (name(a), merged)::merge(b, d)
      case (a::b, c::d) if name(a) < name(c) => a::merge(b, l2)
      case (a::b, c::d) => c::merge(l1, d)
    }
    def map(l1: List[(Name, Coef)])(f: Coef => Coef):List[(Name, Coef)] = l1 match {
      case Nil => Nil
      case a::q => val mapped = f(coef(a))
        if(mapped == 0) map(q)(f) else (name(a),mapped)::map(q)(f)
    }
    def getCoef(e: Expr): Float = e match {
        case FloatLiteral(e) => e
        case IntegerLiteral(e) =>e
        case _ => 0
      }
    def +(other: Sum): Sum = {
      Sum(const + other.const, merge(e, other.e)(_ + _))
    }
    def -(other: Sum): Sum = {
      Sum(const - other.const, merge(e, map(other.e)(_ * (-1)))(_ + _))
    }
    def *(other: Sum): Sum = {
      val coefconst = getCoef(const)
      val coefconst2 = getCoef(other.const)
      Sum(const * other.const, merge(map(e)(_*coefconst2), map(other.e)(_*coefconst))(_ + _)) // Only keep linear terms. TODO : better check
    }
    def *(coefconst2: Float): Sum = {
      this * Sum(coefconst2, Nil)
    }
    def /(other: Sum): Sum = {
      val coefconst2 = getCoef(other.const)
      Sum(const / other.const, map(e)(_ / coefconst2))
    }
    def /(coefconst2: Float): Sum = {
      this / Sum(coefconst2, Nil)
    }
    def %(other: Sum): Sum = {
      val coefconst2 = getCoef(other.const)
      Sum(const % other.const, map(e)(_ % coefconst2))
    }
    def solve(a: Name): Sum = {
      val coefA = coefOfName(a)
      val withoutA = removeA(a)
      withoutA / (-coefA)
    }
    def replace(a: Name, replacement: Sum): Sum = {
      val coefA = coefOfName(a)
      val withoutA = removeA(a)
      withoutA + replacement * coefA
    }
    
    def coefOfName(a: Name): Coef = e.foldLeft(0f){case (res, elem) => if(name(elem) == a) res + coef(elem) else res }
    def removeA(a: Name): Sum = {
      Sum(const, e.filterNot(name(_) == a))
    }
  }
  case class Conjunct(lessEqThanZero: List[Sum], equalZero: List[Sum]) extends ComExpr {
    def &&(other: Conjunct): Conjunct = Conjunct(lessEqThanZero ++ other.lessEqThanZero, equalZero ++ other.equalZero)
  }
  final val True = Conjunct(Nil, Nil)
  case class IfExpr(cond: Conjunct, ifTrue: ComExpr, ifFalse: ComExpr) extends ComExpr
  
  implicit class RichTupleSum(t: (ComExpr, ComExpr)) {
    def asSum(f: (Sum, Sum) => ComExpr) = t match {
      case (s1: Sum, s2: Sum) => f(s1, s2)
      case _ => throw new Error(s"$t cannot be canonized")
    }
    def asConj(f: (Conjunct, Conjunct) => ComExpr) = t match {
      case (s1: Conjunct, s2: Conjunct) => f(s1, s2)
      case _ => throw new Error(s"$t cannot be canonized")
    }
  }
  
  /**
   * Put the input constraint into canonization form.
   * ... <= 0  and ... === 0 where variables are sorted alphabetically
   */
  def canonize[T](expr: Expr)(implicit context: Map[Name, Expr]): ComExpr = expr match {
    case Plus(lhs: Expr, rhs: Expr) => (canonize(lhs),canonize(rhs)) asSum (_ + _)
    case Minus(lhs: Expr, rhs: Expr) => (canonize(lhs),canonize(rhs)) asSum (_ - _)
    case Times(lhs: Expr, rhs: Expr) => (canonize(lhs),canonize(rhs)) asSum (_ * _)
    case Div(lhs: Expr, rhs: Expr) => (canonize(lhs),canonize(rhs)) asSum (_ / _)
    case Mod(lhs: Expr, rhs: Expr) => (canonize(lhs),canonize(rhs)) asSum (_ % _)
  
    case And(lhs: Expr, rhs: Expr) => (canonize(lhs), canonize(rhs)) asConj (_ && _)
    case Or(lhs: Expr, rhs: Expr) => throw new Error(s"$expr cannot be canonized because it contains an Or")
    case Equals(lhs: Expr, rhs: Expr) => (canonize(lhs),canonize(rhs)) asSum { (lhs: Sum, rhs: Sum) => Conjunct(Nil, List(lhs - rhs)) }
    case LessThan(lhs: Expr, rhs: Expr) => (canonize(lhs),canonize(rhs)) asSum { (lhs: Sum, rhs: Sum) => Conjunct(List(lhs - rhs), Nil) }
    case LessEq(lhs: Expr, rhs: Expr) => (canonize(lhs),canonize(rhs)) asSum { (lhs: Sum, rhs: Sum) => Conjunct(List(lhs - rhs), Nil) }
    case GreaterThan(lhs: Expr, rhs: Expr) => (canonize(lhs),canonize(rhs)) asSum { (lhs: Sum, rhs: Sum) => Conjunct(List(rhs - lhs), Nil) }
    case GreaterEq(lhs: Expr, rhs: Expr) => (canonize(lhs),canonize(rhs)) asSum { (lhs: Sum, rhs: Sum) => Conjunct(List(rhs - lhs), Nil) }
    case Not(o: Expr) => throw new Error(s"$expr cannot be canonized because it contains a Not")
    
    case BooleanLiteral(true) => Conjunct(Nil, Nil)
    case BooleanLiteral(false) => Conjunct(Nil, List(Sum(1, Nil)))
    case Choose(_, _) => throw new Error(s"$expr cannot be canonized because it contains a Choose")
    case Collision(_, _) => throw new Error(s"$expr cannot be canonized because it contains a Collision")
    case FingerCoordX1 => Sum(0, List(("FingerCoordX1", 1)))
    case FingerCoordX2 => Sum(0, List(("FingerCoordX2", 1)))
    case FingerCoordY1 => Sum(0, List(("FingerCoordY1", 1)))
    case FingerCoordY2 => Sum(0, List(("FingerCoordY2", 1)))
    case FingerDownOver(_) => throw new Error(s"$expr cannot be canonized because it contains a FingerDownOver")
    case FingerMoveOver(_) => throw new Error(s"$expr cannot be canonized because it contains a FingerMoveOver")
    case FingerUpOver(_) => throw new Error(s"$expr cannot be canonized because it contains a FingerUpOver")
    case FloatLiteral(f) => Sum(f, Nil)
    case GameObjectRef(_, _) => throw new Error(s"$expr cannot be canonized because it contains a GameObjectRef")
    case IfFunc(_, _, _) => throw new Error(s"$expr cannot be canonized because it contains a IfFunc")
    case IntegerLiteral(f) => Sum(f, Nil)
    case On(e) => canonize(e)
    case Once(e) => canonize(e)
    case PropertyIndirect(GameObjectRef(name, obj), prop) => val n = name + "."  + prop
      context(n) = expr
      Sum(0, List((n, 1)))
    case PropertyRef(prop) => val n = prop.parent.name.get + "."  + prop.name
      context(n) = expr
      Sum(0, List((n, 1)))
    case StringLiteral(_) => throw new Error(s"$expr cannot be canonized because it contains a String")
    case UnitLiteral => throw new Error(s"$expr cannot be canonized because it contains a UnitLiteral")
    case Val(f) => Sum(0, List((f, 1)))
    case Vec2Expr(_, _) => throw new Error(s"$expr cannot be canonized because it contains a Vec2Expr")
    case Vec2Literal(_, _) => throw new Error(s"$expr cannot be canonized because it contains a Vec2Literal")
    case VecExpr(_) => throw new Error(s"$expr cannot be canonized because it contains a VecExpr")
  }
  /**
   * Translates the expression back to expr
   */
  def translate(input: ComExpr)(implicit context: Map[Name, Expr]): Expr = input match {
    case Sum(n, Nil) => n
    case Sum(const, l) => val u = l.foldLeft(const){ case (res, a) => 
      val c = coef(a)
      val n = context(name(a))
      (if(c == 1) res + n else if(c == -1) res - n else if(c == 0) res else res + (n * c)) }
      u
    case Conjunct(Nil, Nil) => BooleanLiteral(true)
    case Conjunct(lessEq, eq) =>
      val l1 = lessEq.map(translate).map(LessEq(_, 0)) ++ (eq.map(translate).map(Equals(_, 0)))
      l1 match {
        case Nil => UnitLiteral
        case a::Nil => a
        case l => l.reduceLeft(And(_, _))
      }
    case IfExpr(c, ifTrue, ifFalse) => IfFunc(translate(c), translate(ifTrue), translate(ifFalse))
  }
  
  /**
   * Returns the precondition and the program solving the given variables.
   */
  def solveCanonized(output_variables: List[Name], constraint: Conjunct)(implicit context: Map[Name, Expr]): (ComExpr, List[ComExpr]) = {
    println(s"problem: $output_variables, constraint = $constraint")
    
    output_variables match {
      case Nil => (constraint, Nil)
      case a::Nil => // Simply solve the first equation containing a. If inequalities, nest the constraints.
        val Conjunct(lessEq, eq) = constraint
        val variable_a = Sum(0, List((a, 1)))
        eq.find(_.e exists (name(_) == a)) match {
          case Some(s@Sum(const, list)) =>
            // Solve the equality for a.
            val coefA = s.coefOfName(a)
            val withoutA = s.removeA(a)
            val res = withoutA / (-coefA)
            (Conjunct(Nil, Nil), List(res))
          case None =>// Maybe inequalities ?
            lessEq.find(_.e exists (name(_) == a)) match {
              case ineq@Some(s@Sum(const, list)) =>
                val coefA = s.coefOfName(a)
                val withoutA = s.removeA(a)
                val bound = withoutA / (-coefA)
                // if(coefA <= 0, invert the sign of the inequality)
                val condition = Conjunct(s::Nil, Nil)
                //(True, List(IfExpr(condition, variable_a, bound)))
                solveCanonized(output_variables, Conjunct(lessEq.filterNot(_ == s), Nil)) match {
                  case (_, List(i)) =>
                    (True, List(IfExpr(condition, i, bound)))
                  case e =>
                    println(e)
                    (constraint, Nil)
                }
              case None =>
                (True, List(variable_a))
            }
            
        }
      case (a::q) => // Multiple variables.
        // solve equalities only.
        val Conjunct(lessEq, eqs) = constraint
        val variable_a = Sum(0, List((a, 1)))
        eqs.find(_.e exists (name(_) == a)) match {
          case Some(s@Sum(const, list)) =>
            val coefA = s.coefOfName(a)
            val withoutA = s.removeA(a)
            val res = withoutA / (-coefA)
            val eqsWithoutFirst = eqs.filterNot(_ == s)
            val eqs_replaced = eqsWithoutFirst.map(_.replace(a, res))
            val (condition, l) = solveCanonized(q, Conjunct(lessEq, eqs_replaced)) // Solve given one less variable
            // Now replace the value into the res.
            val newvalue = (q zip l).foldLeft(res){ case (expr, (n, v: Sum)) => expr.replace(n, v) case (expr, _) => expr }
            (condition, newvalue::l)
            
          case None => // No constraint on a, yeepee !!
            val (condition, res) = solveCanonized(q, constraint) // Solve given one less variable
            (condition, variable_a::res)
        }
    }
  }
  
  def solve(o: VecExpr, constraint: Expr): Expr = {
    val output_variables = o.l.map{
      case p:PropertyIndirect => p.name + "." + p.prop
      case p@PropertyRef(prop) => prop.parent.name.get + "." + prop.name
      case e => e.toString
    }
    implicit val context = Map[Name, Expr]()
    canonize(constraint) match {
      case canonized:Conjunct =>
        val result = solveCanonized(output_variables, canonized)
        result._2.map(translate(_)) match {
          case Nil => UnitLiteral
          case a::Nil => a
          case l => VecExpr(l)
        }
      case e => throw new Exception(s"Not possible to solve $e")
    }
  }
}