package ch.epfl.lara.synthesis.kingpong.expression

import scala.collection.mutable.{HashMap => MMap}

import android.util.Log

import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.rules.Context
import ch.epfl.lara.synthesis.kingpong.rules.Events._
import ch.epfl.lara.synthesis.kingpong.objects.Property
import ch.epfl.lara.synthesis.kingpong.objects.AssignableProperty
import ch.epfl.lara.synthesis.kingpong.objects.HistoricalProperty
import ch.epfl.lara.synthesis.kingpong.objects.AssignableProperty

case class InterpreterException(msg: String) extends Exception(msg)

trait Interpreter {
  
  /** Initialize the recursive context. */
  def initRC(): RecContext
  
  /** Initialize the global context. */
  def initGC(): Context
  
  object NumericLiteral {
    def unapply(expr: Expr): Option[Float] = expr match {
      case IntegerLiteral(i) => Some(i.toFloat)
      case FloatLiteral(f)   => Some(f)
      case _ => None
    }
  }
  
  trait RecContext {
    def withNewVar(id: Identifier, e: Expr): RecContext
    def get(id: Identifier): Option[Expr]
  } 
  
  object MutableRecContext {
    def empty = new MutableRecContext(MMap.empty)
  }
  
  class MutableRecContext(mappings: MMap[Identifier, Expr]) extends RecContext {
    def withNewVar(id: Identifier, e: Expr) = {
      mappings += (id -> e)
      this
    }
    def get(id: Identifier) = mappings.get(id)
  }
  
  object ImmutableRecContext {
    val empty = ImmutableRecContext(Map.empty)
  }
  
  case class ImmutableRecContext(mappings: Map[Identifier, Expr]) extends RecContext {
    def withNewVar(id: Identifier, e: Expr) = ImmutableRecContext(mappings + (id -> e))
    def get(id: Identifier) = mappings.get(id)
  }
  
  def evaluate(expr: Expr): Expr = eval(expr)(initGC(), initRC())
  
  /**
   * Used to extract values, especially for Method calls
   */
  object Eval {
    def unapply[T](e: Expr)(implicit gctx: Context, rctx: RecContext): Option[T] = {
      eval(e) match {
        case e: Literal[T] => Some(e.value)
        case _ => None
      }
    }
  }
  
  private val eval_vec2 = Vec2(0, 0)
  
  private def eval(expr: Expr)(implicit gctx: Context, rctx: RecContext): Expr = expr match {
    
    case lit: Literal[_] => lit
    
    case Variable(id) =>
      rctx.get(id) match {
        case Some(e) => e
        case None => throw InterpreterException(s"No value for identifier ${id} in mapping.")
      }
  
    case Let(id, value, body) =>
      val first = eval(value)
      eval(body)(gctx, rctx.withNewVar(id, first))
      
    case Select(e, propertyName) =>
      eval(e).as[GameObject].get(propertyName).getExpr

    case Debug(message, exprs) =>
      val formatter = new java.util.Formatter()
      formatter.format(message, exprs.map(eval).map(_.toString): _*)
      Log.d("kingpong", formatter.toString)
      UnitLiteral
      
    case ParExpr(a :: _) =>
      eval(a)
      
    case ParExpr(Nil) =>
      UnitLiteral
      
    case Foreach(cat, id, body) =>
      cat.objects.foreach { o =>
        eval(body)(gctx, rctx.withNewVar(id, o.expr))
      }
      UnitLiteral
      
    case Forall(cat, id, body) =>
      val res = cat.objects.forall { o =>
        eval(body)(gctx, rctx.withNewVar(id, o.expr)).as[Boolean]
      }
      BooleanLiteral(res)
      
    case Find(cat, id, body) =>
      val res = cat.objects.find { o =>
        eval(body)(gctx, rctx.withNewVar(id, o.expr)).as[Boolean]
      }
      ObjectLiteral(res.getOrElse(null))
      
    case Block(stats) => 
      stats foreach eval
      UnitLiteral

    case If(c, s1, s2) => 
      // TODO return UnitLiteral if the else part is NOP ? 
      eval(c) match {
        case BooleanLiteral(true)  => eval(s1)
        case BooleanLiteral(false) => eval(s2)
        case v => throw InterpreterException(s"The if condition $c is not a boolean but is $v.")
      }

    case Assign(prop, rhs) =>
      eval(prop._1).as[GameObject].get(prop._2) match {
        case assignable: AssignableProperty[_] => assignable.assign(eval(rhs))
        case p => throw InterpreterException(s"The property $p is not assignable but is in $expr")
      }
      UnitLiteral

    case Copy(obj, id, block) =>
      val o = eval(obj).as[GameObject]
      val freshName = gctx.getNewName(id.name)
      val fresh = o.getCopy(freshName)
      fresh.creationTime.setInit(gctx.time)
      gctx.add(fresh)
      
      eval(block)(gctx, rctx.withNewVar(id, fresh.expr))
      UnitLiteral
      
    case Delete(obj) =>
      val o = eval(obj).as[GameObject]
      o.deletionTime.setNext(gctx.time)
      UnitLiteral
      
    case m @ MethodCall(name, args) =>
      (name, args) match {
        case ("importDrawings", ObjectLiteral(o:DrawingObject)::from::to::width::color::Nil) =>
          (eval(from), eval(to), eval(width), eval(color)) match {
            case (Tuple(Seq(FloatLiteral(fromx), FloatLiteral(fromy))), Tuple(Seq(FloatLiteral(tox), FloatLiteral(toy))), FloatLiteral(width), IntegerLiteral(color)) =>
              o.addDrawingElement(gctx.time, fromx, fromy, tox, toy, width, color)
            case _ =>
          }
          UnitLiteral
        case ("gamePixelsPerUnit", ObjectLiteral(o:DrawingObject)::Nil) =>
          FloatLiteral(o.game.pixelsByUnit)
        case _ =>
          error(expr)
      }

    case Tuple(exprs) =>
      Tuple(exprs map eval)
      
    case TupleSelect(t, index) => 
      val Tuple(exprs) = eval(t)
      exprs(index - 1)
        
    case Count(c) => 
      IntegerLiteral(c.objects.size)
    
    case c: Choose =>
      eval(c.evaluatedProgram)
      
    case Plus(lhs, rhs) => eval(lhs) match {
      case IntegerLiteral(v1) => eval(rhs) match {
        case IntegerLiteral(v2) => IntegerLiteral(v1 + v2)
        case FloatLiteral(v2) => FloatLiteral(v1 + v2)
        case _ => error(expr)
      }
      case FloatLiteral(v1) => eval(rhs) match {
        case NumericLiteral(v2) => FloatLiteral(v1 + v2)
        case _ => error(expr)
      }
      case Tuple(exprs) => eval(rhs) match {
        case Tuple(exprs2) if exprs.size == exprs2.size => 
          val l = (exprs zip exprs2) map { case (e1, e2) => 
            eval(Plus(e1, e2))
          }
          Tuple(l)
        case _ => error(expr)
      }
      case StringLiteral(v) => eval(rhs) match {
        case StringLiteral(v2) => StringLiteral(v + v2) 
        case _ => error(expr)
      }
      case _ => error(expr)
    }
      
    case Minus(lhs, rhs) => eval(lhs) match {
      case IntegerLiteral(v1) => eval(rhs) match {
        case IntegerLiteral(v2) => IntegerLiteral(v1 - v2)
        case FloatLiteral(v2) => FloatLiteral(v1 - v2)
        case _ => error(expr)
      }
      case FloatLiteral(v1) => eval(rhs) match {
        case NumericLiteral(v2) => FloatLiteral(v1 - v2)
        case _ => error(expr)
      }
      case Tuple(exprs) => eval(rhs) match {
        case Tuple(exprs2) if exprs.size == exprs2.size => 
          val l = (exprs zip exprs2) map { case (e1, e2) => 
            eval(Minus(e1, e2))
          }
          Tuple(l)
        case _ => error(expr)
      }
      case _ => error(expr)
    }
      
    case Times(lhs, rhs) => eval(lhs) match {
      case num @ IntegerLiteral(v1) => eval(rhs) match {
        case IntegerLiteral(v2) => IntegerLiteral(v1 * v2)
        case FloatLiteral(v2) => FloatLiteral(v1 * v2)
        case Tuple(exprs) => Tuple(exprs.map(e => eval(Times(num, e))))
        case _ => error(expr)
      }
      case num @ FloatLiteral(v1) => eval(rhs) match {
        case NumericLiteral(v2) => FloatLiteral(v1 * v2)
        case Tuple(exprs) => Tuple(exprs.map(e => eval(Times(num, e))))
        case _ => error(expr)
      }
      case Tuple(exprs) => eval(rhs) match {
        case num @ NumericLiteral(_) => Tuple(exprs.map(e => eval(Times(num, e))))
        case _ => error(expr)
      }
      case _ => error(expr)
    }
      
    case Div(lhs, rhs) => eval(lhs) match {
      case IntegerLiteral(v1) => eval(rhs) match {
        case NumericLiteral(v2)=>  FloatLiteral(v1 / v2)
        case _ => error(expr)
      }
      case FloatLiteral(v1) => eval(rhs) match {
        case NumericLiteral(v2) => FloatLiteral(v1 / v2)
        case _ => error(expr)
      }
      case _ => error(expr)
    }

    case Mod(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (IntegerLiteral(v1), IntegerLiteral(v2)) => IntegerLiteral(v1 % v2)
        case (FloatLiteral(v1), FloatLiteral(v2)) => FloatLiteral(v1 % v2)
        case _ => throw InterpreterException(s"The mod operation cannot be applied on the two expressions $lhs and $rhs.")
      }
  
    case And(lhs, rhs) =>
      eval(lhs) match {
        case BooleanLiteral(false) => booleanLiteralFalse
        case BooleanLiteral(true) => eval(rhs)
        case _ => error(expr)
      }
      
    case Or(lhs, rhs) =>
      eval(lhs) match {
        case BooleanLiteral(true) => booleanLiteralTrue
        case BooleanLiteral(false) => eval(rhs)
        case _ => error(expr)
      }
      
    case Equals(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericLiteral(v1), NumericLiteral(v2)) => Literal(v1 == v2) // permits to have 1 == 1f
        case (lit1: Literal[_], lit2: Literal[_]) => Literal(lit1 == lit2)
        case (Tuple(l1), Tuple(l2)) => BooleanLiteral(l1.size == l2.size && (true /: (l1 zip l2)){ case (b, (t1, t2)) => b && (t1 == t2) })
        case _ => booleanLiteralFalse
      }
      
    case LessThan(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericLiteral(v1), NumericLiteral(v2)) => Literal(v1 < v2)
        case _ => throw InterpreterException(s"A LessThan is not possible between $lhs and $rhs.")
      }

    case LessEq(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericLiteral(v1), NumericLiteral(v2)) => Literal(v1 <= v2)
        case _ => throw InterpreterException(s"A LessEq is not possible between $lhs and $rhs.")
      }

    case GreaterThan(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericLiteral(v1), NumericLiteral(v2)) => Literal(v1 > v2)
        case _ => throw InterpreterException(s"A GreaterThan is not possible between $lhs and $rhs.")
      }

    case GreaterEq(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericLiteral(v1), NumericLiteral(v2)) => Literal(v1 >= v2)
        case _ => throw InterpreterException(s"A GreaterEq is not possible between $lhs and $rhs.")
      }
      
    case Not(e) =>
      eval(e) match {
        case BooleanLiteral(v) => Literal(!v)
        case _ => throw InterpreterException(s"A Not is not possible on $e.")
      }

    case FingerMoveOver(expr, id, block) =>
      val obj = eval(expr).as[GameObject]
      val moves = gctx.fingerMoves(_.obj.exists(_ == obj))
      val from = moves.headOption.map(_.from)
      if (from.isDefined) {
        // The block is NOP if the expression is only used for its returned value.
        // No need to instantiate a new RecContext.
        if (block != NOP) {
          val to = moves.last.to
          eval(block)(gctx, rctx.withNewVar(id, Tuple(Seq(from.get, to))))
        }
        booleanLiteralTrue
      } else {
        booleanLiteralFalse
      }
      
    case FingerDownOver(e) =>
      eval(e) match {
        case ObjectLiteral(o) =>
          Literal(gctx.existsFingerDown(_.obj.exists(_ == o)))
        case _ => error(expr)
      }

    case FingerUpOver(e) =>
      eval(e) match {
        case ObjectLiteral(o) =>
          Literal(gctx.existsFingerUp(_.obj.exists(_ == o)))
        case _ => error(expr)
      }

    case Collision(lhs, rhs) =>
       (eval(lhs), eval(rhs)) match {
        case (ObjectLiteral(o1), ObjectLiteral(o2)) => 
          val isCollision = gctx.existsBeginContact { c =>
            (c.objectA == o1 && c.objectB == o2) ||
            (c.objectA == o2 && c.objectB == o1)
          }
          Literal(isCollision)
        case _ => error(expr)
      }
      
    case Contains(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (ObjectLiteral(null), _) => booleanLiteralFalse
        case (ObjectLiteral(o1), ObjectLiteral(o2: Positionable)) => Literal(o1.contains(o2.center.get))
        case (ObjectLiteral(o1), Tuple(Seq(NumericLiteral(x), NumericLiteral(y)))) =>
          eval_vec2.x = x
          eval_vec2.y = y
          Literal(o1.contains(eval_vec2))
        case _ => error(expr)
      }
      
    case ContainingCell(e1, e2) =>
      (eval(e1), eval(e2)) match {
        case (ObjectLiteral(array: Array2D), ObjectLiteral(obj: Positionable)) => 
          array.containingCell(obj)
        case _ => error(expr)
      } 
      
    case Row(e) =>
      eval(e) match {
        case ObjectLiteral(o: Cell) => IntegerLiteral(o.row)
        case _ => error(expr)
      }
      
    case Column(e) =>
      eval(e) match {
        case ObjectLiteral(o: Cell) => IntegerLiteral(o.column)
        case _ => error(expr)
      }
      
    case Apply(e1, e2, e3) =>
      eval(e1) match {
        case ObjectLiteral(o: Array2D) =>
          val col = eval(e2).as[Int]
          val row = eval(e3).as[Int]
          if (col >= 0 && col < o.numColumns.get && row >= 0 && row < o.numRows.get) {
            o.cells(col)(row).expr
          } else {
            ObjectLiteral.empty
          }
        case _ => error(expr)
      }

    case NOP => UnitLiteral
      
    case null =>
      Log.d("kingpong", "Interpreter: Erreur while evaluating null")
      error(expr)
  }
  
  private def error(expr: Expr): Nothing = {
    throw InterpreterException(s"A TypeCheck error occurs during interpretation of $expr.")
  }
}