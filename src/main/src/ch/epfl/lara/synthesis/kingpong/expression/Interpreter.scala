package ch.epfl.lara.synthesis.kingpong.expression

import android.util.Log
import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.objects.GameObject
import ch.epfl.lara.synthesis.kingpong.rules.Context
import ch.epfl.lara.synthesis.kingpong.rules.Events._

sealed trait Value extends Any {
  def as[T : PongType]: T = implicitly[PongType[T]].toScalaValue(this)
}
  
case class IntV(value: Int) extends AnyVal with Value
case class FloatV(value: Float) extends AnyVal with Value
case class StringV(value: String) extends AnyVal with Value
case class BooleanV(value: Boolean) extends AnyVal with Value
case class Vec2V(x: Float, y: Float) extends Value
case object UnitV extends Value
case class GameObjectV(value: GameObject) extends AnyVal with Value
case class TupleV(value: List[Value]) extends AnyVal with Value

object NumericV {
  def unapply(value: Value): Option[Float] = value match {
    case IntV(i) => Some(i.toFloat)
    case FloatV(f) => Some(f)
    case _ => None
  }
}

case class InterpreterException(msg: String) extends Exception(msg)

trait Interpreter {
  
  def eval(stat: Stat)(implicit context: Context): Unit = stat match {
    case ParExpr(a::l) =>
      eval(a)
    case ParExpr(Nil) =>
    case f @ Foreach1(cat, name, r) =>
      f.evaluate(this)
    case Block(stats) => 
      stats map eval

    case If(c, s1, s2) => 
      eval(c) match {
        case BooleanV(true)  => eval(s1)
        case BooleanV(false) => eval(s2)
        case v => throw InterpreterException(s"The if condition $c is not a boolean but is $v.")
      }

    case a@Assign(prop, rhs) =>
      def setValue(prop: Expr, v: Value) = 
        prop match {
        case prop:PropertyRef => prop.setNext(v)
          context.addAssignment(a, prop)
          //context.set(prop.property.fullname, v)
        case prop:PropertyIndirect => prop.expr match {
          case prop:PropertyRef => prop.setNext(v)
          context.addAssignment(a, prop)
          ///context.set(prop.property.fullname, v)
          case e => throw InterpreterException(s"$e is not an assignable property")
        }
        case _ => throw InterpreterException(s"$prop is not an assignable property")
      }
      val v = eval(rhs)
      v match {
        case TupleV(l) => 
          if(l.size == prop.size) {
            (prop zip l) foreach { case (p, r) => setValue(p, r) }
          } else {
            throw InterpreterException(s"$prop is assigned not the same number of variables from $rhs")
          }
        case e => prop match {
          case p::Nil => setValue(p, e)
          case _ => throw InterpreterException(s"$prop is assigned not the same number variables from $rhs")
        }
      }

    case Copy(name, ref, block) =>
      if(ref.obj != null) {
        val realname = context.getNewName(name)
        val c = ref.obj.getCopy(name=realname, this)
        c.creation_time.set(context.time.toInt)
        c.flush()
        context add c
        block.setBinding(name, c)
        eval(block)
        context.addEvent(GameObjectCreated(c))
      }
    case Reset(prop) =>
      prop match {
        case prop:PropertyRef => prop.property.reset(this)
        case prop:PropertyIndirect => prop.expr match {
          case prop:PropertyRef => prop.property.reset(this)
          case _ => throw InterpreterException(s"$prop is not a resetable property")
        }
      }
    case Delete(name, ref) =>
      if(ref.obj != null) {
        //val realname = context.getNewName(name)
        //val c = ref.obj.getCopy(name=realname, this)
        ref.obj.deletion_time.set(context.time.toInt)
        context.addEvent(GameObjectDeleted(ref.obj))
      }
    case NOP => //Do nothing
  }

  def eval(expr: Expr)(implicit context: Context): Value = {
    //Log.d("Eval", s"Evaluating $expr")
    expr match {
      case NValue(v, index) =>
        eval(v) match {
          case Vec2V(x, y) => if(index == 0) FloatV(x) else FloatV(y)
          case _ => error(expr)
        }
      case Count(c) => IntV(c.objects.size)
      case v@VecExpr(l) => TupleV(l map (eval(_)))
    case c@Choose(prop, constraint) =>
      eval(c.evaluatedProgram)
    case IfFunc(cond, ifTrue, ifFalse) => val BooleanV(c) = eval(cond)
      if(c) eval(ifTrue) else eval(ifFalse)
    case ref: PropertyIndirect => eval(ref.expr)
    case ref: PropertyRef => 
      ref.get
      //context.getOrElse(ref.fullname, ref.get)
    case IntegerLiteral(v) => IntV(v)
    case FloatLiteral(v) => FloatV(v)
    case StringLiteral(v) => StringV(v)
    case BooleanLiteral(v) => BooleanV(v)
    case Vec2Expr(lhs, rhs) => 
      eval(lhs) match {
        case IntV(v1) => eval(rhs) match {
          case IntV(v2) => Vec2V(v1, v2)
          case FloatV(v2) => Vec2V(v1, v2)
          case _ => error(expr)
        }
        case FloatV(v1) => eval(rhs) match {
          case IntV(v2) => Vec2V(v1, v2)
          case FloatV(v2) => Vec2V(v1, v2)
          case _ => error(expr)
        }
        case _ => error(expr)
      }
    case Vec2Literal(x, y) => Vec2V(x, y)
    case UnitLiteral => UnitV
    case Val("dx") =>
      context.getOrElse("dx", FloatV(0))
    case Val("dy") =>
      context.getOrElse("dy", FloatV(0))
    case Val(s) => context.getOrElse(s, FloatV(0))
    case Plus(lhs, rhs) => eval(lhs) match {
      case IntV(v1) => eval(rhs) match {
        case IntV(v2) => IntV(v1 + v2)
        case FloatV(v2) => FloatV(v1 + v2)
        case _ => error(expr)
      }
      case FloatV(v1) => eval(rhs) match {
        case IntV(v2) => FloatV(v1 + v2)
        case FloatV(v2) => FloatV(v1 + v2)
        case _ => error(expr)
      }
      case Vec2V(x1, y1) => eval(rhs) match {
        case Vec2V(x2, y2) => Vec2V(x1 + x2, y1 + y2)
        case _ => error(expr)
      }
      case _ => error(expr)
    }
      
    case Minus(lhs, rhs) => eval(lhs) match {
      case IntV(v1) => eval(rhs) match {
        case IntV(v2) => IntV(v1 - v2)
        case FloatV(v2) => FloatV(v1 - v2)
        case _ => error(expr)
      }
      case FloatV(v1) => eval(rhs) match {
        case IntV(v2) => FloatV(v1 - v2)
        case FloatV(v2) => FloatV(v1 - v2)
        case _ => error(expr)
      }
      case Vec2V(x1, y1) => eval(rhs) match {
        case Vec2V(x2, y2) => Vec2V(x1 - x2, y1 - y2)
        case _ => error(expr)
      }
      case _ => error(expr)
    }
      
    case Times(lhs, rhs) => eval(lhs) match {
      case IntV(v1) => eval(rhs) match {
        case IntV(v2) => IntV(v1 * v2)
        case FloatV(v2) => FloatV(v1 * v2)
        case Vec2V(x, y) => Vec2V(v1 * x, v1 * y)
        case _ => error(expr)
      }
      case FloatV(v1) => eval(rhs) match {
        case IntV(v2) => FloatV(v1 * v2)
        case FloatV(v2) => FloatV(v1 * v2)
        case Vec2V(x, y) => Vec2V(v1 * x, v1 * y)
        case _ => error(expr)
      }
      case Vec2V(x, y) => eval(rhs) match {
        case IntV(v) => Vec2V(v * x, v * y)
        case FloatV(v) => Vec2V(v * x, v * y)
        case _ => error(expr)
      }
      case _ => error(expr)
    }
      
    case Div(lhs, rhs) => eval(lhs) match {
      case IntV(v1) => eval(rhs) match {
        case IntV(v2)=>  FloatV(v1 / v2)
        case FloatV(v2) => FloatV(v1 / v2)
        case _ => error(expr)
      }
      case FloatV(v1) => eval(rhs) match {
        case IntV(v2) => FloatV(v1 / v2)
        case FloatV(v2) => FloatV(v1 / v2)
        case _ => error(expr)
      }
      case Vec2V(x, y) => eval(rhs) match {
        case IntV(v) => Vec2V(x / v, y / v)
        case FloatV(v) => Vec2V(x / v, y / v)
        case _ => error(expr)
      }
      case _ => error(expr)
    }

    case Mod(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (IntV(v1), IntV(v2)) => IntV(v1 % v2)
        case (NumericV(v1), NumericV(v2)) => FloatV(v1 % v2)
        case _ => throw InterpreterException(s"The mod operation cannot be applied on the two expressions $lhs and $rhs.")
      }
  
    case And(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (BooleanV(v1), BooleanV(v2)) => BooleanV(v1 && v2)
        case _ => throw InterpreterException(s"An AND is not possible between $lhs and $rhs.")
      }
      
    case Or(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (BooleanV(v1), BooleanV(v2)) => BooleanV(v1 || v2)
        case _ => throw InterpreterException(s"An OR is not possible between $lhs and $rhs.")
      }
      
    case Equals(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericV(v1), NumericV(v2)) => BooleanV(v1 equals v2)
        case (StringV(v1), StringV(v2)) => BooleanV(v1 equals v2)
        case (BooleanV(v1), BooleanV(v2)) => BooleanV(v1 equals v2)
        case (Vec2V(x1, y1), Vec2V(x2, y2)) => BooleanV((x1 equals x2) && (y1 equals y2))
        case (UnitV, UnitV) => BooleanV(true)
        case _ => BooleanV(false)
      }
      
    case LessThan(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericV(v1), NumericV(v2)) => BooleanV(v1 < v2)
        case _ => throw InterpreterException(s"A LessThan is not possible between $lhs and $rhs.")
      }

    case LessEq(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericV(v1), NumericV(v2)) => BooleanV(v1 <= v2)
        case _ => throw InterpreterException(s"A LessEq is not possible between $lhs and $rhs.")
      }

    case GreaterThan(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericV(v1), NumericV(v2)) => BooleanV(v1 > v2)
        case _ => throw InterpreterException(s"A GreaterThan is not possible between $lhs and $rhs.")
      }

    case GreaterEq(lhs, rhs) =>
      (eval(lhs), eval(rhs)) match {
        case (NumericV(v1), NumericV(v2)) => BooleanV(v1 >= v2)
        case _ => throw InterpreterException(s"A GreaterEq is not possible between $lhs and $rhs.")
      }
      
    case Not(e) =>
      eval(e) match {
        case BooleanV(v) => BooleanV(!v)
        case _ => throw InterpreterException(s"A Not is not possible on $e.")
      }

    case FingerMoveOver(o) =>
      var fromx = 0f
      var fromy = 0f
      var tox = 0f
      var toy = 0f
      var first = true
      var n = 0
      if(!(context.events.collect{ case e: FingerMove => e }).isEmpty) {
        n *= 2
      }
      context.fingerMoves(_.obj.exists(_ == o.obj)) foreach {
        event => if(first) {
          fromx = event.from.x
          fromy = event.from.y
          first = false
        }
        tox = event.to.x
        toy = event.to.y
        n += 1
      }
      var result = if(n != 0) {
        context.set("from", Vec2V(fromx, fromy))
        context.set("to", Vec2V(tox, toy))
        context.set("dx", FloatV(tox-fromx))
        context.set("dy", FloatV(toy-fromy))
        true
      } else false
      
      BooleanV(result)

    case FingerDownOver(o) => 
      BooleanV(context.fingerDowns(_.obj.exists(_ == o.obj)).nonEmpty)

    case FingerUpOver(o) => 
      BooleanV(context.fingerUps(_.obj.exists(_ == o.obj)).nonEmpty)

    case Collision(o1, o2) =>
      val v = context.beginContacts{ c =>
        (c.contact.objectA == o1.obj && c.contact.objectB == o2.obj) ||
        (c.contact.objectA == o2.obj && c.contact.objectB == o1.obj)
      }.nonEmpty
      BooleanV(v)
    case FingerCoordX1 =>
      FloatV(context.getOrElse("from", Vec2V(0, 0)).asInstanceOf[Vec2V].x)
    case FingerCoordX2 =>
      FloatV(context.getOrElse("to", Vec2V(0, 0)).asInstanceOf[Vec2V].x)
    case FingerCoordY1 =>
      FloatV(context.getOrElse("from", Vec2V(0, 0)).asInstanceOf[Vec2V].y)
    case FingerCoordY2 =>
      FloatV(context.getOrElse("to", Vec2V(0, 0)).asInstanceOf[Vec2V].y)
    case GameObjectRef(name, obj) =>
      GameObjectV(obj)
    case On(cond) => // Seems to be useless / or to be used when a collision occurs once until it is removed.
      eval(cond) // TODO : Check that this is true until the condition is false, for any pair.
    case Once(cond) =>
      eval(cond) // TODO : Check that it happens only once.
    case null =>
      Log.d("Interpreter", "Erreur while evaluating null")
      throw new InterpreterException("Error while evaluating null")
    }
  }
  
  private def error(expr: Expr): Nothing = {
    throw InterpreterException(s"A TypeCheck error occurs during interpretation of $expr.")
  }
}