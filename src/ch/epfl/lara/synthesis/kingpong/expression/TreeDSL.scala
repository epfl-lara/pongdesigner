package ch.epfl.lara.synthesis.kingpong.expression

// Remove implicit warnings
import language.implicitConversions

import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._

object TreeDSL {
  implicit def NumericIsExpr[T: Numeric](n: T): Expr = FloatLiteral(implicitly[Numeric[T]].toFloat(n))
  implicit def FloatIsExpr(f: Float): Expr = FloatLiteral(f)
  implicit def IntegerIsExpr(i: Int): Expr = IntegerLiteral(i)
  implicit def StringIsExpr(s: String): Expr = StringLiteral(s)
  implicit def BooleanIsExpr(b: Boolean): Expr = BooleanLiteral(b)
  implicit def Vec2IsExpr(v: Vec2): Expr = Tuple(Seq(FloatLiteral(v.x), FloatLiteral(v.y)))
  implicit def ListIsExpr(v: List[Expr]): Tuple = Tuple(v)
  
  implicit def proxyToExpr(ref: Proxy): Expr = ref.expr
  implicit def objectToExpr(obj: GameObject): Expr = obj.expr
  implicit def objectToProxyExpr(obj: GameObject): ObjectProxyExpr = new ObjectProxyExpr(obj.expr)
  implicit def propertyToExpr(prop: Property[_]): Expr = prop.expr
  implicit def seqStatToStat(stats: Seq[Expr]) = stats match {
    case Seq()  => NOP
    case Seq(s) => s
    case _      => Block(stats)
  }
  
  trait RichExpr extends AnyRef {
    def expr: Expr
    
    def as[T : PongType]: T = implicitly[PongType[T]].toScalaValue(expr)
    
    def +(e: Expr): Expr = e match {
      case IntegerLiteral(0) | FloatLiteral(0) => expr
      case IntegerLiteral(i) => expr match {
        case IntegerLiteral(j) => IntegerLiteral(i + j)
        case FloatLiteral(j) => FloatLiteral(i + j)
        case _ => Plus(expr, e)
      }
      case FloatLiteral(i) => expr match {
        case IntegerLiteral(j) => FloatLiteral(i + j)
        case FloatLiteral(j) => FloatLiteral(i + j)
        case _ => Plus(expr, e)
      }
      case _ => expr match {
        case IntegerLiteral(0) | FloatLiteral(0) => e
        case _ => Plus(expr, e)
      }
    }
    
    def -(e: Expr): Expr = e match {
      case IntegerLiteral(0) | FloatLiteral(0) => expr
      case _ => Minus(expr, e)
    }
    
    def unary_- : Expr = expr match {
      case IntegerLiteral(j) => IntegerLiteral(-j)
      case FloatLiteral(j) => FloatLiteral(-j)
      case _ => Minus(IntegerLiteral(0), expr)
    }
    
    def *(e: Expr): Expr = e match {
      case IntegerLiteral(0) | FloatLiteral(0) => e
      case IntegerLiteral(1) | FloatLiteral(1) => expr
      case IntegerLiteral(-1) | FloatLiteral(-1) => expr match {
        case IntegerLiteral(i) => IntegerLiteral(-i)
        case FloatLiteral(i) => FloatLiteral(-i)
        case _ => Times(expr, e)
      }
      case FloatLiteral(i) => expr match {
        case IntegerLiteral(j) => FloatLiteral(j*i)
        case FloatLiteral(j) => FloatLiteral(j*i)
        case _ => Times(expr, e)
      }
      case _ => expr match {
        case IntegerLiteral(0) | FloatLiteral(0) => expr
        case _ => Times(expr, e)
      }
    }
    
    def /(e: Expr): Expr = e match {
      case IntegerLiteral(1) | FloatLiteral(1) => expr
      case IntegerLiteral(-1) | FloatLiteral(-1) => expr match {
        case IntegerLiteral(i) => IntegerLiteral(-i)
        case FloatLiteral(i) => FloatLiteral(-i)
        case _ => Div(expr, e)
      }
      case FloatLiteral(i) => expr match {
        case IntegerLiteral(j) => FloatLiteral(j/i)
        case FloatLiteral(j) => FloatLiteral(j/i)
        case _ => Div(expr, e)
      }
      case _ => expr match {
        case IntegerLiteral(0) | FloatLiteral(0) => expr
        case _ => Div(expr, e)
      }
    }
    
    def %(e: Expr): Expr = Mod(expr, e)
    def &&(e: Expr): Expr = And(expr, e)
    def ||(e: Expr): Expr = Or(expr, e)
    def =:=(e: Expr): Expr = Equals(expr, e)
    def =!=(e: Expr): Expr = Not(Equals(expr, e))
    def <(e: Expr): Expr = LessThan(expr, e)
    def <=(e: Expr): Expr = LessEq(expr, e)
    def >(e: Expr): Expr = GreaterThan(expr, e)
    def >=(e: Expr): Expr = GreaterEq(expr, e)
    def unary_! : Expr = Not(expr)
  }
  
  implicit class RichExprLiteral(val expr: Expr) extends RichExpr
  implicit class RichExprProxy(ref: Proxy) extends RichExpr {
    def expr = ref.expr
  }
  implicit class RichExprProperty(prop: Property[_]) extends RichExpr {
    def expr = prop.expr
  }
  
  
  trait Proxy extends AnyRef {
    def expr: Expr
    
    def x = new PropertyProxySingleRef(expr, "x").setType(TFloat)
    def y = new PropertyProxySingleRef(expr, "y").setType(TFloat)
    def angle = new PropertyProxySingleRef(expr, "angle").setType(TFloat)
    def visible = new PropertyProxySingleRef(expr, "visible").setType(TBoolean)
    def color = new PropertyProxySingleRef(expr, "color").setType(TBoolean)
    def velocity = new PropertyProxySingleRef(expr, "velocity").setType(TVec2)
    def radius = new PropertyProxySingleRef(expr, "radius").setType(TFloat)
    
    //TODO handle Box[T] type...
    def value = new PropertyProxySingleRef(expr, "value").setType(TUntyped)
    
    def bottom = new PropertyProxySingleRef(expr, "bottom").setType(TFloat)
    def top = new PropertyProxySingleRef(expr, "top").setType(TFloat)
    def left = new PropertyProxySingleRef(expr, "left").setType(TFloat)
    def right = new PropertyProxySingleRef(expr, "right").setType(TFloat) 
    
    def cell(column: Expr, row: Expr) = new ArrayApplyProxy(expr, column, row)
  }
  
  class ObjectProxy(id: Identifier) extends Proxy {
    def expr = Variable(id).setType(TObject)
  }
  
  class ObjectProxyExpr(val expr: Expr) extends Proxy

  class ArrayApplyProxy(obj: Expr, column: Expr, row: Expr) extends Proxy {
    def expr = Apply(obj, column, row).setType(TObject)
  }
  
  trait PropertyProxy extends Proxy with Typed {
    protected def properties: Seq[(Expr, PropertyId)]
    def :=(e: Expr): Expr = Assign(properties, e)
    def :=(e1: Expr, e2: Expr): Expr = Assign(properties, Tuple(Seq(e1, e2)))
  }
  
  trait PropertyProxySingle extends PropertyProxy {
    protected def propertyPair: (Expr, PropertyId)
    protected def properties = Seq(propertyPair)
    def expr: Expr = Select(propertyPair._1, propertyPair._2).setType(getType)
    def +=(e: Expr): Expr = Assign(properties, Plus(expr, e))
    def -=(e: Expr): Expr = Assign(properties, Minus(expr, e))
    def *=(e: Expr): Expr = Assign(properties, Times(expr, e))
    def /=(e: Expr): Expr = Assign(properties, Div(expr, e))
  }
  
  class PropertyProxySingleRef(baseExpr: Expr, property: String) extends PropertyProxySingle {
    protected def propertyPair = (baseExpr, property)
  }
  
  implicit def propertyToProxy(property: Property[_] with AssignableProperty[_]): PropertyProxySingle = {
    new PropertyProxySingle {
      protected def propertyPair = (property.parent.expr, property.name)
    }
  }
  
  implicit def propertyTupleToProxy(props: (Property[_] with AssignableProperty[_], Property[_] with AssignableProperty[_])): PropertyProxy = {
    new PropertyProxy {
      def expr = Tuple(Seq(props._1, props._2).map(_.expr))
      protected def properties = Seq(props._1, props._2).map(p => (p.parent.expr, p.name))
    }
  }
  
  implicit def propertySeqToProxy(props: Seq[Property[_] with AssignableProperty[_]]): PropertyProxy = {
    new PropertyProxy {
      def expr = Tuple(props.map(_.expr))
      protected def properties = props.map(p => (p.parent.expr, p.name))
    }
  }
  
  
}