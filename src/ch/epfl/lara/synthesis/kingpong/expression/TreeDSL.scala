package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.objects._


object TreeDSL {

  implicit def objectReftoExpr(ref: ObjectProxy): Expr = ref.expr
  implicit def objectToExpr(obj: GameObject): Expr = obj.expr
  implicit def propertyToExpr(prop: Property[_]): Expr = prop.expr
  implicit def propertyProxytoExpr(ref: ObjectProxy#AssignablePropertyProxy): Expr = ref.expr
  implicit def propertyProxytoPropertyIdentifier(ref: ObjectProxy#AssignablePropertyProxy): PropertyIdentifier = ref.identifier
  
  implicit class RichAssignableProperty(property: Property[_] with AssignableProperty[_]) extends AnyRef {
    private def identifier = property.identifier
    def :=(expr: Expr): Stat = Assign(Seq(identifier), expr)
    def +=(expr: Expr): Stat = Assign(Seq(identifier), Plus(Variable(identifier), expr))
    def -=(expr: Expr): Stat = Assign(Seq(identifier), Minus(Variable(identifier), expr))
    def *=(expr: Expr): Stat = Assign(Seq(identifier), Times(Variable(identifier), expr))
    def /=(expr: Expr): Stat = Assign(Seq(identifier), Div(Variable(identifier), expr))
  }
  
  trait RichExpr extends AnyRef {
    def expr: Expr
    
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
  implicit class RichExprPropertyProxy(ref: ObjectProxy#AssignablePropertyProxy) extends RichExpr {
    def expr = ref.expr
  }
  implicit class RichExprProperty(prop: Property[_]) extends RichExpr {
    def expr = prop.expr
  }
  
  
  class ObjectProxy(id: ObjectIdentifier) {
    lazy val expr: Expr = Variable(id)
    
    /* Read-Write properties */
    def x = AssignablePropertyProxy("x", TFloat)
    def y = AssignablePropertyProxy("y", TFloat)
    def angle = AssignablePropertyProxy("angle", TFloat)
    def visible = AssignablePropertyProxy("visible", TBoolean)
    def color = AssignablePropertyProxy("color", TBoolean)
    def velocity = AssignablePropertyProxy("velocity", TVec2)
    def radius = AssignablePropertyProxy("radius", TFloat)
    
    //TODO handle Box[T] type...
    def value = AssignablePropertyProxy("value", TUntyped)
    
    /* Read-only properties */
    def bottom = Variable(FreshPropertyIdentifier(id, "bottom")).setType(TFloat)
    def top = Variable(FreshPropertyIdentifier(id, "top")).setType(TFloat)
    def left = Variable(FreshPropertyIdentifier(id, "left")).setType(TFloat)
    def right = Variable(FreshPropertyIdentifier(id, "right")).setType(TFloat)
    
    object AssignablePropertyProxy {
      def apply(property: String, tpe: Type) = new AssignablePropertyProxy(property, tpe)
    }
    
    class AssignablePropertyProxy(property: String, tpe: Type) {
      lazy val expr: Expr = Variable(identifier)
      lazy val identifier: PropertyIdentifier = FreshPropertyIdentifier(id, property).setType(tpe)
      
      def :=(expr: Expr): Stat = Assign(List(this), expr)
      def +=(expr: Expr): Stat = Assign(List(this), Plus(this, expr))
      def -=(expr: Expr): Stat = Assign(List(this), Minus(this, expr))
      def *=(expr: Expr): Stat = Assign(List(this), Times(this, expr))
      def /=(expr: Expr): Stat = Assign(List(this), Div(this, expr))
    }
    
  }
}