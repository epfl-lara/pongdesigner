package ch.epfl.lara.synthesis.kingpong.expression

// Remove implicit warnings
import language.implicitConversions

import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._

object TreeDSL {
  
  final val booleanLiteralTrue = BooleanLiteral(true)
  final val booleanLiteralFalse = BooleanLiteral(false)
  final val integerLiteralZero = IntegerLiteral(0)
  final val integerLiteralOne = IntegerLiteral(1)
  final val integerLiteralTwo = IntegerLiteral(2)
  final val integerLiteralThree = IntegerLiteral(3)
  final val floatLiteralZero = FloatLiteral(0f)
  final val floatLiteralOne = FloatLiteral(1f)
  final val floatLiteralTwo = FloatLiteral(2f)
  final val floatLiteralThree = FloatLiteral(3f)
  
  /**
   * Converts a value to a literal.
   * Use some optimization if possible to use the same objects.
   */
  object Literal {
    def apply(value: Any): Expr = {
      value match {
        case true  => booleanLiteralTrue
        case false => booleanLiteralFalse
        case 0 => integerLiteralZero
        case 1 => integerLiteralOne
        case 2 => integerLiteralTwo
        case 3 => integerLiteralThree
        case 0f => floatLiteralZero
        case 1f => floatLiteralOne
        case 2f => floatLiteralTwo
        case 3f => floatLiteralThree
        case e: Float => FloatLiteral(e)
        case e: Int => IntegerLiteral(e)
        case e: String => StringLiteral(e)
        case e: Boolean => BooleanLiteral(e)
        case e: Long => IntegerLiteral(e.toInt)
        case e: Unit => UnitLiteral
        case e: GameObject => ObjectLiteral(e)
        case _ => UnitLiteral
      }
    }
  }
  
  implicit def NumericIsExpr[T: Numeric](n: T): Expr = Literal(implicitly[Numeric[T]].toFloat(n))
  implicit def FloatIsExpr(f: Float): Expr = Literal(f)
  implicit def IntegerIsExpr(i: Int): Expr = Literal(i)
  implicit def StringIsExpr(s: String): Expr = Literal(s)
  implicit def BooleanIsExpr(b: Boolean): Expr = Literal(b)
  implicit def Vec2IsExpr(v: Vec2): Expr = Tuple(Seq(Literal(v.x), Literal(v.y)))
  implicit def ListIsExpr(v: List[Expr]): Tuple = Tuple(v)
  
  implicit def proxyToExpr(ref: Proxy): Expr = ref.expr
  implicit def objectToExpr(obj: GameObject): Expr = obj.expr
  implicit def objectToProxyExpr(obj: GameObject): ProxyExpr = new ProxyExpr(obj.expr)
  implicit def propertyToExpr(prop: Property[_]): Expr = prop.expr
  implicit def seqStatToStat(stats: Seq[Expr]) = stats match {
    case Seq()  => NOP
    case Seq(s) => s
    case _      => Block(stats)
  }
  
  implicit def literalToRichExpr(e: Expr): RichExpr = new RichExpr(e)
  implicit def proxyToRichExpr(ref: Proxy): RichExpr = new RichExpr(ref.expr)
  implicit def propertyToRichExpr(prop: Property[_]): RichExpr = new RichExpr(prop.expr)
  
  class RichExpr(val expr: Expr) extends AnyVal {
    
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
    def _1: Expr = TupleSelect(expr, 1)
    def _2: Expr = TupleSelect(expr, 2)
    def _3: Expr = TupleSelect(expr, 3)
    def ::(e: Expr) : Expr = (expr, e) match{
      case (Block(a), Block(b)) => Block(a++b)
      case (Block(a), b) => Block(a++List(b))
      case (a, Block(b)) => Block(a::b.toList)
      case (a, b) => Block(List(a, b))
    }
  }
  
  trait Proxy extends Any {
    def expr: Expr
    
    def name = new PropertyProxySingleRef(expr, "name").setType(TString)
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
    def center = new PropertyProxySingleRef(expr, "center").setType(TVec2)
    
    def cell(column: Expr, row: Expr) = {
      new ProxyExpr(Apply(expr, column, row))
    }
  }
  
  class IdentifierProxy(id: Identifier) extends Proxy {
    def expr = Variable(id)
  }
  
  class ProxyExpr(val expr: Expr) extends Proxy

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
  
  def fingerMoveOver(obj: Expr)(body: Proxy => Expr): Expr = {
    val id = FreshIdentifier("move").setType(TTuple(TVec2, TVec2))
    val ref = new IdentifierProxy(id)
    FingerMoveOver(obj, id, body(ref))
  }
  
  def isFingerMoveOver(obj: Expr): Expr = {
    val id = FreshIdentifier("move").setType(TTuple(TVec2, TVec2))
    FingerMoveOver(obj, id, NOP)
  }
  
  def let(name: String, expr: Expr)(body: Proxy => Expr): Expr = {
    val id = FreshIdentifier(name).setType(expr.getType)
    val ref = new IdentifierProxy(id)
    Let(id, expr, body(ref))
  }
  
  def foreach(category: Category)(body: Proxy => Expr): Expr = {
    val id = FreshIdentifier(category.name).setType(TObject)
    val ref = new IdentifierProxy(id)
    Foreach(category, id, body(ref))
  }

  def foreach(category1: Category, category2: Category)(body: (Proxy, Proxy) => Expr): Expr = {
    val id1 = FreshIdentifier(category1.name).setType(TObject)
    val id2 = FreshIdentifier(category2.name).setType(TObject)
    val ref1 = new IdentifierProxy(id1)
    val ref2 = new IdentifierProxy(id2)
    Foreach(category1, id1, Foreach(category2, id2, body(ref1, ref2)))
  }
  
  def foreach(category1: Category, category2: Category, category3: Category)(body: (Proxy, Proxy, Proxy) => Expr): Expr = {
    val id1 = FreshIdentifier(category1.name).setType(TObject)
    val id2 = FreshIdentifier(category2.name).setType(TObject)
    val id3 = FreshIdentifier(category3.name).setType(TObject)
    val ref1 = new IdentifierProxy(id1)
    val ref2 = new IdentifierProxy(id2)
    val ref3 = new IdentifierProxy(id3)
    Foreach(category1, id1, Foreach(category2, id2, Foreach(category3, id3, body(ref1, ref2, ref3))))
  }
  
  def copy(obj: Expr)(body: Proxy => Expr): Expr = {
    val id = FreshIdentifier("copy").setType(TObject)
    val ref = new IdentifierProxy(id)
    Copy(obj, id, body(ref))
  }
  
  def forall(category: Category)(body: Proxy => Expr): Expr = {
    val id = FreshIdentifier(category.name).setType(TObject)
    val ref = new IdentifierProxy(id)
    Forall(category, id, body(ref))
  }
  
  def find(category: Category)(body: Proxy => Expr): Expr = {
    val id = FreshIdentifier(category.name).setType(TObject)
    val ref = new IdentifierProxy(id)
    Find(category, id, body(ref))
  }
  
  def whenever(cond: Expr)(actions: Expr*): Expr = {
    If(cond, TreeOps.flatten(actions.toSeq))
  }
  
  def debug(msg: String, args: Expr*): Expr = {
    Debug(msg, args.toSeq)
  }
  
}