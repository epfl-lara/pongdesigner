package ch.epfl.lara.synthesis.kingpong.expression

// Remove implicit warnings
import language.implicitConversions
import scala.annotation.tailrec

import org.jbox2d.dynamics.BodyType

import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import Interpreter.NumericLiteral

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
    def in(e: Expr): Expr = Contains(e, expr)
    def orElse(e: Expr): ParExpr = e match {
      case ParExpr(l) =>
        ParExpr(expr::l.filterNot(_ eq expr))
      case e =>
        ParExpr(expr::e::Nil)
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
    def width = new PropertyProxySingleRef(expr, "width").setType(TFloat)
    def height = new PropertyProxySingleRef(expr, "height").setType(TFloat)
    
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
    def :=(e: Expr): Expr
    def :=(e1: Expr, e2: Expr): Expr = this := Tuple(e1, e2)
  }
  
  trait PropertyProxySingle extends PropertyProxy {
    protected def propertyPair: (Expr, PropertyId)
    def expr: Expr = Select(propertyPair._1, propertyPair._2).setType(getType)
    def :=(e: Expr): Expr = Assign(propertyPair, e)
    def +=(e: Expr): Expr = Assign(propertyPair, Plus(expr, e))
    def -=(e: Expr): Expr = Assign(propertyPair, Minus(expr, e))
    def *=(e: Expr): Expr = Assign(propertyPair, Times(expr, e))
    def /=(e: Expr): Expr = Assign(propertyPair, Div(expr, e))
  }

  trait PropertyProxyMultiple extends PropertyProxy {
    protected def propertyPairs: Seq[(Expr, PropertyId)]
    def :=(e: Expr): Expr = let("tuple", e) { tuple =>
      propertyPairs.zipWithIndex.map{ case (pair, index) => Assign(pair, TupleSelect(tuple, index + 1)) }
    }
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
    new PropertyProxyMultiple {
      def expr = Tuple(Seq(props._1, props._2).map(_.expr))
      protected def propertyPairs = Seq(props._1, props._2).map(p => (p.parent.expr, p.name))
    }
  }
  
  implicit def propertySeqToProxy(props: Seq[Property[_] with AssignableProperty[_]]): PropertyProxy = {
    new PropertyProxyMultiple {
      def expr = Tuple(props.map(_.expr))
      protected def propertyPairs = props.map(p => (p.parent.expr, p.name))
    }
  }
  
  def fingerMoveOver(obj: Expr)(body: Proxy => Expr): Expr = {
    val id = FreshIdentifier("move").setType(TTuple(TVec2, TVec2))
    val ref = new IdentifierProxy(id)
    FingerMoveOver(obj, id, body(ref))
  }
  
  def fingerDownOver(obj: Expr)(body: Proxy => Expr): Expr = {
    val id = FreshIdentifier("fing").setType(TVec2)
    val ref = new IdentifierProxy(id)
    FingerDownOver(obj, id, body(ref))
  }
  
  def isFingerMoveOver(obj: Expr): Expr = {
    val id = FreshIdentifier("move").setType(TTuple(TVec2, TVec2))
    FingerMoveOver(obj, id, NOP)
  }
  
   def isFingerUpOver(obj: Expr): Expr = {
    IsFingerUpOver(obj)
  }
  
   def isFingerDownOver(obj: Expr): Expr = {
    IsFingerDownOver(obj)
  }
  
  def let(name: String, expr: Expr)(body: Proxy => Expr): Expr = {
    val id = FreshIdentifier(name).setType(expr.getType)
    val ref = new IdentifierProxy(id)
    Let(id, expr, body(ref))
  }
  
  def let(name1: String, name2: String, expr1: Expr, expr2: Expr)(body: (Proxy, Proxy) => Expr): Expr = {
    val id1 = FreshIdentifier(name1).setType(expr1.getType)
    val id2 = FreshIdentifier(name2).setType(expr2.getType)
    val ref1 = new IdentifierProxy(id1)
    val ref2 = new IdentifierProxy(id2)
    Let(id1, expr1, Let(id2, expr2, body(ref1, ref2)))
  }
  
  def foreach(category: Category)(body: Proxy => Expr): Expr = {
    val id = identifierFromCategory(category).setType(TObject)
    val ref = new IdentifierProxy(id)
    Foreach(category, id, body(ref))
  }

  def foreach(category1: Category, category2: Category)(body: (Proxy, Proxy) => Expr): Expr = {
    val id1 = identifierFromCategory(category1).setType(TObject)
    val id2 = identifierFromCategory(category2).setType(TObject)
    val ref1 = new IdentifierProxy(id1)
    val ref2 = new IdentifierProxy(id2)
    Foreach(category1, id1, Foreach(category2, id2, body(ref1, ref2)))
  }
  
  def foreach(category1: Category, category2: Category, category3: Category)(body: (Proxy, Proxy, Proxy) => Expr): Expr = {
    val id1 = identifierFromCategory(category1).setType(TObject)
    val id2 = identifierFromCategory(category2).setType(TObject)
    val id3 = identifierFromCategory(category3).setType(TObject)
    val ref1 = new IdentifierProxy(id1)
    val ref2 = new IdentifierProxy(id2)
    val ref3 = new IdentifierProxy(id3)
    Foreach(category1, id1, Foreach(category2, id2, Foreach(category3, id3, body(ref1, ref2, ref3))))
  }
  
  // Generic foreach
  def foreach(categories: Seq[Category])(body: Seq[Proxy] => Expr): Expr = {
    val ids = categories.map(identifierFromCategory(_).setType(TObject))
    val refs = ids map { id => new IdentifierProxy(id) }
    ((categories zip ids) :\ body(refs)) { case ((category, id), body) => Foreach(category, id, body) }
  }
  
  def copy(obj: Expr)(body: Proxy => Expr): Expr = {
    val id = FreshIdentifier("copy").setType(TObject)
    val ref = new IdentifierProxy(id)
    Copy(obj, id, body(ref))
  }
  
  def forall(category: Category)(body: Proxy => Expr): Expr = {
    val id = identifierFromCategory(category).setType(TObject)
    val ref = new IdentifierProxy(id)
    Forall(category, id, body(ref))
  }
  
  def find(category: Category)(body: Proxy => Expr): Expr = {
    val id = identifierFromCategory(category).setType(TObject)
    val ref = new IdentifierProxy(id)
    Find(category, id, body(ref))
  }
  
  def whenever(cond: Expr)(actions: Expr*): Expr = {
    If(cond, TreeOps.flatten(actions.toSeq))
  }
  
  def debug(msg: String, args: Expr*): Expr = {
    Debug(msg, args.toSeq)
  }

  def isNull(expr: Expr): Expr  = expr =:= ObjectLiteral(null)
  def notNull(expr: Expr): Expr = expr =!= ObjectLiteral(null)

  @tailrec
  def and(exprs: List[Expr]): Expr = exprs match {
    case Nil           => BooleanLiteral(true)
    case e1 :: Nil     => e1
    case e1 :: e2 :: t => and(And(e1, e2) :: t)
  }
  
  private def identifierFromCategory(cat: Category): Identifier = cat.name match {
    case ""   => FreshIdentifier("id", true)
    case name => FreshIdentifier(name.toLowerCase.substring(0, 1), true)
  }
  
  /* Object builders */
  
  def circle(category: CategoryObject)(
      name: Expr,
      x: Expr,
      y: Expr,
      radius: Expr = category.radius,
      visible: Expr = category.visible,
      velocity: Expr = category.velocity,
      angularVelocity: Expr = category.angularVelocity,
      density: Expr = category.density,
      friction: Expr = category.friction,
      restitution: Expr = category.restitution,
      linearDamping: Expr = category.linearDamping,
      fixedRotation: Expr = category.fixedRotation,
      color: Expr = category.color,
      sensor: Expr = category.sensor,
      tpe: BodyType = category.tpe)(implicit game: Game): Circle = {
    val obj = new Circle(game, name, x, y, radius, visible, velocity, angularVelocity, 
                         density, friction, restitution, linearDamping, fixedRotation, color, sensor, tpe)
    obj.setCategory(category)
    game.add(obj)
    obj
  }

  def rectangle(category: CategoryObject)(
      name: Expr,
      x: Expr,
      y: Expr,
      angle: Expr = category.angle,
      width: Expr = category.width,
      height: Expr = category.height,
      visible: Expr = category.visible,
      velocity: Expr = category.velocity,
      angularVelocity: Expr = category.angularVelocity,
      density: Expr = category.density,
      friction: Expr = category.friction,
      restitution: Expr = category.restitution,
      linearDamping: Expr = category.linearDamping,
      fixedRotation: Expr = category.fixedRotation,
      color: Expr = category.color,
      sensor: Expr = category.sensor,
      tpe: BodyType = category.tpe)(implicit game: Game): Rectangle = {
    val obj = new Rectangle(game, name, x, y, angle, width, height, visible, velocity, angularVelocity, 
                            density, friction, restitution, linearDamping, fixedRotation, color, sensor, tpe)
    obj.setCategory(category)
    game.add(obj)
    obj
  }
  
  def randomGenerator(category: CategoryObject)(
      name: Expr,
      x: Expr,
      y: Expr,
      angle: Expr = category.angle,
      width: Expr = category.width,
      height: Expr = category.height,
      minValue: Expr = category.randomMinValue,
      maxValue: Expr = category.randomMaxValue,
      visible: Expr = category.visible,
      color: Expr = category.color)(implicit game: Game): RandomGenerator = {
    val obj = new RandomGenerator(game, name, x, y, angle, width, height, minValue, maxValue, visible, color)
    obj.setCategory(category)
    game.add(obj)
    obj
  }
  
  def drawingObject(category: CategoryObject)(
      name: Expr,
      x: Expr,
      y: Expr,
      angle: Expr = category.angle,
      width: Expr = category.width,
      height: Expr = category.height,
      visible: Expr = category.visible,
      color: Expr = category.color,
      stroke_width: Expr = category.stroke_width,
      color_drawing: Expr = category.color_drawing)(implicit game: Game): DrawingObject = {
    val obj = new DrawingObject(game, name, x, y, angle, width, height, visible, color, stroke_width, color_drawing)
    obj.setCategory(category)
    game.addRule(obj.defaultRule(game))
    game.add(obj)
    obj
  }
  
  def soundRecorder(category: CategoryObject)(
      name: Expr,
      x: Expr,
      y: Expr,
      angle: Expr = category.angle,
      width: Expr = category.width,
      height: Expr = category.height,
      visible: Expr = category.visible,
      color: Expr = category.color,
      recording: Expr = category.recording)(implicit game: Game): SoundRecorder = {
    val obj = new SoundRecorder(game, name, x, y, angle, width, height, visible, color, recording)
    obj.setCategory(category)
    //game.addRule(obj.defaultRule(game))
    game.add(obj)
    obj
  }
  
  def soundTTS(category: CategoryObject)(
      name: Expr,
      x: Expr,
      y: Expr,
      angle: Expr = category.angle,
      width: Expr = category.width,
      height: Expr = category.height,
      visible: Expr = false,
      color: Expr = category.color,
      language: Expr = category.language,
      text: Expr = "",
      time: Expr = category.time
      )(implicit game: Game): SoundTTS = {
    val obj = new SoundTTS(game, name, x, y, angle, width, height, visible, color, language, text, time)
    obj.setCategory(category)
    //game.addRule(obj.defaultRule(game))
    game.add(obj)
    obj
  }

  def array(category: CategoryObject)(
      name: Expr,
      x: Expr,
      y: Expr,
      columns: Expr,
      rows: Expr,
      cellWidth: Expr = category.cellWidth,
      cellHeight: Expr = category.cellHeight,
      angle: Expr = category.angle,
      visible: Expr = category.visible,
      color: Expr = category.color)(implicit game: Game): Array2D = {
    val obj = new Array2D(game, name, x, y, visible, color, cellWidth, cellHeight, columns, rows)
    obj.setCategory(category)
    game.add(obj)
    obj.cells.foreach(_ foreach { cell =>
      game.add(cell)
    })
    obj
  }

  def intbox(category: CategoryObject)(
      name: Expr,
      x: Expr,
      y: Expr,
      value: Expr = category.value,
      angle: Expr = category.angle,
      width: Expr = category.width,
      height: Expr = category.height,
      visible: Expr = category.visible,
      color: Expr = category.color)(implicit game: Game): Box[Int] = {
    val obj = new IntBox(game, name, x, y, angle, width, height, value, visible, color)
    obj.setCategory(category)
    game.add(obj)
    obj
  }
  
  def booleanbox(category: CategoryObject)(
      name: Expr,
      x: Expr,
      y: Expr,
      value: Expr = category.value,
      angle: Expr = category.angle,
      width: Expr = category.width,
      height: Expr = category.height,
      visible: Expr = category.visible,
      color: Expr = category.color)(implicit game: Game): Box[Boolean] = {
    val obj = new BooleanBox(game, name, x, y, angle, width, height, value, visible, color)
    obj.setCategory(category)
    game.add(obj)
    obj
  }
  
  def stringbox(category: CategoryObject)(
      name: Expr,
      x: Expr,
      y: Expr,
      value: Expr = category.value,
      angle: Expr = category.angle,
      width: Expr = category.width,
      height: Expr = category.height,
      visible: Expr = category.visible,
      color: Expr = category.color)(implicit game: Game): Box[String] = {
    val obj = new StringBox(game, name, x, y, angle, width, height, value, visible, color)
    obj.setCategory(category)
    game.add(obj)
    obj
  }
  
  def joystick(category: CategoryObject)(
      name: Expr,
      x: Expr,
      y: Expr,
      angle: Expr = category.angle,
      radius: Expr = category.radius,
      visible: Expr = category.visible,
      color: Expr = category.color)(implicit game: Game): Joystick = {
    val obj = new Joystick(game, name, x, y, angle, radius, visible, color)
    obj.setCategory(category)
    game.add(obj)
    obj
  }
  
  def character(category: CategoryObject)(
      name: Expr,
      x: Expr,
      y: Expr,
      angle: Expr = category.angle,
      width: Expr = category.width,
      height: Expr = category.height,
      visible: Expr = category.visible,
      velocity: Expr = category.velocity,
      angularVelocity: Expr = category.angularVelocity,
      density: Expr = category.density,
      friction: Expr = category.friction,
      restitution: Expr = category.restitution,
      linearDamping: Expr = category.linearDamping,
      fixedRotation: Expr = category.fixedRotation,
      color: Expr = category.color,
      tpe: BodyType = category.tpe)(implicit game: Game): Character = {
    val obj = new Character(game, name, x, y, angle, width, height, visible, velocity, angularVelocity, 
                            density, friction, restitution, linearDamping, fixedRotation, color, tpe)
    obj.setCategory(category)
    game.add(obj)
    obj
  }
  
  def gravity(category: CategoryObject)(
      name: Expr,
      x: Expr,
      y: Expr,
      angle: Expr = category.angle,
      radius: Expr = category.radius,
      visible: Expr = false,
      color: Expr = category.color)(implicit game: Game): Gravity = {
    val vecGravity = game.world.getGravity
    
    val (nradius, nangle) : (Expr, Expr) = angle match { // By default, if the angle is null, takes the current world gravity.
      case NumericLiteral(0) =>
        (vecGravity.length(), Math.atan2(vecGravity.y, vecGravity.x))
      case _ =>
        (radius, angle)
    }
    val obj = new Gravity(game, name, x, y, nangle, nradius, visible, color)
    obj.setCategory(category)
    game.add(obj)
    obj
  }
  
}