package ch.epfl.lara.synthesis.kingpong.expression

// remove the warning 
import language.existentials

import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._

object Trees {

  sealed trait Tree {
    def setBinding(n: String, o: GameObject): this.type
  }
  sealed trait NoBinding extends Tree {
     def setBinding(n: String, o: GameObject) = { this }
  }
  sealed trait OBinding extends Tree {
    def o: Tree
    def setBinding(n: String, ob: GameObject) = { o.setBinding(n, ob); this }
  }

  /** Statement, can have side-effect. */
  sealed trait Stat extends Tree

  case class Assign(prop: PropertyRefTrait, rhs: Expr) extends Stat { def setBinding(n: String, o: GameObject) = { prop.setBinding(n, o); rhs.setBinding(n, o); this} }
  case class Reset(prop: PropertyRefTrait) extends Stat { def setBinding(n: String, o: GameObject) = { prop.setBinding(n, o); this} }
  case class Block(stats: Seq[Stat]) extends Stat { def setBinding(n: String, o: GameObject) = { stats.foreach(_.setBinding(n, o)); this } }
  case class If(cond: Expr, s1: Stat, s2: Stat) extends Stat { def setBinding(n: String, o: GameObject) = { cond.setBinding(n, o); s1.setBinding(n, o); s2.setBinding(n, o); this } }
  case class Copy(name: String, o: GameObjectRef, b: Block) extends Stat { def setBinding(n: String, obj: GameObject) = { o.setBinding(n, obj); b.setBinding(n, obj); this } }
  case object NOP extends Stat with NoBinding

  /** Expressions, without side-effect. */
  sealed trait Expr extends Tree with Typed {

    def +(e: Expr): Expr = Plus(this, e)
    def -(e: Expr): Expr = Minus(this, e)
    def *(e: Expr): Expr = Times(this, e)
    def /(e: Expr): Expr = Div(this, e)
    def %(e: Expr): Expr = Mod(this, e)
    def &&(e: Expr): Expr = And(this, e)
    def ||(e: Expr): Expr = Or(this, e)
    def =:=(e: Expr): Expr = Equals(this, e)
    def <(e: Expr): Expr = LessThan(this, e)
    def <=(e: Expr): Expr = LessEq(this, e)
    def >(e: Expr): Expr = GreaterThan(this, e)
    def >=(e: Expr): Expr = GreaterEq(this, e)
    def unary_! : Expr = Not(this)

    /*def :=(expr: Expr): Stat = throw new Exception(s"$this is not assignable $expr")
    def +=(expr: Expr): Stat = throw new Exception(s"$this is not assignable $expr")
    def -=(expr: Expr): Stat = throw new Exception(s"$this is not assignable $expr")
    def *=(expr: Expr): Stat = throw new Exception(s"$this is not assignable $expr")
    def /=(expr: Expr): Stat = throw new Exception(s"$this is not assignable $expr")*/
    def property: Property[_] = throw new Exception(s"$this is not a property reference")
    def reset(): Stat = throw new Exception(s"$this is not a property reference and thus reset cannot be called")
    
    def copy: Expr = this // TODO: to change in the future
  }

  case class IntegerLiteral(value: Int) extends Expr with NoBinding
  case class FloatLiteral(value: Float) extends Expr with NoBinding
  case class StringLiteral(value: String) extends Expr with NoBinding
  case class BooleanLiteral(value: Boolean) extends Expr with NoBinding
  case class Vec2Expr(x: Expr, y: Expr) extends Expr with NoBinding
  case class Vec2Literal(x: Float, y: Float) extends Expr with NoBinding
  case object UnitLiteral extends Expr with NoBinding
  case class Val(name: String) extends Expr with NoBinding

  sealed trait LeftRightBinding extends Expr {
    val lhs: Expr
    val rhs: Expr
    def setBinding(n: String, o: GameObject) = { lhs.setBinding(n, o); rhs.setBinding(n, o); this }
  }
  case class Plus(lhs: Expr, rhs: Expr) extends LeftRightBinding
  case class Minus(lhs: Expr, rhs: Expr) extends LeftRightBinding
  case class Times(lhs: Expr, rhs: Expr) extends LeftRightBinding
  case class Div(lhs: Expr, rhs: Expr) extends LeftRightBinding
  case class Mod(lhs: Expr, rhs: Expr) extends LeftRightBinding

  case class And(lhs: Expr, rhs: Expr) extends LeftRightBinding
  case class Or(lhs: Expr, rhs: Expr) extends LeftRightBinding
  case class Equals(lhs: Expr, rhs: Expr) extends LeftRightBinding
  case class LessThan(lhs: Expr, rhs: Expr) extends LeftRightBinding
  case class LessEq(lhs: Expr, rhs: Expr) extends LeftRightBinding
  case class GreaterThan(lhs: Expr, rhs: Expr) extends LeftRightBinding
  case class GreaterEq(lhs: Expr, rhs: Expr) extends LeftRightBinding
  case class Not(expr: Expr) extends Expr { def setBinding(n: String, o: GameObject) = { expr.setBinding(n, o); this } }
  case class On(cond: Expr) extends Expr { def setBinding(n: String, o: GameObject) = { cond.setBinding(n, o); this } }
  case class Once(cond: Expr) extends Expr { def setBinding(n: String, o: GameObject) = { cond.setBinding(n, o); this } }

  case class GameObjectRef(ref: String, var obj: GameObject) extends /*GameObject(StringLiteral(ref)) with*/ Expr {
    def setBinding(n: String, o: GameObject): this.type = {
      if(n == ref) obj = o
      this
    }
    val angle: ch.epfl.lara.synthesis.kingpong.objects.Property[Float] = if(obj != null) obj.angle else null
    def contains(pos: ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface.Vec2): Boolean = if(obj != null) obj.contains(pos) else false
    def getAABB(): ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface.AABB = if(obj != null) obj.getAABB() else null
    val visible: ch.epfl.lara.synthesis.kingpong.objects.Property[Boolean] = if(obj != null) obj.visible else null
    def x: Property[Float] = if(obj != null) obj.x else null
    def y: Property[Float] = if(obj != null) obj.y else null
    def apply(property: String): PropertyRefRef = PropertyRefRef(ref, obj, property)
    def update(property: String, arg: Expr): Stat = PropertyRefRef(ref, obj, property) := arg
   
    def category = if(obj != null) obj.category else null
    def category_=(c: Category) = if(obj != null) obj.category = c
    def properties = if(obj != null) obj.properties else null //super.properties
    def name = if(obj != null) obj.name else null //super.name
    def setCategory(c: Category) = { if(obj != null) obj.setCategory(c) else {} /*super.setCategory(c)*/  ;this }
    def copy(name: String)(blocks: Seq[Stat]) = Copy(name, this, Block(blocks))
    /*def makecopy(name: String): GameObject = {
      val thecopy = this.copy()
      thecopy
    }*/
    def delete() = this("deleted") := BooleanLiteral(true)
  }
  case class FingerMoveOver(o: GameObjectRef) extends Expr with OBinding
  case class FingerDownOver(o: GameObjectRef) extends Expr with OBinding
  case class FingerUpOver(o: GameObjectRef) extends Expr with OBinding
  case object FingerCoordX1 extends Expr with NoBinding
  case object FingerCoordY1 extends Expr with NoBinding
  case object FingerCoordX2 extends Expr with NoBinding
  case object FingerCoordY2 extends Expr with NoBinding
  case class Collision(lhs: GameObjectRef, rhs: GameObjectRef) extends Expr with LeftRightBinding 

  sealed trait PropertyRefTrait extends Expr {
    def :=(expr: Expr): Stat = Assign(this, expr)
    def +=(expr: Expr): Stat = Assign(this, Plus(this, expr))
    def -=(expr: Expr): Stat = Assign(this, Minus(this, expr))
    def *=(expr: Expr): Stat = Assign(this, Times(this, expr))
    def /=(expr: Expr): Stat = Assign(this, Div(this, expr))
    
    def property: Property[_]
    override def reset(): Stat = Reset(this)
    
    /** Return the internal type of this property. */
    private[kingpong] def getPongType: Type = property.getPongType
    
    /** Set the next value if this property. */
    private[kingpong] def setNext(v: Value) = {
      property.setNext(v)
      this
    }
    /** Get the current value of this property. */
    private[kingpong] def get: Value = property.getPongValue
  }
  case class PropertyRef(override val property: Property[_]) extends PropertyRefTrait with NoBinding {
    
  }
  case class PropertyRefRef(ref: String, var obj: GameObject, prop: String) extends PropertyRefTrait {
    var expr: Expr = null
    override def property = obj(prop).property
    def setBinding(n: String, o: GameObject): this.type = {
      if(n == ref) {
        obj = o
        expr = obj(prop) // Can be of any type.
      }
      this
    }
  }
}
