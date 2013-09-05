package ch.epfl.lara.synthesis.kingpong.expression

// remove the warning 
import language.existentials
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import android.util.Log

object Trees {
  trait Writer {
    def add(s: CharSequence): Unit
  }
  
  implicit class RichTree(e: Tree) {
    def ::(other: Tree): List[Tree] = List(other, e)
  }

  sealed trait Tree {
    def setBinding(n: String, o: GameObject): this.type
    def setBindingReplace(n: String, o: GameObject): Tree // Same as setBinding, but replaces the object indirect properties with real formulas.
    def children: Seq[Tree]
    def traverse[B](f: Tree => B): Unit = {
      f(this)
      children.foreach(_.traverse(f))
    }
  }
  sealed trait NoBinding extends Tree {
     def setBinding(n: String, o: GameObject) = { this }
     def children = List()
  }
  sealed trait NoReplaceStatBinding extends Stat {
    def setBindingReplace(n: String, obj: GameObject): Stat = {this}
  }
  sealed trait NoReplaceExprBinding extends Expr {
    def setBindingReplace(n: String, obj: GameObject): Expr = {this}
  }
  sealed trait OBinding extends Tree { self =>
    def o: Tree
    def setBinding(n: String, ob: GameObject) = { o.setBinding(n, ob); this }
    def children = List(o)
  }

  /** Statement, can have side-effect. */
  sealed trait Stat extends Tree {
    override def setBindingReplace(n: String, obj: GameObject): Stat
  }

  case class Assign(props: List[MaybeAssignable], rhs: Expr) extends Stat {
    def setBinding(n: String, o: GameObject) = { props.map(_.setBinding(n, o)); rhs.setBinding(n, o); this}
    def setBindingReplace(n: String, o: GameObject): Assign = {
      props.map(_.setBindingReplace(n, o)) match {
        case c if c.forall(_.isInstanceOf[MaybeAssignable]) => Assign(c.asInstanceOf[List[MaybeAssignable]], rhs.setBindingReplace(n, o))
        case c => throw new Exception(s"$c cannot be assigned $rhs.")
      }
    }
    def children = rhs :: props
  }
  case class Reset(prop: MaybeAssignable) extends Stat {
    def setBinding(n: String, o: GameObject) = { prop.setBinding(n, o); this}
    def setBindingReplace(n: String, o: GameObject): Reset = {
      prop.setBindingReplace(n, o) match {
        case c:MaybeAssignable => Reset(c)
        case c => throw new Exception(s"$c cannot be reset.")
      }
      
    }
    def children = List(prop)
  }
  case class Block(stats: Seq[Stat]) extends Stat {
    def setBinding(n: String, o: GameObject) = { stats.foreach(_.setBinding(n, o)); this }
    def setBindingReplace(n: String, o: GameObject): Block = Block(stats.map(_.setBindingReplace(n, o)))
    def children = stats
  }
  case class If(cond: Expr, s1: Stat, s2: Stat) extends Stat { def setBinding(n: String, o: GameObject) = { cond.setBinding(n, o); s1.setBinding(n, o); s2.setBinding(n, o); this }
    def setBindingReplace(n: String, o: GameObject): If = If(cond.setBindingReplace(n, o), s1.setBindingReplace(n, o), s2.setBindingReplace(n, o))
    def children = cond :: s1 :: s2
  }
  case class Copy(name: String, o: GameObjectRef, b: Block) extends Stat { def setBinding(n: String, obj: GameObject) = { o.setBinding(n, obj); b.setBinding(n, obj); this } 
    def setBindingReplace(n: String, obj: GameObject): Copy = Copy(name, o.setBindingReplace(n, obj), b.setBindingReplace(n, obj))
    def children = o :: b
  }
  case class Delete(name: String, o: GameObjectRef) extends Stat { def setBinding(n: String, obj: GameObject) = { o.setBinding(n, obj); this } 
    def setBindingReplace(n: String, obj: GameObject): Delete = Delete(name, o.setBindingReplace(n, obj))
    def children = List(o)
  }
  case object NOP extends Stat with NoBinding with NoReplaceStatBinding

  case class IfFunc(cond: Expr, s1: Expr, s2: Expr) extends Expr { def setBinding(n: String, o: GameObject) = { cond.setBinding(n, o); s1.setBinding(n, o); s2.setBinding(n, o); this }
    def setBindingReplace(n: String, obj: GameObject): IfFunc = IfFunc(cond.setBindingReplace(n, obj), s1.setBindingReplace(n, obj), s2.setBindingReplace(n, obj))
    def children = cond :: s1 :: s2
  }
  
  /** Expressions, without side-effect. */
  sealed trait Expr extends Tree with Typed {
    override def setBindingReplace(n: String, o: GameObject): Expr
    def +(e: Expr): Expr = e match {
      case IntegerLiteral(0) | FloatLiteral(0) => this
      case IntegerLiteral(i) => this match {
        case IntegerLiteral(j) => IntegerLiteral(i + j)
        case FloatLiteral(j) => FloatLiteral(i + j)
        case _ => Plus(this, e)
      }
      case FloatLiteral(i) => this match {
        case IntegerLiteral(j) => FloatLiteral(i + j)
        case FloatLiteral(j) => FloatLiteral(i + j)
        case _ => Plus(this, e)
      }
      case _ => this match {
        case IntegerLiteral(0) | FloatLiteral(0) => e
        case _ => Plus(this, e)
      }
    }
    def -(e: Expr): Expr = e match {
      case IntegerLiteral(0) | FloatLiteral(0) => this
      case _ => Minus(this, e)
    }
    def *(e: Expr): Expr = e match {
      case IntegerLiteral(0) | FloatLiteral(0) => e
      case IntegerLiteral(1) | FloatLiteral(1) => this
      case IntegerLiteral(-1) | FloatLiteral(-1) => this match {
        case IntegerLiteral(i) => IntegerLiteral(-i)
        case FloatLiteral(i) => FloatLiteral(-i)
        case _ => Times(this, e)
      }
      case FloatLiteral(i) => this match {
        case IntegerLiteral(j) => FloatLiteral(j*i)
        case FloatLiteral(j) => FloatLiteral(j*i)
        case _ => Times(this, e)
      }
      case _ => this match {
        case IntegerLiteral(0) | FloatLiteral(0) => this
        case _ => Times(this, e)
      }
    }
    def /(e: Expr): Expr = e match {
      case IntegerLiteral(1) | FloatLiteral(1) => this
      case IntegerLiteral(-1) | FloatLiteral(-1) => this match {
        case IntegerLiteral(i) => IntegerLiteral(-i)
        case FloatLiteral(i) => FloatLiteral(-i)
        case _ => Div(this, e)
      }
      case FloatLiteral(i) => this match {
        case IntegerLiteral(j) => FloatLiteral(j/i)
        case FloatLiteral(j) => FloatLiteral(j/i)
        case _ => Div(this, e)
      }
      case _ => this match {
        case IntegerLiteral(0) | FloatLiteral(0) => this
        case _ => Div(this, e)
      }
    }
    def %(e: Expr): Expr = Mod(this, e)
    def &&(e: Expr): Expr = And(this, e)
    def ||(e: Expr): Expr = Or(this, e)
    def =:=(e: Expr): Expr = Equals(this, e)
    def <(e: Expr): Expr = LessThan(this, e)
    def <=(e: Expr): Expr = LessEq(this, e)
    def >(e: Expr): Expr = GreaterThan(this, e)
    def >=(e: Expr): Expr = GreaterEq(this, e)
    def unary_! : Expr = Not(this)
    
    /*def above(e: Expr): Expr = this < e
    def below(e: Expr): Expr = this > e
    def toLeftOf(e: Expr): Expr = this < e
    def toRightOf(e: Expr): Expr = this > e*/

    def :=(expr: Expr): Stat = {
      this match {
        case p@VecExpr(l) => Assign(l collect { case m: MaybeAssignable => m }, expr)
        case p:MaybeAssignable => p := expr
        case Plus(p:MaybeAssignable, part) => p := expr - part
        case Minus(p:MaybeAssignable, part) => p := expr + part
        case Times(p:MaybeAssignable, part) => p := expr / part
        case Div(p:MaybeAssignable, part) => p := expr * part
        case _ => throw new Exception(s"$this is not assignable $expr")
      }
    }
    def reset(): Stat = {
      this match {
        case p:MaybeAssignable => Reset(p)
        case _ => throw new Exception(s"$this is not resetable")
      }
    }
    /*def +=(expr: Expr): Stat = throw new Exception(s"$this is not assignable += $expr")
    def -=(expr: Expr): Stat = throw new Exception(s"$this is not assignable -= $expr")
    def *=(expr: Expr): Stat = throw new Exception(s"$this is not assignable *= $expr")
    def /=(expr: Expr): Stat = throw new Exception(s"$this is not assignable /= $expr")*/
    //def property: Property[_] = throw new Exception(s"$this is not a property reference")
    //def reset(): Stat = throw new Exception(s"$this is not a property reference and thus reset cannot be called")
    
    def copy: Expr = this // TODO: to change in the future
  }
  
  sealed trait ListBinding[T <: Expr] extends Expr {
    val l: List[Expr]
    def setBinding(n: String, o: GameObject) = { l.foreach(_.setBinding(n, o)); this }
    override def setBindingReplace(n: String, o: GameObject): T = constructor(l.map(_.setBindingReplace(n, o)))
    def constructor: (List[Expr]) => T
    def children = l
  }

  case class IntegerLiteral(value: Int) extends Expr with NoBinding with NoReplaceExprBinding
  case class FloatLiteral(value: Float) extends Expr with NoBinding with NoReplaceExprBinding
  case class StringLiteral(value: String) extends Expr with NoBinding with NoReplaceExprBinding
  case class BooleanLiteral(value: Boolean) extends Expr with NoBinding with NoReplaceExprBinding
  case class VecExpr(l: List[Expr]) extends Expr with ListBinding[VecExpr]  { def constructor = VecExpr.apply }
  case class Vec2Expr(lhs: Expr, rhs: Expr) extends LeftRightBinding { def constructor = Vec2Expr.apply }
  case class Vec2Literal(x: Float, y: Float) extends Expr with NoBinding with NoReplaceExprBinding
  case object UnitLiteral extends Expr with NoBinding with NoReplaceExprBinding
  case class Val(name: String) extends Expr with NoBinding with NoReplaceExprBinding

  sealed trait LeftRightBinding extends Expr {
    val lhs: Expr
    val rhs: Expr
    def setBinding(n: String, o: GameObject) = { lhs.setBinding(n, o); rhs.setBinding(n, o); this }
    override def setBindingReplace(n: String, o: GameObject): Expr = constructor(lhs.setBindingReplace(n, o), rhs.setBindingReplace(n, o))
    def constructor: (Expr, Expr) => Expr
    def children = lhs :: rhs
  }
  case class Plus(lhs: Expr, rhs: Expr) extends LeftRightBinding { def constructor = Plus.apply }
  case class Minus(lhs: Expr, rhs: Expr) extends LeftRightBinding { def constructor = Minus.apply }
  case class Times(lhs: Expr, rhs: Expr) extends LeftRightBinding { def constructor = Times.apply }
  case class Div(lhs: Expr, rhs: Expr) extends LeftRightBinding { def constructor = Div.apply }
  case class Mod(lhs: Expr, rhs: Expr) extends LeftRightBinding { def constructor = Mod.apply }

  case class And(lhs: Expr, rhs: Expr) extends LeftRightBinding { def constructor = And.apply }
  case class Or(lhs: Expr, rhs: Expr) extends LeftRightBinding { def constructor = Or.apply }
  case class Equals(lhs: Expr, rhs: Expr) extends LeftRightBinding { def constructor = Equals.apply }
  case class LessThan(lhs: Expr, rhs: Expr) extends LeftRightBinding { def constructor = LessThan.apply }
  case class LessEq(lhs: Expr, rhs: Expr) extends LeftRightBinding { def constructor = LessEq.apply }
  case class GreaterThan(lhs: Expr, rhs: Expr) extends LeftRightBinding { def constructor = GreaterThan.apply }
  case class GreaterEq(lhs: Expr, rhs: Expr) extends LeftRightBinding { def constructor = GreaterEq.apply }
  case class Not(o: Expr) extends Expr with OBinding { override def setBindingReplace(n: String, obj: GameObject): Expr = Not(o.setBindingReplace(n, obj)) }
  case class On(o: Expr) extends Expr with OBinding { override def setBindingReplace(n: String, obj: GameObject): Expr = On(o.setBindingReplace(n, obj)) }
  case class Once(o: Expr) extends Expr with OBinding { override def setBindingReplace(n: String, obj: GameObject): Expr = Once(o.setBindingReplace(n, obj)) }

  case class GameObjectRef(ref: String, var obj: GameObject) extends Expr {
    def setBinding(n: String, o: GameObject): this.type = {
      if(n == ref) obj = o
      this
    }
    def setBindingReplace(n: String, obj: GameObject): GameObjectRef = {setBinding(n, obj)}
    def children = List()
    val angle: ch.epfl.lara.synthesis.kingpong.objects.Property[Float] = if(obj != null) obj.angle else null
    def contains(pos: ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface.Vec2): Boolean = if(obj != null) obj.contains(pos) else false
    def getAABB(): ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface.AABB = if(obj != null) obj.getAABB() else null
    val visible: ch.epfl.lara.synthesis.kingpong.objects.Property[Boolean] = if(obj != null) obj.visible else null
    //def x: Property[Float] = if(obj != null) obj.x else null
    //def y: Property[Float] = if(obj != null) obj.y else null
    def apply(property: String): Expr = if(ref == null && obj != null) obj(property) else PropertyIndirect(ref, obj, property)
    def update(property: String, arg: Expr): Stat = apply(property) := arg
   
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
    def toLeftOfAtMost(other:GameObjectRef) = this("right") <= other("left")
    def toRightOfAtMost(other:GameObjectRef) = this("left") >= other("right")
    def toLeftOf(other:GameObjectRef) = this("right") =:= other("left")
    def toRightOf(other:GameObjectRef) = this("left") =:= other("right")
    def above(other:GameObjectRef) = this("bottom") <= other("top")
    def below(other:GameObjectRef) = this("top") >=  other("bottom")
    def justAbove(other:GameObjectRef) = this("bottom") =:= other("top")
    def justBelow(other:GameObjectRef) = this("top") =:=  other("bottom")
    def alignLeft(other: GameObjectRef) = this("left") =:= other("left")
    def alignRight(other: GameObjectRef) = this("right") =:= other("right")
    
    def collides(other: GameObjectRef) = Collision(this, other)
  }
  case class FingerMoveOver(o: GameObjectRef) extends Expr with OBinding { override def setBindingReplace(n: String, obj: GameObject): Expr = FingerMoveOver(o.setBindingReplace(n, obj)) }
  case class FingerDownOver(o: GameObjectRef) extends Expr with OBinding { override def setBindingReplace(n: String, obj: GameObject): Expr = FingerDownOver(o.setBindingReplace(n, obj)) }
  case class FingerUpOver(o: GameObjectRef) extends Expr with OBinding { override def setBindingReplace(n: String, obj: GameObject): Expr = FingerUpOver(o.setBindingReplace(n, obj)) }
  case object FingerCoordX1 extends Expr with NoBinding with NoReplaceExprBinding
  case object FingerCoordY1 extends Expr with NoBinding with NoReplaceExprBinding
  case object FingerCoordX2 extends Expr with NoBinding with NoReplaceExprBinding
  case object FingerCoordY2 extends Expr with NoBinding with NoReplaceExprBinding
  case class Collision(lhs: GameObjectRef, rhs: GameObjectRef) extends Expr with LeftRightBinding  { def constructor = (a: Expr, b: Expr) => {
    Collision.apply(a.asInstanceOf[GameObjectRef], b.asInstanceOf[GameObjectRef])} }
  
  sealed trait MaybeAssignable extends Expr {
    override def :=(expr: Expr): Stat = Assign(List(this), expr)
    def +=(expr: Expr): Stat = Assign(List(this), Plus(this, expr))
    def -=(expr: Expr): Stat = Assign(List(this), Minus(this, expr))
    def *=(expr: Expr): Stat = Assign(List(this), Times(this, expr))
    def /=(expr: Expr): Stat = Assign(List(this), Div(this, expr))
    override def setBindingReplace(n: String, o: GameObject): Expr
  }
  /** Reference to a property. */
  case class PropertyRef(val property: Property[_]) extends Expr with NoBinding with MaybeAssignable {
    
    /** Return the internal type of this property. */
    private[kingpong] def getPongType: Type = property.getPongType
    
    /** Set the next value if this property. */
    private[kingpong] def setNext(v: Value) = {
      property.setNext(v)
      this
    }
    /** Get the current value of this property. */
    private[kingpong] def get: Value = property.getPongValue
    
    override def setBindingReplace(n: String, o: GameObject): Expr = this
  }
  
  /** Reference to a property of a dynamically linked object.
   *  name: Name of the object
   **/
  case class PropertyIndirect(name: String, var obj: GameObject, prop: String) extends Expr with MaybeAssignable {
    var expr: Expr = if(obj != null) obj(prop) else null
    var previous_obj: GameObject = obj // Caching mechanism to have expr computed only once.

    def setBinding(n: String, o: GameObject): this.type = {
      if(n == name && o != previous_obj) {
        obj = o
        expr = obj(prop) // Can be of any type, like PropertyRef(x), PropertyRef(x)+PropertyRef(width)/2, etc.
        // TODO : Change the binding
        
        previous_obj = obj
      }
      this
    }
    override def setBindingReplace(n: String, o: GameObject): Expr = {
      if(n == name && o != previous_obj) {
        obj = o
        //expr = obj(prop)
        o.structurally(this, prop)
      } else {
        this
      }
    }
    def children = List()
    override def equals(other: Any): Boolean = {
      other match {
        case p:PropertyIndirect =>
          name == p.name && prop == p.prop
        case _ => false
      }
    }
  }
  
  case class Choose(prop: VecExpr, constraint: Expr) extends Expr {
    def children = if(evaluatedProgram == null) List() else List(evaluatedProgram)
    var evaluatedProgram: Expr = null
    prop.l foreach { _ match {
      case p: MaybeAssignable => 
      case _ => throw new Exception(s"$prop is not a property but it should")
    }}
    
    var expandedConstraint: Expr = null
    def getContraintForSolving = if(expandedConstraint == null) constraint else expandedConstraint
    // Call to Comfusy or Leon?
    // Simple solver for linear constraints

    /*def solve(constraint: Expr, default: MaybeAssignable => Expr = p => p): Expr = {
      constraint match {
        case LessEq(lhs: MaybeAssignable, rhs) if lhs == prop => IfFunc(constraint, default(lhs), rhs)
        case LessEq(Plus(lhs: MaybeAssignable, rhs1), rhs2) if lhs == prop => IfFunc(constraint, default(lhs), rhs2-rhs1)
        case LessEq(Minus(lhs: MaybeAssignable, rhs1), rhs2) if lhs == prop => IfFunc(constraint, default(lhs), rhs2+rhs1)
        case LessThan(lhs: MaybeAssignable, rhs) if lhs == prop => IfFunc(constraint, default(lhs), rhs)
        case LessThan(Plus(lhs: MaybeAssignable, rhs1), rhs2) if lhs == prop => IfFunc(constraint, default(lhs), rhs2-rhs1)
        case LessThan(Minus(lhs: MaybeAssignable, rhs1), rhs2) if lhs == prop => IfFunc(constraint, default(lhs), rhs2+rhs1)
        case GreaterEq(lhs: MaybeAssignable, rhs) if lhs == prop => IfFunc(constraint, default(lhs), rhs)
        case GreaterEq(Plus(lhs: MaybeAssignable, rhs1), rhs2) if lhs == prop => IfFunc(constraint, default(lhs), rhs2-rhs1)
        case GreaterEq(Minus(lhs: MaybeAssignable, rhs1), rhs2) if lhs == prop => IfFunc(constraint, default(lhs), rhs2+rhs1)
        case GreaterThan(lhs: MaybeAssignable, rhs) if lhs == prop => IfFunc(constraint, default(lhs), rhs)
        case GreaterThan(Plus(lhs: MaybeAssignable, rhs1), rhs2) if lhs == prop => IfFunc(constraint, default(lhs), rhs2-rhs1)
        case GreaterThan(Minus(lhs: MaybeAssignable, rhs1), rhs2) if lhs == prop => IfFunc(constraint, default(lhs), rhs2+rhs1)
        case Equals(Plus(lhs: MaybeAssignable, rhs1), rhs2) if lhs == prop => rhs2 - rhs1
        case Equals(Minus(lhs: MaybeAssignable, rhs1), rhs2) if lhs == prop => rhs2 + rhs1
        case Equals(lhs: MaybeAssignable, rhs) if lhs == prop => rhs
        case And(lhs, rhs) => solve(lhs, p => solve(rhs)) // Solve the first constraint. If first constraint satisfied, solve the second constraint.
        case _ => //Log.d("Trees.scala", s"Unable to solve constraint $constraint")
          prop
      }
    }*/
    def setBinding(n: String, o: GameObject): this.type = {
      //if(evaluatedProgram != null) evaluatedProgram.setBinding(n, o)
      //else { // First time there is a binding, it will help us to solve the equations.
      if(evaluatedProgram == null) {// Set the binding 
        prop.setBinding(n, o)
        expandedConstraint = constraint.setBindingReplace(n, o)
      }
      this
    }
    override def setBindingReplace(n: String, o: GameObject): Expr = {
      //if(evaluatedProgram != null) evaluatedProgram.setBinding(n, o)
      //else { // First time there is a binding, it will help us to solve the equations.
      Choose(prop.setBindingReplace(n, o), constraint.setBindingReplace(n, o))
    }
    
    //evaluatedProgram = solve(constraint)
  }
}
