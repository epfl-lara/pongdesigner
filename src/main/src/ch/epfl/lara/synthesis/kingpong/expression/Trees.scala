package ch.epfl.lara.synthesis.kingpong.expression

// remove the warning 
import language.existentials
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import android.util.Log
import ch.epfl.lara.synthesis.kingpong.rules.Context
import util.control.Breaks._

object Trees {
  /**
   * How the tree is traversed:
   * - ContinueWithChildren: After the function is applied to the node,
   *   the same function is applied to the children of this node.
   * - ContinueSiblings: After the function is applied to the node, the
   *   function is not applied to the children, but to its siblings
   * - Stopsiblings: After the function is applied to the node, it
   *   is not applied to the siblings, but to its uncles and children.
   * - Interrupt: Interrups the whole traversing
   */
  object TraverseMode extends Enumeration {
    type TraverseMode = Value
    val ContinueWithChildren, ContinueSiblings, StopSiblings, Interrupt = Value
  }
  /**
   * How the traverse ended:
   * - ContinueChildrenReturn: Indicate that the function can traverse other children
   * - ContinueSiblingsReturn: Indicates that the functions cannot traverse other children,
   *   but can traverse the parent's children
   * - InterruptReturn: Indicates that the function cannot traverse anymore.
   */
  object TraverseReturnMode extends Enumeration {
    type TraverseReturnMode = Value
    val ContinueChildrenReturn, ContinueSiblingsReturn, InterruptReturn = Value
  }
  import TraverseMode.{Value => _, _}
  import TraverseReturnMode.{Value => _, _}
  
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
    /** Continue to traverse the tree depending on the result, it continues to traverse the children.*/
    def traverse(nodeTraverser: Tree => TraverseMode): TraverseReturnMode = {
      nodeTraverser(this) match {
        case ContinueWithChildren =>
          children.foreach{ child =>
            child.traverse(nodeTraverser) match {
              case ContinueChildrenReturn =>
              case ContinueSiblingsReturn => return ContinueChildrenReturn
              case InterruptReturn =>  return InterruptReturn
            }
          }
          ContinueSiblingsReturn
        case ContinueSiblings => ContinueChildrenReturn
        case StopSiblings     => ContinueSiblingsReturn
        case Interrupt        => InterruptReturn
      }
    }
    /*def traverseReplace(nodeTraverser: Tree => (Tree, List[Stat])): (Tree, TraverseReturnMode) = {
      nodeTraverser(this) match {
        case (t, ifFalseStats, ContinueWithChildren) =>
          
        case (t, ifFalseStats, ContinueSiblings) =>
        
        case (t, ifFalseStats, StopSiblings) =>
          
        case (t, ifFalseStats, Interrupt) =>
          
      }
    }*/
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
  
  sealed trait Prioritized extends Tree {
    private var mPriority = 0f
    def setPriority(p: Float): this.type = { mPriority = p ; this }
    def priority = mPriority
  }
  
  object Expr {
    def recursiveFlattenStat(l: Stat): Stat = {
      l match {
        case Block(a) => recursiveFlattenBlock(a.toList) match {
          case Nil => NOP
          case a::Nil => a
          case l => Block(l)
        }
        case ParExpr(a) => ParExpr(recursiveFlattenParallel(a))
        case _ => l
      }
    }
    def recursiveFlattenBlock(l: List[Stat]): List[Stat] = {
      l match {
        case Nil => Nil
        case (p@ParExpr(a))::q => recursiveFlattenStat(p)::recursiveFlattenBlock(q)
        case Block(a)::q => recursiveFlattenBlock((a ++ q).toList)
        case If(condition, codeIfTrue, codeIfFalse)::q =>
          If(condition, recursiveFlattenStat(codeIfTrue), recursiveFlattenStat(codeIfFalse))::recursiveFlattenBlock(q)
        case a::q => a::recursiveFlattenBlock(q)
      }
    }
    def recursiveFlattenParallel(l: List[Stat]): List[Stat] = {
      l match {
        case Nil => Nil
        case (b@Block(l))::q => recursiveFlattenStat(b) :: recursiveFlattenParallel(q)
        case ParExpr(l)::q => recursiveFlattenParallel(l ++ q)
        case If(condition, codeIfTrue, codeIfFalse)::q =>
          If(condition, recursiveFlattenStat(codeIfTrue) , recursiveFlattenStat(codeIfFalse))::recursiveFlattenParallel(q)
        case a::q => a::recursiveFlattenParallel(q)
      }
    } // Ensures that none of the elements of the resulting list is a ParallelExpression
  }

  /** Statement, can have side-effect. */
  sealed trait Stat extends Tree with Prioritized {
    override def setBindingReplace(n: String, obj: GameObject): Stat
    def evaluate(interpreter: Interpreter)(implicit context: Context): Unit = {
      interpreter.eval(this)
    }
    def Else(ifFalse: Stat) = {
      this match {
        case If(cond, ifTrue, NOP) =>
          If(cond, ifTrue, ifFalse)
        case _ => this
      }
    }
    def toList() = this match {
      case NOP => Nil
      case e => List(e)
    }
  }
  
  sealed trait RuleIterator extends Stat /*extends History*/ {
    
    //private var state: Map[Keys, Boolean] = Map.empty.withDefaultValue(false)
    
    /** Contains the history. The head corresponds to the most recent value. */
    //private val history: RingBuffer[(Long, Map[Keys, Boolean])] = new RingBuffer(History.MAX_HISTORY_SIZE)
 
    trait BindingGenerator extends (Keys => Stat) {
      def apply(keys: Keys): Stat
    }
    implicit class BindingGeneratorIterator(r: Stat) extends BindingGenerator {
      def apply(keys: Keys): Stat = {
        keys foreach { case (name, obj) =>
          r.setBinding(name, obj)
        }
        r
      }
    }
    class UniqueGenerator(r: Stat) extends BindingGenerator {
      def apply(keys: Keys): Stat = r
    }
    protected type Keys <: Iterable[(String, GameObject)]
    protected def generator: BindingGenerator
    protected def keys: Iterable[Keys]

    def typeCheck(typechecker: TypeChecker): Unit = {
      (keys map generator) foreach typechecker.typeCheck
    }
    
    def children = (keys map generator).toSeq

    /** Evaluate the rules according to the previous evaluations flags. */
    override def evaluate(interpreter: Interpreter)(implicit context: Context): Unit = {
      val c = keys
      c foreach { key =>
        interpreter.eval(generator(key))
      }
    }

    /** Reset the rules evaluation flags. */
    //def reset(): Unit = state = Map.empty.withDefaultValue(false)

    /*def save(t: Long): Unit = {
      if (history.isEmpty || history.last._2 != state) {
        history += (t, state)
      }
    }*/

    /*def restore(t: Long): Unit = {
      history.findLast(_._1 <= t) match {
        case Some((_, s)) => state = s
        case None => sys.error(s"The timestamp $t doesn't exist in the history.")
      }
    }*/

    //def clear(): Unit = history.clear()

  }
  
  case class Foreach1(category: Category, nameBinding: String, protected val rule: Stat) extends RuleIterator {
    protected type Keys = Seq[(String, GameObject)]
    protected def keys = category.objects.map(o => List((nameBinding, o)))
    def generator: BindingGenerator = rule
    
    def setBinding(n: String, o: GameObject) = { if(nameBinding != n) rule.setBinding(n, o); this}
    def setBindingReplace(n: String, o: GameObject): RuleIterator = {
      if(nameBinding != n) Foreach1(category, nameBinding, rule.setBindingReplace(n, o)) else this
    }
    /*def children = rule :: Nil*/
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
  // TODO : delete reset.
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
  
  case class ParExpr(exprs: List[Stat]) extends Stat {
    def setBinding(n: String, o: GameObject) = { exprs.foreach(_.setBinding(n, o)); this }
    def setBindingReplace(n: String, o: GameObject): ParExpr = ParExpr(exprs.map(_.setBindingReplace(n, o)))
    def children = exprs
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
    def unary_- : Expr = this match {
      case IntegerLiteral(j) => IntegerLiteral(-j)
      case FloatLiteral(j) => FloatLiteral(-j)
      case _ => Minus(IntegerLiteral(0), this)
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
    
    def x = NValue(this, 0)
    def y = NValue(this, 1)
    
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
  case class NValue(o: Expr, index: Int) extends Expr with OBinding { override def setBindingReplace(n: String, obj: GameObject): NValue = NValue(o.setBindingReplace(n, obj), index) }
  case class Count(category: Category) extends Expr with NoBinding with NoReplaceExprBinding
  case class IntegerLiteral(value: Int) extends Expr with NoBinding with NoReplaceExprBinding
  case class FloatLiteral(value: Float) extends Expr with NoBinding with NoReplaceExprBinding
  case class StringLiteral(value: String) extends Expr with NoBinding with NoReplaceExprBinding
  case class BooleanLiteral(value: Boolean) extends Expr with NoBinding with NoReplaceExprBinding
  case class VecExpr(l: List[Expr]) extends Expr with ListBinding[VecExpr]  { def constructor = VecExpr.apply }
  case class Vec2Expr(lhs: Expr, rhs: Expr) extends LeftRightBinding { def constructor = Vec2Expr.apply }
  case class Vec2Literal(lhs: Float, rhs: Float) extends Expr with NoBinding with NoReplaceExprBinding
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

  object GameObjectRef {
    def apply(o: GameObject): GameObjectRef = GameObjectRef(null, o)
  }
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
    def apply(property: String): Expr = if(ref == null && obj != null) obj.get(property) else PropertyIndirect(ref, obj, property)
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
    def isProperty: Boolean = this match {
      case PropertyRef(_) => true
      case e @ PropertyIndirect(_, _, _) => e.expr match {
        case PropertyRef(_) => true
        case _ => false
      }
    }
    def getProperty: Option[Property[_]] = this match {
      case PropertyRef(property) => Some(property)
      case e @ PropertyIndirect(_, _, _) => e.expr match {
        case PropertyRef(property) => Some(property)
        case _ => None
      }
    }
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
    var expr: Expr = if(obj != null) obj.get(prop) else null
    var previous_obj: GameObject = obj // Caching mechanism to have expr computed only once.

    def setBinding(n: String, o: GameObject): this.type = {
      if(n == name && o != previous_obj) {
        obj = o
        expr = obj.get(prop) // Can be of any type, like PropertyRef(x), PropertyRef(x)+PropertyRef(width)/2, etc.
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
