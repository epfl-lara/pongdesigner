package ch.epfl.lara.synthesis.kingpong.expression

// remove the warning 
import language.existentials
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
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

  implicit class RichTree(e: Tree) {
    def ::(other: Tree): List[Tree] = List(other, e)
  }

  sealed trait Tree { self =>
    def replace[T](n: Property[T], m: Property[T]): Tree = {
      copyFromChildren(children map (_.replace(n, m)))
    }
    //def setBinding(n: String, o: GameObject): this.type
    def expandProperties(n: String, o: GameObject): Tree // Same as setBinding, but replaces the object indirect properties with real formulas.
    def children: Seq[Tree]
    def copyFromChildren(newChildren: Seq[Tree]): Tree
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
          ContinueChildrenReturn
        case ContinueSiblings => ContinueChildrenReturn
        case StopSiblings     => ContinueSiblingsReturn
        case Interrupt        => InterruptReturn
      }
    }
    
    def setBinding(n: String, o: GameObject): this.type = {
      traverse {
        case g @ GameObjectRef(StringLiteral(s)) if s == n =>
          g.obj = o
          ContinueWithChildren
        case p @ PropertyIndirect(indirectObject, prop) =>
          if(n == p.name && o != indirectObject.obj) {
            indirectObject.obj = o
            p.expr = o.get(prop)
          }
          ContinueWithChildren
        case c @ Choose(prop, constraint) =>
          if(c.evaluatedProgram == null) {// Set the binding 
            c.expandedConstraint = constraint.expandProperties(n, o)
          }
          ContinueWithChildren
        case _ =>
          ContinueWithChildren
      }
      this
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
  
  case class MethodDecl(retType: Type, id: Val, args: List[Formal], stats: Stat, retExpr: Expr) extends Tree {
    var fastImplementation: List[Value] => Value = null
    def withFastImplementation(f: List[Value] => Value): this.type = {
      fastImplementation = f
      this
    }
    def children = stats :: List(retExpr)
    
    override def copyFromChildren(newChildren: Seq[Tree]): MethodDecl = copy(stats=newChildren(0).asInstanceOf[Stat], retExpr=newChildren(1).asInstanceOf[Expr])
    def expandProperties(n: String, o: GameObject): Tree = {
      stats.expandProperties(n, o)
      retExpr.expandProperties(n, o)
      this
    }
  }
  sealed case class Formal(tpe: Type, id: Val) extends Tree with NoBinding {
    def expandProperties(n: String, o: GameObject): Tree = this
  }
  
  sealed trait NoBinding extends Tree { self =>
     def children = List()
     def copyFromChildren(c: Seq[Tree]): self.type = this
  }
  sealed trait NoReplaceStatBinding extends Stat {
    def expandProperties(n: String, obj: GameObject): Stat = {this}
  }
  sealed trait NoReplaceExprBinding extends Expr {
    def expandProperties(n: String, obj: GameObject): Expr = {this}
  }
  sealed trait OBinding extends Tree { self =>
    def o: Tree
    def children = List(o)
  }
  
  sealed trait Prioritized extends Tree {
    private var mPriority = 0f
    def setPriority(p: Float): this.type = { mPriority = p ; this }
    def priority = mPriority
  }
  
  object Expr {
    /**
     * Takes a stat and recursively flatten its statements.
     */
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
    /**
     * Takes a list of sequential statements and flatten them.
     */
    def recursiveFlattenBlock(l: List[Stat]): List[Stat] = {
      l match {
        case Nil => Nil
        case (p@ParExpr(a))::q => recursiveFlattenStat(p)::recursiveFlattenBlock(q)
        case Block(a)::q => recursiveFlattenBlock((a ++ q).toList)
        case If(condition, codeIfTrue, codeIfFalse)::q =>
          If(condition, recursiveFlattenStat(codeIfTrue), codeIfFalse.map(recursiveFlattenStat(_)))::recursiveFlattenBlock(q)
        case a::q => a::recursiveFlattenBlock(q)
      }
    }
    /**
     * Takes a list of equivalent statements and flattens them.
     */
    def recursiveFlattenParallel(l: List[Stat]): List[Stat] = {
      l match {
        case Nil => Nil
        case (b@Block(l))::q => recursiveFlattenStat(b) :: recursiveFlattenParallel(q)
        case ParExpr(l)::q => recursiveFlattenParallel(l ++ q)
        case If(condition, codeIfTrue, codeIfFalse)::q =>
          If(condition, recursiveFlattenStat(codeIfTrue) , codeIfFalse.map(recursiveFlattenStat(_)))::recursiveFlattenParallel(q)
        case a::q => a::recursiveFlattenParallel(q)
      }
    } // Ensures that none of the elements of the resulting list is a ParallelExpression
  }
  
  object %:: {
    def unapply(s: Stat): Option[(Stat, Stat)] = s match {
      case Block(a::q) => Some((a, Block(q)))
      case e => None
    }
  }

  /** Statement, can have side-effect. */
  sealed trait Stat extends Tree with Prioritized {
    override def expandProperties(n: String, obj: GameObject): Stat
    override def copyFromChildren(newChildren: Seq[Tree]): Stat
    override def replace[T](n: Property[T], m: Property[T]): Stat = {
      copyFromChildren(children map (_.replace(n, m)))
    }
    def evaluate(interpreter: Interpreter)(implicit context: Context): Unit = {
      interpreter.eval(this)
    }
    def Else(ifFalse: Stat) = {
      this match {
        case If(cond, ifTrue, None) =>
          If(cond, ifTrue, Some(ifFalse))
        case _ => this
      }
    }
    def toList() = this match {
      case NOP => Nil
      case e => List(e)
    }
    def ::(other: Stat) = Block(List(other, this))
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

    def typeCheck(typechecker: TypeChecker)(implicit context: Context): Unit = {
      (keys map generator) foreach (typechecker.typeCheck(_))
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
    
    def expandProperties(n: String, o: GameObject): RuleIterator = {
      if(nameBinding != n) Foreach1(category, nameBinding, rule.expandProperties(n, o)) else this
    }
    override def copyFromChildren(newChildren: Seq[Tree]): Foreach1 = copy(rule=newChildren(0).asInstanceOf[Stat])
    /*def children = rule :: Nil*/
  }
  
  case class Assign(props: List[MaybeAssignable], rhs: Expr) extends Stat {
    def expandProperties(n: String, o: GameObject): Assign = {
      props.map(_.expandProperties(n, o)) match {
        case c if c.forall(_.isInstanceOf[MaybeAssignable]) => Assign(c.asInstanceOf[List[MaybeAssignable]], rhs.expandProperties(n, o))
        case c => throw new Exception(s"$c cannot be assigned $rhs.")
      }
    }
    def children = rhs :: props
    override def copyFromChildren(newChildren: Seq[Tree]): Assign = copy(props=newChildren.tail.asInstanceOf[List[MaybeAssignable]], rhs=newChildren(0).asInstanceOf[Expr])
  }
  // TODO : delete reset.
  case class Reset(prop: MaybeAssignable) extends Stat {
    def expandProperties(n: String, o: GameObject): Reset = {
      prop.expandProperties(n, o) match {
        case c:MaybeAssignable => Reset(c)
        case c => throw new Exception(s"$c cannot be reset.")
      }
      
    }
    def children = List(prop)
    override def copyFromChildren(newChildren: Seq[Tree]): Reset = copy(prop=newChildren(0).asInstanceOf[MaybeAssignable])
  }
  object Block {
    def apply(s1: Stat, s: Stat*): Block = {
      Block(List(s1) ++ s.toList)
    }
  }
  case class Block(stats: Seq[Stat]) extends Stat {
    def expandProperties(n: String, o: GameObject): Block = Block(stats.map(_.expandProperties(n, o)))
    def children = stats
    override def copyFromChildren(newChildren: Seq[Tree]): Block = copy(stats=newChildren.asInstanceOf[List[Stat]])
    override def ::(other: Stat) = Block(other::stats.toList)
  }
  object If {
    def apply(cond: Expr, s1: Stat, s2: Stat): If = If(cond, s1, Some(s2))
  }
  case class If(cond: Expr, s1: Stat, s2: Option[Stat]) extends Stat {
    def expandProperties(n: String, o: GameObject): If = If(cond.expandProperties(n, o), s1.expandProperties(n, o), s2.map(_.expandProperties(n, o)))
    def children = List(cond, s1) ++ s2.toList
    override def copyFromChildren(newChildren: Seq[Tree]): If = copy(cond=newChildren(0).asInstanceOf[Expr], s1=newChildren(1).asInstanceOf[Stat], s2=if(newChildren.size >= 3) Some(newChildren(2).asInstanceOf[Stat]) else None)
  }
  case class Copy(name: String, o: GameObjectRef, b: Stat) extends Stat {
    def expandProperties(n: String, obj: GameObject): Copy = Copy(name, o.expandProperties(n, obj), b.expandProperties(n, obj))
    def children = o :: b
    override def copyFromChildren(newChildren: Seq[Tree]): Copy = copy(o=newChildren(0).asInstanceOf[GameObjectRef], b=newChildren(1).asInstanceOf[Stat])
  }
  case class Delete(name: String, o: GameObjectRef) extends Stat {
    def expandProperties(n: String, obj: GameObject): Delete = Delete(name, o.expandProperties(n, obj))
    def children = List(o)
    override def copyFromChildren(newChildren: Seq[Tree]): Delete = copy(o=newChildren(0).asInstanceOf[GameObjectRef])
  }
  case object NOP extends Stat with NoBinding with NoReplaceStatBinding

  case class IfFunc(cond: Expr, s1: Expr, s2: Expr) extends Expr {
    def expandProperties(n: String, obj: GameObject): IfFunc = IfFunc(cond.expandProperties(n, obj), s1.expandProperties(n, obj), s2.expandProperties(n, obj))
    def children = cond :: s1 :: s2
    override def copyFromChildren(newChildren: Seq[Tree]): IfFunc = copy(cond=newChildren(0).asInstanceOf[Expr], s1=newChildren(1).asInstanceOf[Expr], s2=newChildren(2).asInstanceOf[Expr])
  }
  
  case class ParExpr(exprs: List[Stat]) extends Stat {
    def expandProperties(n: String, o: GameObject): ParExpr = ParExpr(exprs.map(_.expandProperties(n, o)))
    def children = exprs
    override def copyFromChildren(newChildren: Seq[Tree]): ParExpr = copy(exprs=newChildren.asInstanceOf[List[Stat]])
  }
  
  /** Expressions, without side-effect. */
  sealed trait Expr extends Tree with Typed {
    override def copyFromChildren(c: Seq[Tree]): Expr
    override def replace[T](n: Property[T], m: Property[T]): Expr = {
      copyFromChildren(children map (_.replace(n, m)))
    }
    override def expandProperties(n: String, o: GameObject): Expr
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
    def =!=(e: Expr): Expr = Not(Equals(this, e))
    def <(e: Expr): Expr = LessThan(this, e)
    def <=(e: Expr): Expr = LessEq(this, e)
    def >(e: Expr): Expr = GreaterThan(this, e)
    def >=(e: Expr): Expr = GreaterEq(this, e)
    def unary_! : Expr = Not(this)
    
    def apply(property: String): Expr = PropertyIndirect(GameObjectRef(this), property)
    def update(property: String, arg: Expr): Stat = Assign(List(this(property).asInstanceOf[MaybeAssignable]), arg)
    
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
    
    //def copy: Expr = this // TODO: to change in the future
  }
  
  sealed trait ListBinding[T <: Expr] extends Expr {
    val l: List[Expr]
    override def expandProperties(n: String, o: GameObject): T = constructor(l.map(_.expandProperties(n, o)))
    def constructor: (List[Expr]) => T
    def children = l
    override def copyFromChildren(newChildren: Seq[Tree]): Expr = constructor(children.asInstanceOf[List[Expr]])
  }
  case class NValue(o: Expr, index: Int) extends Expr with OBinding { override def expandProperties(n: String, obj: GameObject): NValue = NValue(o.expandProperties(n, obj), index) 
    override def copyFromChildren(c: Seq[Tree]): NValue = copy(o = c(0).asInstanceOf[Expr])
  }
  case class Count(category: Category) extends Expr with NoBinding with NoReplaceExprBinding
  val NULL = ObjectLiteral(null)
  case class ObjectLiteral(value: GameObject) extends Expr with NoBinding with NoReplaceExprBinding
  case class IntegerLiteral(value: Int) extends Expr with NoBinding with NoReplaceExprBinding
  case class FloatLiteral(value: Float) extends Expr with NoBinding with NoReplaceExprBinding
  case class StringLiteral(value: String) extends Expr with NoBinding with NoReplaceExprBinding
  case class BooleanLiteral(value: Boolean) extends Expr with NoBinding with NoReplaceExprBinding
  object VecExpr {
    def apply(s: Expr*): VecExpr = VecExpr(s.toList)
  }
  object VecExpr2 {
    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case VecExpr(List(s1: Expr, s2: Expr)) => Some((s1, s2))
      case _ => None
    }
  }
  case class VecExpr(l: List[Expr]) extends Expr with ListBinding[VecExpr]  { def constructor = VecExpr.apply }
  //case class Vec2Expr(lhs: Expr, rhs: Expr) extends LeftRightBinding { def constructor = Vec2Expr.apply }
  case class Vec2Literal(lhs: Float, rhs: Float) extends Expr with NoBinding with NoReplaceExprBinding
  case object UnitLiteral extends Expr with NoBinding with NoReplaceExprBinding
  case class Val(name: String) extends Expr with NoBinding with NoReplaceExprBinding
  case class MethodCall(name: String, l: List[Expr]) extends Expr with ListBinding[MethodCall]  { def constructor = MethodCall.apply(name, _) }

  sealed trait LeftRightBinding extends Expr {
    val lhs: Expr
    val rhs: Expr
    override def expandProperties(n: String, o: GameObject): Expr = constructor(lhs.expandProperties(n, o), rhs.expandProperties(n, o))
    def constructor: (Expr, Expr) => Expr
    def children = lhs :: rhs
    override def copyFromChildren(newChildren: Seq[Tree]): Expr = constructor(newChildren(0).asInstanceOf[Expr], newChildren(1).asInstanceOf[Expr])
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
  case class Not(o: Expr) extends Expr with OBinding { override def expandProperties(n: String, obj: GameObject): Expr = Not(o.expandProperties(n, obj)) 
    override def copyFromChildren(newChildren: Seq[Tree]): Not = copy(o=newChildren(0).asInstanceOf[Expr])
  }
  case class On(o: Expr) extends Expr with OBinding { override def expandProperties(n: String, obj: GameObject): Expr = On(o.expandProperties(n, obj)) 
    override def copyFromChildren(newChildren: Seq[Tree]): On = copy(o=newChildren(0).asInstanceOf[Expr])
  }
  case class Once(o: Expr) extends Expr with OBinding { override def expandProperties(n: String, obj: GameObject): Expr = Once(o.expandProperties(n, obj)) 
    override def copyFromChildren(newChildren: Seq[Tree]): Once = copy(o=newChildren(0).asInstanceOf[Expr])
  }

  object GameObjectRef {
    def apply(o: GameObject): GameObjectRef = GameObjectRef(ObjectLiteral(o))
  }
  
  /**
   * Soft reference to a game object, or a game object if ref is null
   */
  case class GameObjectRef(reference: Expr) extends Expr {
    var ref: String = reference match {case StringLiteral(s) => s case _ => null}
    var obj: GameObject = reference match {case ObjectLiteral(o) => o case _ => null}
    override def equals(other: Any) = other match { case g@GameObjectRef(reference) => reference == g.reference || ref == g.ref || (g.ref == null && ref == null && obj == g.obj) case _ => false}
    
    def expandProperties(n: String, obj: GameObject): GameObjectRef = {setBinding(n, obj)}
    def children = List()
    def contains(pos: ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface.Vec2): Boolean = if(obj != null) obj.contains(pos) else false
    def getAABB(): ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface.AABB = if(obj != null) obj.getAABB() else null
    val visible: ch.epfl.lara.synthesis.kingpong.objects.Property[Boolean] = if(obj != null) obj.visible else null
    //def x: Property[Float] = if(obj != null) obj.x else null
    //def y: Property[Float] = if(obj != null) obj.y else null
    override def apply(property: String): Expr = if(ref == null && obj != null) obj.get(property) else PropertyIndirect(this, property)
    override def update(property: String, arg: Expr): Stat = apply(property) := arg
   
    def category = if(obj != null) obj.category else null
    def category_=(c: CategoryObject) = if(obj != null) obj.category = c
    def properties = if(obj != null) obj.properties else null //super.properties
    def name = if(obj != null) obj.name else null //super.name
    def setCategory(c: CategoryObject) = { if(obj != null) obj.setCategory(c) else {} /*super.setCategory(c)*/  ;this }
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
    def copyFromChildren(newChildren: Seq[Tree]): GameObjectRef =  this
  }
  case class FingerMoveOver(o: GameObjectRef) extends Expr with OBinding { override def expandProperties(n: String, obj: GameObject): Expr = FingerMoveOver(o.expandProperties(n, obj))
    def copyFromChildren(newChildren: Seq[Tree]): FingerMoveOver =  this
  }
  case class FingerDownOver(o: GameObjectRef) extends Expr with OBinding { override def expandProperties(n: String, obj: GameObject): Expr = FingerDownOver(o.expandProperties(n, obj)) 
    def copyFromChildren(newChildren: Seq[Tree]): FingerDownOver =  this
  }
  case class FingerUpOver(o: GameObjectRef) extends Expr with OBinding { override def expandProperties(n: String, obj: GameObject): Expr = FingerUpOver(o.expandProperties(n, obj)) 
    def copyFromChildren(newChildren: Seq[Tree]): FingerUpOver =  this
  }
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
    override def expandProperties(n: String, o: GameObject): Expr
    def isProperty: Boolean = this match {
      case PropertyRef(_) => true
      case e @ PropertyIndirect(_, _) => e.expr match {
        case PropertyRef(_) => true
        case _ => false
      }
    }
    def getProperty: Option[Property[_]] = this match {
      case PropertyRef(property) => Some(property)
      case e @ PropertyIndirect(_, _) => e.expr match {
        case PropertyRef(property) => Some(property)
        case _ => None
      }
    }
  }
  /** Reference to a hard-coded property. */
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
    
    override def expandProperties(n: String, o: GameObject): Expr = this
    override def replace[T](n: Property[T], m: Property[T]): PropertyRef = if(n == property) m.ref else this
  }
  
  /** Reference to a property of a dynamically linked object.
   *  name: Name of the object
   **/
  case class PropertyIndirect(indirectObject: GameObjectRef, prop: String) extends Expr with MaybeAssignable {
    def obj = indirectObject.obj
    def name = indirectObject.ref
    var expr: Expr = if(obj != null) obj.get(prop) else null

    override def expandProperties(n: String, o: GameObject): Expr = {
      if(n == name && o != indirectObject.obj) {
        indirectObject.obj = o
        //expr = obj(prop)
        o.structurally(this, prop)
      } else {
        this
      }
    }
    def children = List(indirectObject)
    def copyFromChildren(newChildren: Seq[Tree]): PropertyIndirect =  copy(indirectObject=newChildren(0).asInstanceOf[GameObjectRef])
    override def replace[T](n: Property[T], m: Property[T]): Expr = {
      expr match {
        case null => this
        case p: MaybeAssignable if p.getProperty.get == n => 
          m.ref
          //PropertyIndirect(indirectObject, m.name)
        case _ => 
          this
      }
    }

    override def equals(other: Any): Boolean = {
      other match {
        case p:PropertyIndirect =>
          name == p.name && prop == p.prop
        case _ => false
      }
    }
  }
  
  /**
   * Choose construct to choose some assignments given the constraint.
   */
  case class Choose(prop: VecExpr, constraint: Expr) extends Expr {
    def children = if(evaluatedProgram == null) List(prop, constraint) else List(prop, constraint, evaluatedProgram)
    def copyFromChildren(newChildren: Seq[Tree]): Choose = {
      val res = copy(prop=newChildren(0).asInstanceOf[VecExpr], constraint=newChildren(1).asInstanceOf[Expr])
      if(newChildren.size >= 0) {
        res.evaluatedProgram = newChildren(2).asInstanceOf[Expr]
      }
      res
    }
    
    var evaluatedProgram: Expr = null
    prop.l foreach { _ match {
      case p: MaybeAssignable => 
      case _ => throw new Exception(s"$prop is not a property but it should")
    }}
    
    var expandedConstraint: Expr = null
    def getContraintForSolving = if(expandedConstraint == null) constraint else expandedConstraint
    override def expandProperties(n: String, o: GameObject): Expr = {
      //if(evaluatedProgram != null) evaluatedProgram.setBinding(n, o)
      //else { // First time there is a binding, it will help us to solve the equations.
      Choose(prop.expandProperties(n, o), constraint.expandProperties(n, o))
    }
    
    //evaluatedProgram = solve(constraint)
  }
}
