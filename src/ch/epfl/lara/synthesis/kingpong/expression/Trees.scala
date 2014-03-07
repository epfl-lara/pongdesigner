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
  
  sealed trait NoBinding { self: Tree =>
     def children = Nil
  }
 
  sealed trait OBinding { self: Tree =>
    def o: Tree
    def children = List(o)
  }
  
  sealed trait Prioritized { self: Tree =>
    private var _priority = 0f
    def setPriority(p: Float): self.type = { _priority = p ; self }
    def priority = _priority
  }
  
  object Tree {
    private[kingpong] def replace0[T](n: Property[T], m: Property[T]): PartialFunction[Expr, Expr] = {
      case prop: PropertyRef      => prop.replace(n, m)
      case prop: PropertyIndirect => prop.replace(n, m)
    }
  }

  sealed trait Tree { self =>
    
    def transform(f: PartialFunction[Expr, Expr]): Tree
    
    // Same as setBinding, but replaces the object indirect properties with real formulas.
    def expandProperties(n: String, o: GameObject): Tree
    
    def children: Seq[Tree]
    
    def replace[T](n: Property[T], m: Property[T]): Tree
        
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
            p.expr = o(prop)
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

  }
  
  case class MethodDecl(retType: Type, id: Val, args: List[Formal], stats: Stat, retExpr: Expr) extends Tree {
    var fastImplementation: List[Value] => Value = null
    def withFastImplementation(f: List[Value] => Value): this.type = {
      fastImplementation = f
      this
    }
    def children = stats :: List(retExpr)
    
    def transform(f: PartialFunction[Expr, Expr]): MethodDecl = {
      MethodDecl(retType, id, args, stats.transform(f), retExpr.transform(f))
    }
    
    def replace[T](n: Property[T], m: Property[T]): MethodDecl = {
      MethodDecl(retType, id, args, stats.replace(n, m), retExpr.replace(n, m))
    }
    
    def expandProperties(n: String, o: GameObject): MethodDecl = {
      MethodDecl(retType, id, args, stats.expandProperties(n, o), retExpr.expandProperties(n, o))
    }
  }
  
  case class Formal(tpe: Type, id: Val) extends Tree with NoBinding {
    def expandProperties(n: String, o: GameObject): Tree = this
    def replace[T](n: Property[T], m: Property[T]): Formal = this
    def transform(f: PartialFunction[Expr, Expr]): Formal = this
  }
  
  object Stat {
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
        case (p @ ParExpr(a)) :: q => recursiveFlattenStat(p) :: recursiveFlattenBlock(q)
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
    
    /** Recursive function to transform the entire tree by applying the 
     *  given function to all its expressions nodes.
     *  The function is first applied to the children and then to the current node.
     */
    def transform(f: PartialFunction[Expr, Expr]): Stat = this match {
      case ParExpr(stats) =>
        ParExpr(stats map (_.transform(f)))
        
      case Foreach1(cat, name, r) =>
        Foreach1(cat, name, r.transform(f))
        
      case Block(stats) => 
        Block(stats map (_.transform(f)))
  
      case If(c, s1, s2) => 
        If(c.transform(f), s1.transform(f), s2 map (_.transform(f)))
  
      case Copy(name, ref, block) =>
        Copy(name, ref, block.transform(f))
        
      case Assign(props, rhs) =>
        Assign(props map (_.transform(f)), rhs.transform(f))
      
      case Reset(prop) =>
        Reset(prop.transform(f))
        
      case Delete(name, ref) => this
      case NOP => this
    }
    
    def replace[T](n: Property[T], m: Property[T]): Stat = transform(Tree.replace0(n, m))
    
    def expandProperties(n: String, obj: GameObject): Stat = this match {
      case ParExpr(stats) =>
        ParExpr(stats map (_.expandProperties(n, obj)))
        
      case Foreach1(cat, name, r) =>
        Foreach1(cat, name, r.expandProperties(n, obj))
        
      case Block(stats) => 
        Block(stats map (_.expandProperties(n, obj)))
  
      case If(c, s1, s2) => 
        If(c.expandProperties(n, obj), s1.expandProperties(n, obj), s2 map (_.expandProperties(n, obj)))
  
      case Copy(name, ref, block) =>
        Copy(name, ref.expandProperties(n, obj), block.expandProperties(n, obj))
        
      case Assign(props, rhs) =>
        Assign(props map (_.expandProperties(n, obj)), rhs.expandProperties(n, obj))
      
      case Reset(prop) =>
        Reset(prop.expandProperties(n, obj))
        
      case Delete(name, ref) =>
        Delete(name, ref.expandProperties(n, obj))
        
      case NOP => this
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
  
  sealed trait RuleIterator extends Stat {
    
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

  }
  
  case class Foreach1(category: Category, nameBinding: String, protected val rule: Stat) extends RuleIterator {
    protected type Keys = Seq[(String, GameObject)]
    protected def keys = category.objects.map(o => List((nameBinding, o)))
    def generator: BindingGenerator = rule
  }
  
  case class Assign(props: List[Expr], rhs: Expr) extends Stat {
    def children = rhs :: props
  }
  
  // TODO : delete reset.
  case class Reset(prop: Expr) extends Stat {
    def children = List(prop)
  }
  
  object Block {
    def apply(s1: Stat, s: Stat*): Block = {
      Block(List(s1) ++ s.toList)
    }
  }
  
  case class Block(stats: Seq[Stat]) extends Stat {
    def children = stats
    override def ::(other: Stat) = Block(other::stats.toList)
  }
  
  object If {
    def apply(cond: Expr, s1: Stat, s2: Stat): If = If(cond, s1, Some(s2))
  }
  
  case class If(cond: Expr, s1: Stat, s2: Option[Stat]) extends Stat {
    def children = List(cond, s1) ++ s2.toList
  }
  
  case class Copy(name: String, o: GameObjectRef, b: Stat) extends Stat {
    def children = o :: b
  }
  
  case class Delete(name: String, o: GameObjectRef) extends Stat {
    def children = List(o)
  }
  
  case object NOP extends Stat with NoBinding

  case class ParExpr(exprs: List[Stat]) extends Stat {
    def children = exprs
  }
  
  /** Expressions, without side-effect. 
   *  An expression has a type.
   */
  sealed trait Expr extends Tree with Typed {
    
    /** Recursive function to transform the entire tree by applying the 
     *  given function to all its nodes.
     *  The function is first applied to the children and then to the current node.
     */
    def transform(f: PartialFunction[Expr, Expr]): Expr = if (f.isDefinedAt(this)) f(this) else this match {
      case MethodCall(name, args) =>
        MethodCall(name, args map (_.transform(f)))

      case NValue(v, index) => 
        NValue(v.transform(f), index)
        
      case VecExpr(l) => 
        VecExpr(l map (_.transform(f)))
        
      case Choose(VecExpr(l), constraint) =>
        Choose(VecExpr(l map (_.transform(f))), constraint.transform(f))
        
      case IfFunc(cond, ifTrue, ifFalse) => 
        IfFunc(cond.transform(f), ifTrue.transform(f), ifFalse.transform(f))
        
      case _: Count => this
      case _: PropertyIndirect => this
      case _: PropertyRef => this
      case UnitLiteral => this
      case _: ObjectLiteral => this
      case _: IntegerLiteral => this
      case _: FloatLiteral => this
      case _: StringLiteral => this
      case _: BooleanLiteral => this
      case _: Vec2Literal => this
      case _: Val => this
      
      case Plus(lhs, rhs) => 
        Plus(lhs.transform(f), rhs.transform(f))
        
      case Minus(lhs, rhs) => 
        Minus(lhs.transform(f), rhs.transform(f))
        
      case Times(lhs, rhs) => 
        Times(lhs.transform(f), rhs.transform(f))
        
      case Div(lhs, rhs) => 
        Div(lhs.transform(f), rhs.transform(f))
  
      case Mod(lhs, rhs) =>
        Mod(lhs.transform(f), rhs.transform(f))
    
      case And(lhs, rhs) =>
        And(lhs.transform(f), rhs.transform(f))
        
      case Or(lhs, rhs) =>
        Or(lhs.transform(f), rhs.transform(f))
        
      case Equals(lhs, rhs) =>
        Equals(lhs.transform(f), rhs.transform(f))
        
      case LessThan(lhs, rhs) =>
        LessThan(lhs.transform(f), rhs.transform(f))
  
      case LessEq(lhs, rhs) =>
        LessEq(lhs.transform(f), rhs.transform(f))
  
      case GreaterThan(lhs, rhs) =>
        GreaterThan(lhs.transform(f), rhs.transform(f))
  
      case GreaterEq(lhs, rhs) =>
        GreaterEq(lhs.transform(f), rhs.transform(f))
        
      case Not(e) =>
        Not(e.transform(f))
  
      case FingerMoveOver(o) =>
        FingerMoveOver(o.transform(f))
        
      case FingerDownOver(o) =>
        FingerDownOver(o.transform(f))
        
      case FingerUpOver(o) =>
        FingerUpOver(o.transform(f))
      
      case FingerCoordX1 => this
      case FingerCoordX2 => this
      case FingerCoordY1 => this
      case FingerCoordY2 => this
      
      case Collision(lhs, rhs) =>
        Collision(lhs.transform(f), rhs.transform(f))
        
      case Contains(lhs, rhs) =>
        Contains(lhs.transform(f), rhs.transform(f))
      
      case _: GameObjectRef => this
      
      case On(cond) => 
        On(cond.transform(f))
        
      case Once(cond) =>
        Once(cond.transform(f))
      
      case null =>
        this
    }
    
    def replace[T](n: Property[T], m: Property[T]): Expr = transform(Tree.replace0(n, m))
    
    /** Replace all `PropertyRef` by their corresponding `PropertyIndirect` */
    def structuralize(): Expr = transform { 
      case PropertyRef(p) => PropertyIndirect(p.parent.expr, p.name)
    }
    
    def expandProperties(n: String, o: GameObject): Expr = transform {
      case oRef: GameObjectRef => 
        oRef.expandProperties(n, o)
        
      case indirect @ PropertyIndirect(indirectObject, prop) =>
        if(n == indirect.name && o != indirectObject.obj) {
          indirectObject.obj = o
          o.getStructurally(prop)
        } else {
          indirect
        }
    }
    
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
    def update(property: String, arg: Expr): Stat = Assign(List(this(property)), arg)
    
    def x = NValue(this, 0)
    def y = NValue(this, 1)

    def isProperty: Boolean = getProperty.isDefined
    
    def getProperty: Option[Property[_]] = this match {
      case PropertyRef(property) => Some(property)
      case e: PropertyIndirect => e.expr match {
        case PropertyRef(property) => Some(property)
        case _ => None
      }
      case _ => None
    }
    
    def :=(expr: Expr): Stat = {
      this match {
        case p @ VecExpr(l) => Assign(l collect { case m: MaybeAssignable => m }, expr)
        case p: MaybeAssignable => p := expr
        case Plus(p: MaybeAssignable, part)  => p := expr - part
        case Minus(p: MaybeAssignable, part) => p := expr + part
        case Times(p: MaybeAssignable, part) => p := expr / part
        case Div(p: MaybeAssignable, part)   => p := expr * part
        case _ => throw new Exception(s"$this is not assignable $expr")
      }
    }
    
    def reset(): Stat = {
      this match {
        case p:MaybeAssignable => Reset(p)
        case _ => throw new Exception(s"$this is not resetable")
      }
    }
    
  }
  
  case class IfFunc(cond: Expr, s1: Expr, s2: Expr) extends Expr {
    def children = cond :: s1 :: s2
  }
  
  sealed trait ListBinding[T <: Expr] { self: Expr =>
    val l: List[Expr]
    def children = l
  }
  
  case class NValue(o: Expr, index: Int) extends Expr with OBinding
  case class Count(category: Category) extends Expr with NoBinding
  val NULL = ObjectLiteral(null)
  case class ObjectLiteral(value: GameObject) extends Expr with NoBinding
  case class IntegerLiteral(value: Int) extends Expr with NoBinding
  case class FloatLiteral(value: Float) extends Expr with NoBinding
  case class StringLiteral(value: String) extends Expr with NoBinding
  case class BooleanLiteral(value: Boolean) extends Expr with NoBinding
  
  object VecExpr {
    def apply(s: Expr*): VecExpr = VecExpr(s.toList)
  }
  object VecExpr2 {
    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case VecExpr(List(s1: Expr, s2: Expr)) => Some((s1, s2))
      case _ => None
    }
  }
  case class VecExpr(l: List[Expr]) extends Expr with ListBinding[VecExpr]
  case class Vec2Literal(lhs: Float, rhs: Float) extends Expr with NoBinding
  case object UnitLiteral extends Expr with NoBinding
  case class Val(name: String) extends Expr with NoBinding
  case class MethodCall(name: String, l: List[Expr]) extends Expr with ListBinding[MethodCall]

  sealed trait LeftRightBinding { self: Expr =>
    val lhs: Expr
    val rhs: Expr
    def children = lhs :: rhs
  }
  
  case class Plus(lhs: Expr, rhs: Expr) extends Expr with LeftRightBinding
  case class Minus(lhs: Expr, rhs: Expr) extends Expr with LeftRightBinding
  case class Times(lhs: Expr, rhs: Expr) extends Expr with LeftRightBinding
  case class Div(lhs: Expr, rhs: Expr) extends Expr with LeftRightBinding
  case class Mod(lhs: Expr, rhs: Expr) extends Expr with LeftRightBinding

  case class And(lhs: Expr, rhs: Expr) extends Expr with LeftRightBinding
  case class Or(lhs: Expr, rhs: Expr) extends Expr with LeftRightBinding
  case class Equals(lhs: Expr, rhs: Expr) extends Expr with LeftRightBinding
  case class LessThan(lhs: Expr, rhs: Expr) extends Expr with LeftRightBinding
  case class LessEq(lhs: Expr, rhs: Expr) extends Expr with LeftRightBinding
  case class GreaterThan(lhs: Expr, rhs: Expr) extends Expr with LeftRightBinding
  case class GreaterEq(lhs: Expr, rhs: Expr) extends Expr with LeftRightBinding
  case class Not(o: Expr) extends Expr with OBinding
  case class On(o: Expr) extends Expr with OBinding
  case class Once(o: Expr) extends Expr with OBinding

  object GameObjectRef {
    def apply(o: GameObject): GameObjectRef = GameObjectRef(ObjectLiteral(o))
  }
  
  /**
   * Soft reference to a game object, or a game object if ref is null
   */
  case class GameObjectRef(reference: Expr) extends Expr {
    var ref: String = reference match {case StringLiteral(s) => s case _ => null}
    var obj: GameObject = reference match {case ObjectLiteral(o) => o case _ => null}
    
    override def equals(other: Any) = other match { 
      case g: GameObjectRef => 
        reference == g.reference || 
        ref == g.ref || 
        (g.ref == null && ref == null && obj == g.obj) 
      case _ => false
    }
    
    override def expandProperties(n: String, obj: GameObject): GameObjectRef = {setBinding(n, obj)}
    
    // TODO check why removing these two lines raise a runtime error.
    override def apply(property: String): Expr = if(ref == null && obj != null) obj(property) else PropertyIndirect(this, property)
    override def update(property: String, arg: Expr): Stat = apply(property) := arg
    
    def children = Nil
    def name = if (obj != null) obj.name.get else "null"
        
    def copy(name: String)(blocks: Seq[Stat]) = Copy(name, this, Block(blocks))
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
  
  case class FingerMoveOver(o: Expr) extends Expr with OBinding
  case class FingerDownOver(o: Expr) extends Expr with OBinding
  case class FingerUpOver(o: Expr) extends Expr with OBinding
  
  case object FingerCoordX1 extends Expr with NoBinding
  case object FingerCoordY1 extends Expr with NoBinding
  case object FingerCoordX2 extends Expr with NoBinding
  case object FingerCoordY2 extends Expr with NoBinding
  
  case class Collision(lhs: Expr, rhs: Expr) extends Expr with LeftRightBinding
  
  sealed trait MaybeAssignable extends Expr { self: Expr =>
    override def :=(expr: Expr): Stat = Assign(List(this), expr)
    def +=(expr: Expr): Stat = Assign(List(this), Plus(this, expr))
    def -=(expr: Expr): Stat = Assign(List(this), Minus(this, expr))
    def *=(expr: Expr): Stat = Assign(List(this), Times(this, expr))
    def /=(expr: Expr): Stat = Assign(List(this), Div(this, expr))
  }
  
  /** Reference to a hard-coded property. */
  case class PropertyRef(property: Property[_]) extends Expr with NoBinding with MaybeAssignable {
    override def replace[T](n: Property[T], m: Property[T]): Expr = if(n == property) m.expr else this
  }
  
  /** Reference to a property of a dynamically linked object.
   *  name: Name of the object
   */
  case class PropertyIndirect(indirectObject: GameObjectRef, prop: String) extends Expr with MaybeAssignable {
    def obj = indirectObject.obj
    def name = indirectObject.ref
    var expr: Expr = if(obj != null) obj(prop) else null

    def children = List(indirectObject)
    
    override def replace[T](n: Property[T], m: Property[T]): Expr = expr match {
      case null => this
      case p: MaybeAssignable if p.getProperty == Some(n) => 
        m.expr
      case _ => 
        this     
    }

    override def equals(other: Any): Boolean = {
      other match {
        case p: PropertyIndirect =>
          name == p.name && prop == p.prop
        case _ => false
      }
    }
  }
  
  /**
   * Choose construct to choose some assignments given the constraint.
   */
  case class Choose(prop: VecExpr, constraint: Expr) extends Expr {
    
    prop.l foreach { _ match {
      case p: MaybeAssignable => 
      case _ => throw new Exception(s"$prop is not a property but it should")
    }}
    
    def children = if(evaluatedProgram == null) List(prop, constraint) else List(prop, constraint, evaluatedProgram)
    
    var evaluatedProgram: Expr = null
    var expandedConstraint: Expr = null
    def getContraintForSolving = if(expandedConstraint == null) constraint else expandedConstraint
   
  }
  
  /**
   * Does the left object contains the right one?
   * Return a boolean after evaluation
   */
  case class Contains(lhs: Expr, rhs: Expr) extends Expr with LeftRightBinding
  
}
