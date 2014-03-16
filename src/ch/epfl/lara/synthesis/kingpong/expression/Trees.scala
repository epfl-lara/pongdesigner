package ch.epfl.lara.synthesis.kingpong.expression

// remove the warning 
import language.existentials
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.rules.Context

object Trees {
  
  sealed trait Terminal {
    self: Expr =>
  }
 
  sealed trait Prioritized { self: Tree =>
    private var _priority = 0f
    def setPriority(p: Float): self.type = { _priority = p ; self }
    def priority = _priority
  }

  sealed trait Tree {
    
    def copiedFrom(o: Tree): this.type = {
      (this, o) match {
        // do not force if already set
        case (t1: Typed, t2: Typed)  if !t1.isTyped =>
          t1.setType(t2.getType)
        case _ =>
      }
      this
    }
    
  }
  
  trait Identifier extends Tree with Typed {
    def name: String
    def freshen: Identifier
  }
  
  
  /**
   * Identifier for game objects in `foreach` statements.
   */
  class ObjectIdentifier private[Trees](val name: String, private val globalId: Int, val id: Int, alwaysShowUniqueID: Boolean = false) 
    extends Identifier { self =>

    override def equals(other: Any): Boolean = {
      if(other == null || !other.isInstanceOf[ObjectIdentifier])
        false
      else
        other.asInstanceOf[ObjectIdentifier].globalId == this.globalId
    }

    override def hashCode: Int = globalId

    override def toString: String = {
      if(alwaysShowUniqueID) {
        name + (if(id > 0) id else "")
      } else {
        name
      }
    }

    def uniqueName : String = name + id
    def toVariable: Variable = Variable(this)
    def freshen: ObjectIdentifier = FreshIdentifier(name, alwaysShowUniqueID).copiedFrom(this)
  }

  object FreshIdentifier {
    def apply(name: String, alwaysShowUniqueID: Boolean = false): ObjectIdentifier = {
      new ObjectIdentifier(name, UniqueCounter.nextGlobal, UniqueCounter.next(name), alwaysShowUniqueID)
    }
    def apply(name: String, forceId: Int): ObjectIdentifier = {
      new ObjectIdentifier(name, UniqueCounter.nextGlobal, forceId, true)
    }
  }
  
  /**
   * Identifier only used to reference directly or not an assignable property.
   */
  class PropertyIdentifier private[Trees](val obj: ObjectIdentifier, val property: String, protected val globalId: Int, val id: Int, protected val alwaysShowUniqueID: Boolean)
    extends Identifier {
    val name = obj.name + "." + property
    def freshen: PropertyIdentifier = FreshPropertyIdentifier(obj, property, alwaysShowUniqueID).copiedFrom(this)
    
    override def toString: String = {
      if(alwaysShowUniqueID) {
        obj.toString + "." + property + (if(id > 0) id else "")
      } else {
        name
      }
    }
    
    override def equals(other: Any): Boolean = {
      if(other == null || !other.isInstanceOf[PropertyIdentifier])
        false
      else
        other.asInstanceOf[PropertyIdentifier].globalId == this.globalId
    }

    override def hashCode: Int = globalId
  }
  
  object FreshPropertyIdentifier {
    def apply(obj: ObjectIdentifier, property: String, alwaysShowUniqueID: Boolean = false): PropertyIdentifier = {
      val name = obj.name + "." + property
      new PropertyIdentifier(obj, property, UniqueCounter.nextGlobal, UniqueCounter.next(name), alwaysShowUniqueID)
    }
    def apply(obj: ObjectIdentifier, property: String, forceId: Int): PropertyIdentifier = {
      new PropertyIdentifier(obj, property, UniqueCounter.nextGlobal, forceId, true)
    }
  }
  
  private object UniqueCounter {
    private var globalId = -1
    private var nameIds = Map[String, Int]().withDefaultValue(-1)

    def next(name: String): Int = {
      nameIds += name -> (1+nameIds(name))
      nameIds(name)
    }
    
    def nextGlobal = {
      globalId += 1
      globalId
    }
  }

  case class MethodDecl(id: Identifier, args: List[Identifier], stats: Stat, retExpr: Expr) extends Tree with FixedType {
    val fixedType = id.getType
    
//    var fastImplementation: List[Value] => Value = null
//    def withFastImplementation(f: List[Value] => Value): this.type = {
//      fastImplementation = f
//      this
//    }
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
//    def transform(f: PartialFunction[Expr, Expr]): Stat = this match {
//      case ParExpr(stats) =>
//        ParExpr(stats map (_.transform(f)))
//        
//      case Foreach1(cat, name, r) =>
//        Foreach1(cat, name, r.transform(f))
//        
//      case Block(stats) => 
//        Block(stats map (_.transform(f)))
//  
//      case If(c, s1, s2) => 
//        If(c.transform(f), s1.transform(f), s2 map (_.transform(f)))
//  
//      case Copy(name, ref, block) =>
//        Copy(name, ref, block.transform(f))
//        
//      case Assign(props, rhs) =>
//        Assign(props map (_.transform(f)), rhs.transform(f))
//      
//      case Reset(prop) =>
//        Reset(prop.transform(f))
//        
//      case Delete(name, ref) => this
//      case NOP => this
//    }
    
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
  
  case class Foreach(category: Category, id: Identifier, body: Stat) extends Stat
  
  case class Assign(props: Seq[PropertyIdentifier], rhs: Expr) extends Stat
  
  // TODO : delete reset.
  case class Reset(id: PropertyIdentifier) extends Stat
  
  object Block {
    def apply(s1: Stat, s: Stat*): Block = {
      Block(List(s1) ++ s.toList)
    }
  }
  
  case class Block(stats: Seq[Stat]) extends Stat {
    override def ::(other: Stat) = Block(other::stats.toList)
  }
  
  object If {
    def apply(cond: Expr, s1: Stat, s2: Stat): If = If(cond, s1, Some(s2))
  }
  
  case class If(cond: Expr, s1: Stat, s2: Option[Stat]) extends Stat
  
  case class Copy(name: String, obj: Expr, b: Stat) extends Stat
  
  case class Delete(ojb: Expr) extends Stat
  
  case object NOP extends Stat

  case class ParExpr(exprs: List[Stat]) extends Stat
  
  /** Expressions, without side-effect. 
   *  An expression has a type.
   */
  sealed trait Expr extends Tree with Typed {
    
  }
  
  /**
   * A variable used to reference either a game object or a property.
   */
  case class Variable(id: Identifier) extends Expr with Terminal {
    override def getType = id.getType
    override def setType(t: Type) = { id.setType(t); this }
  }
   
  case class Tuple(exprs: Seq[Expr]) extends Expr with FixedType {
    val fixedType = TTuple(exprs.map(_.getType))
  }

  object TupleSelect {
    def apply(tuple: Expr, index: Int): Expr = {
      tuple match {
        case Tuple(exprs) => exprs(index-1) // indexes as 1-based
        case _ => new TupleSelect(tuple, index)
      }
    }

    def unapply(e: TupleSelect): Option[(Expr, Int)] = {
      if (e eq null) None else Some((e.tuple, e.index))
    }
  }

  /**
   * The `index` must be 1-indexed ! (So are methods of Scala Tuples)
   */
  class TupleSelect(val tuple: Expr, val index: Int) extends Expr with FixedType {
    assert(index >= 1)
    assert(tuple.getType.isInstanceOf[TTuple], "Applying TupleSelect on a non-tuple tree [%s] of type [%s].".format(tuple, tuple.getType))

    val fixedType: Type = tuple.getType match {
      case TTuple(ts) =>
        assert(index <= ts.size)
        ts(index - 1)
      case _ =>
        TAny
    }

    override def equals(that: Any): Boolean = (that != null) && (that match {
      case t: TupleSelect => t.tuple == tuple && t.index == index
      case _ => false
    })

    override def hashCode: Int = tuple.hashCode + index.hashCode
  }
  
  /**
   * Choose construct to choose some assignments given the constraint.
   */
  case class Choose(vars: List[PropertyIdentifier], constraint: Expr) extends Expr with FixedType {
    
    assert(!vars.isEmpty)
    
    val fixedType = if (vars.size > 1) TTuple(vars.map(_.getType)) else vars.head.getType
    
    var evaluatedProgram: Expr = null
    var expandedConstraint: Expr = null
    def getContraintForSolving = if(expandedConstraint == null) constraint else expandedConstraint
  }
  
  case class MethodCall(name: String, l: List[Expr]) extends Expr
  
  case class IfFunc(cond: Expr, s1: Expr, s2: Expr) extends Expr
  
  case class Count(category: Category) extends Expr with Terminal with FixedType {
    val fixedType = TInt
  }
  
  /* Literals */
  sealed abstract class Literal[T] extends Expr with Terminal {
    val value: T
  }
  
  case class ObjectLiteral(value: GameObject) extends Literal[GameObject] with FixedType {
    val fixedType = TObject
  }
  case class IntegerLiteral(value: Int) extends Literal[Int] with FixedType {
    val fixedType = TInt
  }
  case class FloatLiteral(value: Float) extends Literal[Float] with FixedType {
    val fixedType = TFloat
  }
  case class StringLiteral(value: String) extends Literal[String] with FixedType {
    val fixedType = TString
  }
  case class BooleanLiteral(value: Boolean) extends Literal[Boolean] with FixedType {
    val fixedType = TBoolean
  }
  case object UnitLiteral extends Literal[Unit] with FixedType {
    val fixedType = TUnit
    val value = ()
  }
  
  /* Arithmetics */
  case class Plus(lhs: Expr, rhs: Expr) extends Expr
  case class Minus(lhs: Expr, rhs: Expr) extends Expr
  case class Times(lhs: Expr, rhs: Expr) extends Expr
  case class Div(lhs: Expr, rhs: Expr) extends Expr
  case class Mod(lhs: Expr, rhs: Expr) extends Expr

  case class And(lhs: Expr, rhs: Expr) extends Expr with FixedBooleanType
  case class Or(lhs: Expr, rhs: Expr) extends Expr with FixedBooleanType
  case class Equals(lhs: Expr, rhs: Expr) extends Expr with FixedBooleanType
  case class LessThan(lhs: Expr, rhs: Expr) extends Expr with FixedBooleanType
  case class LessEq(lhs: Expr, rhs: Expr) extends Expr with FixedBooleanType
  case class GreaterThan(lhs: Expr, rhs: Expr) extends Expr with FixedBooleanType
  case class GreaterEq(lhs: Expr, rhs: Expr) extends Expr with FixedBooleanType
  case class Not(o: Expr) extends Expr with FixedBooleanType

//        
//    def copy(name: String)(blocks: Seq[Stat]) = Copy(name, this, Block(blocks))
//    def delete() = this("deleted") := BooleanLiteral(true)
//    def toLeftOfAtMost(other:GameObjectRef) = this("right") <= other("left")
//    def toRightOfAtMost(other:GameObjectRef) = this("left") >= other("right")
//    def toLeftOf(other:GameObjectRef) = this("right") =:= other("left")
//    def toRightOf(other:GameObjectRef) = this("left") =:= other("right")
//    def above(other:GameObjectRef) = this("bottom") <= other("top")
//    def below(other:GameObjectRef) = this("top") >=  other("bottom")
//    def justAbove(other:GameObjectRef) = this("bottom") =:= other("top")
//    def justBelow(other:GameObjectRef) = this("top") =:=  other("bottom")
//    def alignLeft(other: GameObjectRef) = this("left") =:= other("left")
//    def alignRight(other: GameObjectRef) = this("right") =:= other("right")
//    def collides(other: GameObjectRef) = Collision(this, other)
  
  case class FingerMoveOver(o: Expr) extends Expr with FixedBooleanType
  case class FingerDownOver(o: Expr) extends Expr with FixedBooleanType
  case class FingerUpOver(o: Expr) extends Expr with FixedBooleanType
  
  case object FingerCoordX1 extends Expr with Terminal with FixedType {
    val fixedType = TFloat
  }
  case object FingerCoordY1 extends Expr with Terminal with FixedType {
    val fixedType = TFloat
  }
  case object FingerCoordX2 extends Expr with Terminal with FixedType {
    val fixedType = TFloat
  }
  case object FingerCoordY2 extends Expr with Terminal with FixedType {
    val fixedType = TFloat
  }
  
  case class Collision(lhs: Expr, rhs: Expr) extends Expr with FixedBooleanType
  
  /**
   * Does the left object contains the right one?
   * Return a boolean after evaluation.
   */
  case class Contains(lhs: Expr, rhs: Expr) extends Expr with FixedBooleanType
    
}
