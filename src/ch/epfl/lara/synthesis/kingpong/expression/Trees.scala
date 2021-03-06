package ch.epfl.lara.synthesis.kingpong.expression

// remove the warning 
import language.existentials
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression.TypeOps._
import PrettyPrinterExtendedTypical._

object Trees {
  
  type PropertyId = String
  
  sealed trait Terminal {
    self: Expr =>
  }
 
  sealed trait Prioritized { self: Tree =>
    private var _priority = 0f
    def setPriority(p: Float): self.type = { _priority = p ; self }
    def priority = _priority
  }
  
  sealed trait Commented { self: Tree =>
    private var _comment: StringMaker => StringMaker = null
    def setComment(c: StringMaker => StringMaker): self.type = { _comment = c; self}
    def comment = _comment
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
  
  
  /**
   * Identifier XXX
   */
  class Identifier private[Trees](val name: String, private val globalId: Int, val id: Int, alwaysShowUniqueID: Boolean = false) 
    extends Tree with Typed { self =>

    override def equals(other: Any): Boolean = {
      if(other == null || !other.isInstanceOf[Identifier])
        false
      else
        other.asInstanceOf[Identifier].globalId == this.globalId
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
    def freshen: Identifier = FreshIdentifier(name, alwaysShowUniqueID).copiedFrom(this)
  }

  object FreshIdentifier {
    def apply(name: String, alwaysShowUniqueID: Boolean = false): Identifier = {
      new Identifier(name, UniqueCounter.nextGlobal, UniqueCounter.next(name), alwaysShowUniqueID)
    }
    def apply(name: String, forceId: Int): Identifier = {
      new Identifier(name, UniqueCounter.nextGlobal, forceId, true)
    }
  }
  
  private[kingpong] object UniqueCounter {
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

  case class MethodDecl(id: Identifier, args: List[Identifier], exprs: Expr, retExpr: Expr) extends Tree with FixedType {
    val fixedType = id.getType
    
//    var fastImplementation: List[Value] => Value = null
//    def withFastImplementation(f: List[Value] => Value): this.type = {
//      fastImplementation = f
//      this
//    }
  }
  
  /** Expressions. 
   *  An expression has a type.
   */
  sealed trait Expr extends Tree with Typed with Prioritized with Commented{
    
  }
  
  /** Unit expressions, some of them can have side-effect. */
  sealed trait UnitExpr extends Expr with FixedType {
    val fixedType = TUnit
  }
  
  case class Foreach(category: CategoryObject, id: Identifier, body: Expr) extends UnitExpr
  
  case class Assign(prop: (Expr, PropertyId), rhs: Expr) extends UnitExpr
  
  object Block {
    def apply(e1: Expr, e: Expr*): Block = {
      Block(List(e1) ++ e.toList)
    }
  }
  
  case class Block(exprs: Seq[Expr]) extends UnitExpr
  
  case class Copy(obj: Expr, id: Identifier, body: Expr) extends UnitExpr
  
  case class Delete(obj: Expr) extends UnitExpr
  
  case class ParExpr(exprs: List[Expr]) extends UnitExpr
  
  object If {
    def apply(cond: Expr, e: Expr): If = If(cond, e, UnitLiteral)
  }
  
  case class If(cond: Expr, thenn: Expr, elze: Expr) extends Expr with FixedType {
    val fixedType = leastUpperBound(thenn.getType, elze.getType).getOrElse(TAny)
  }
  
  case class Forall(category: CategoryObject, id: Identifier, body: Expr) extends Expr with FixedBooleanType
  
  case class Find(category: CategoryObject, id: Identifier, body: Expr) extends Expr with FixedType {
    val fixedType = TObject
  }
  
  /** 
   * Debug expression. When evaluated, prints the message.
   * The interpretation of the formatting patterns is described in `java.util.Formatter`.
   */
  case class Debug(message: String, exprs: Seq[Expr]) extends UnitExpr with Terminal
  
  /**
   * A variable XXX
   */
  case class Variable(id: Identifier) extends Expr with Terminal {
    override def getType = id.getType
    override def setType(t: Type) = { id.setType(t); this }
  }
  
  case class Let(id: Identifier, expr: Expr, body: Expr) extends Expr with FixedType {
    val fixedType = body.getType
  }
  
  case class Select(expr: Expr, property: PropertyId) extends Expr
   
  object Tuple {
    def apply(e1: Expr, e2: Expr): Tuple = Tuple(Seq(e1, e2))
    def apply(e1: Expr, e2: Expr, e3: Expr): Tuple = Tuple(Seq(e1, e2, e3))
  }
  
  case class Tuple(exprs: Seq[Expr]) extends Expr with FixedType {
    lazy val fixedType = TTuple(exprs.map(_.getType))
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
    //assert(tuple.getType.isInstanceOf[TTuple], "Applying TupleSelect on a non-tuple tree [%s] of type [%s].".format(tuple, tuple.getType))

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
    
    override def toString = "TupleSelect(" + tuple + ", " + index + ")"
  }
  
  /**
   * Choose construct to choose some assignments given the constraint.
   */
  case class Choose(vars: List[Expr], constraint: Expr, code: Expr) extends Expr with FixedType {
    
    assert(!vars.isEmpty)
    
    val fixedType = if (vars.size > 1) TTuple(vars.map(_.getType)) else vars.head.getType
    
    def evaluatedProgram: Expr = code
    var expandedConstraint: Expr = null
    def getContraintForSolving = if(expandedConstraint == null) constraint else expandedConstraint
  }
  
  case class MethodCall(name: String, l: Seq[Expr]) extends Expr
  
  /* Literals */
  sealed abstract class Literal[T] extends Expr with Terminal {
    def value: T
  }
  
  object ObjectLiteral {
    val empty = ObjectLiteral(null)
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
  case object UnitLiteral extends Literal[Unit] with UnitExpr {
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
  
  case class Collision(lhs: Expr, rhs: Expr) extends Expr with FixedBooleanType
  case class Colliding(lhs: Expr, rhs: Expr) extends Expr with FixedBooleanType
  case class OutOfCollision(lhs: Expr, rhs: Expr) extends Expr with FixedBooleanType
  /* Finger interactions */
  
  case class FingerMoveOver(obj: Expr, id: Identifier, block: Expr) extends Expr with FixedBooleanType
  case class FingerDownOver(obj: Expr, id: Identifier, block: Expr) extends Expr with FixedBooleanType
  case class FingerUpOver(obj: Expr, id: Identifier, block: Expr) extends Expr with FixedBooleanType
  case class IsFingerDownOver(o: Expr) extends Expr with FixedBooleanType
  case class IsFingerUpOver(o: Expr) extends Expr with FixedBooleanType

  case class ApplyForce(obj: Expr, force: Expr) extends UnitExpr

  /* Array operations */
  
  /** Test if the left object contains the center of the right one. */
  case class Contains(lhs: Expr, rhs: Expr) extends Expr with FixedBooleanType
  /** Test if the object on the left contains the right one. */
  case class ContainsTotally(lhs: Expr, rhs: Expr) extends Expr with FixedBooleanType
  
  case class ContainingCell(array: Expr, obj: Expr) extends Expr with FixedType {
    val fixedType = TObject
  }
  
  /** Get the cell at the specified position in the array. */
  case class Apply(obj: Expr, column: Expr, row: Expr) extends Expr with FixedType {
    val fixedType = TObject
  }
}
