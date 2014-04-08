package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.expression.Trees.Expr

object Types {

  sealed abstract class Type

  case object TInt extends Type {
    override def toString = "Int"
  }
  
  case object TFloat extends Type {
    override def toString = "Float"
  }
  
  case object TString extends Type {
    override def toString = "String"
  }

  case object TBoolean extends Type {
    override def toString = "Boolean"
  }

  case object TObject extends Type {
    override def toString = "Object"
  }
  
  object TTuple {
    def apply(t1: Type, t2: Type): TTuple = TTuple(Seq(t1, t2))
    def apply(t1: Type, t2: Type, t3: Type): TTuple = TTuple(Seq(t1, t2, t3))
  }
  
  case class TTuple(types: Seq[Type]) extends Type {
    override def toString = types.mkString("(", ", ", ")")
  }
    
  val TVec2 = TTuple(TFloat, TFloat)
  
  case object TAny extends Type {
    override def toString = "Any"
  }
  
  case object TUnit extends Type {
    override def toString = "Unit"
  }
  
  case object TUntyped extends Type {
    override def toString = "<untyped>"
  }
  
  case object TError extends Type {
    override def toString = "<error>"
  }
 
  trait Typed extends Serializable { self =>

    private var _type: Option[Type] = None

    def getType: Type = _type match {
      case None => TUntyped
      case Some(t) => t
    }

    def setType(tt: Type): self.type = _type match {
      case None => _type = Some(tt); this
      case Some(o) if o != tt => scala.sys.error("Resetting type information! Type [" + o + "] is modified to [" + tt + "]")
      case _ => this
    }

    def isTyped : Boolean = (getType != TUntyped)
  }
  
  class TypeErrorException(msg: String) extends Exception(msg)

  object TypeErrorException {
    def apply(obj: Expr, exp: List[Type]): TypeErrorException = {
      new TypeErrorException("Type error: "+obj+", expected: "+exp.mkString(" or ")+", found "+obj.getType)
    }

    def apply(obj: Expr, exp: Type): TypeErrorException = {
      apply(obj, List(exp))
    }
  }

  trait FixedType extends Typed { self =>
    val fixedType: Type
    override def getType: Type = fixedType
    override def setType(tt: Type): self.type = this
  }
  
  trait FixedBooleanType extends FixedType {
    val fixedType = TBoolean
  }
  
  trait PongType[T] {
    def getPongType: Type
    def toScalaValue(v: Expr): T
    def toExpr(v: T): Expr
    def clone(v: T): T
  }

}