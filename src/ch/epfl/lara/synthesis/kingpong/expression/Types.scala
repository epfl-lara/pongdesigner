package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.expression.Trees.Expr

object Types {

  sealed abstract class Type {
    def accept(tpe: Type): Boolean
  }

  case object TInt extends Type {
    override def accept(tpe: Type) = tpe == this
    override def toString = "Int"
  }
  
  case object TFloat extends Type {
    override def accept(tpe: Type) = tpe match {
      case TInt | TFloat => true
      case _ => false
    }
    override def toString = "Float"
  }
  
  case object TString extends Type {
    override def accept(tpe: Type) = tpe == this
    override def toString = "String"
  }

  case object TBoolean extends Type {
    override def accept(tpe: Type) = tpe == this
    override def toString = "Boolean"
  }

  case object TObject extends Type {
    override def accept(tpe: Type) = tpe == this
    override def toString = "Object"
  }
  
  case class TTuple(types: Seq[Type]) extends Type {
    
    //TODO !! incorrect
    override def accept(tpe: Type) = tpe == this || {
      (tpe, types) match {
        case (TTuple(List(TFloat | TInt, TFloat | TInt)), List(TFloat | TInt, TFloat | TInt)) => true
        case _ => false
      }
    }
    override def toString = types.mkString("(", ", ", ")")
  }
    
  val TVec2 = TTuple(List(TFloat, TFloat))
  
  case object TAny extends Type {
    override def accept(tpe: Type) = false
    override def toString = "Any"
  }
  
  case object TUnit extends Type {
    override def accept(tpe: Type) = tpe match {
      case TUntyped | TError => false
      case _ => true
    }
    override def toString = "Unit"
  }
  
  case object TUntyped extends Type {
    override def accept(tpe: Type) = false
    override def toString = "<untyped>"
  }
  
  case object TError extends Type {
    override def accept(tpe: Type) = false
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
      case Some(o) if o != tt => scala.sys.error("Resetting type information! Type [" + o + "] is modified to [" + tt)
      case _ => this
    }

    def isTyped : Boolean = (getType != TUntyped)
  }

  trait FixedType extends Typed { self =>
    val fixedType: Type
    override def getType: Type = fixedType
    override def setType(tt2: Type): self.type = this
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