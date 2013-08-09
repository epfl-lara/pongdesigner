package ch.epfl.lara.synthesis.kingpong.expression

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

  case object TVec2 extends Type {
    override def accept(tpe: Type) = tpe == this
    override def toString = "Vec2"
  }
  case object TObject extends Type {
    override def accept(tpe: Type) = tpe == this
    override def toString = "Object"
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
 
  trait Typed { self =>

    private var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  trait PongType[T] {
    def getPongType: Type
    def toPongValue(v: Any): Value
    def toScalaValue(v: Value): T
    def clone(v: T): T
  }

}