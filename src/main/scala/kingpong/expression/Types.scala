package ch.epfl.lara.synthesis.kingpong.expression

object Types {

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type) = tpe == this
    override def toString = "Int"
  }
  
  case object TFloat extends Type {
    override def isSubTypeOf(tpe: Type) = tpe == this
    override def toString = "Float"
  }
  
  case object TString extends Type {
    override def isSubTypeOf(tpe: Type) = tpe == this
    override def toString = "String"
  }

  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type) = tpe == this
    override def toString = "Boolean"
  }
  
  case object TUnit extends Type {
    override def isSubTypeOf(tpe: Type) = false
    override def toString = "Unit"
  }
  
  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type) = false
    override def toString = "<untyped>"
  }
  
  case object TError extends Type {
    override def isSubTypeOf(tpe: Type) = false
    override def toString = "<error>"
  }

  def anyToType(any: Any): Type = any match {
    case _: Int => TInt
    case _: Float => TFloat
    case _: String => TString
    case _: Boolean => TBoolean
    case _: Unit => TUnit
    case _ => throw InterpreterException(s"The value $any has an unknown type.")
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
  }

}