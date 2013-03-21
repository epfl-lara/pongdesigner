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

  /*
  case object TIntArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == this
    override def toString = "int[]"
  }
  
  // It's convenient to reference classes by their symbol, rather than by their name, to avoid doing the lookup every time.
  case class TObject(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case t if t == anyObject => true
      case TObject(cs) =>
        def objectSubType(csToUp: Option[ClassSymbol]): Boolean = csToUp match {
          case Some(parent_cs) =>
            if (parent_cs == cs) true
            else objectSubType(parent_cs.parent)
          case None => false
        }
        objectSubType(Some(classSymbol))
      case _ => false
    }
    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses.
  val anyObject = TObject(new ClassSymbol("Object"))

  */

  trait Typed { self =>

    private var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  trait PongType[T] {
    def getPongType: Type
    def toPongValue(v: Any): Value[T]
    def toScalaValue(v: Value[Any]): T
  }

}