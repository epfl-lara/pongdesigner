package ch.epfl.lara.synthesis.kingpong.objects

import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Value

abstract class Property[T : PongType]() extends Timed { self => 
  
  def name: String

  def get: T
  
  /** Write the current value to the underlying structure. The common case
   *  is to force this value to the physical world, but it could also do
   *  nothing.
   */
  def flush(): self.type
  
  /** Load the value from the underlying structure, typically the physical world.
   */
  def load(): self.type

  // TODO history for properties
  // def history: Seq[(T, Long)]
  
  def setInit(e: Expr): self.type
  def set(v: T): self.type
  def reset(implicit interpreter: Interpreter): self.type

  def getPongType: Type = tpe.getPongType
  def setPongValue(v: Value): self.type = set(tpe.toScalaValue(v))
  def getPongValue: Value = tpe.toPongValue(get)

  /** Get the reference of this property. */
  lazy val ref = SinglePropertyRef(this)

  /** The PongType implicit used for conversions. */
  lazy val tpe = implicitly[PongType[T]]
}

abstract class ConcreteProperty[T : PongType](val name: String, init: Expr) extends Property[T] {
  
  protected var _crt: T = _
  protected var _init: Expr = init
  protected var _snap: T = _
  
  def get = _crt

  //TODO look at this
  def snapshot() = _snap = _crt
  def revert() = _crt = _snap
  
  def setInit(e: Expr) = {
    _init = e
    this
  }

  def set(v: T) = {
    _crt = v
    this
  }

  

  def reset(implicit interpreter: Interpreter) = {
    set(interpreter.eval(_init).as[T])
  } 
}

abstract class PhysicalProperty[T : PongType](name: String, init: Expr) 
  extends ConcreteProperty[T](name, init) {
  
  def flush() = {
    flusher(get)
    this
  }
  
  def load() = {
    set(loader())
  }

  val flusher: T => Unit
  val loader: () => T
}

abstract class SimplePhysicalProperty[T : PongType](name: String, init: Expr) 
  extends ConcreteProperty[T](name, init) {
  
  def flush() = {
    flusher(get)
    this
  }
  
  def load() = this

  val flusher: T => Unit
}

class SimpleProperty[T : PongType](name: String, init: Expr) extends ConcreteProperty[T](name, init) {
  def flush() = this
  def load() = this
}