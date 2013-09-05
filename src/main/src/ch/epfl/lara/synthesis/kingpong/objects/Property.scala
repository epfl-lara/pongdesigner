package ch.epfl.lara.synthesis.kingpong.objects

import ch.epfl.lara.synthesis.kingpong.common.History
import ch.epfl.lara.synthesis.kingpong.common.Snap
import ch.epfl.lara.synthesis.kingpong.common.RingBuffer
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Value
import ch.epfl.lara.synthesis.kingpong.rules.Context

abstract class Property[T : PongType](val parent: GameObject) extends History with Snap { self => 
  
  def name: String

  /** Current value. */
  def get: T

  def getPongValue: Value = tpe.toPongValue(get)

  /** Next value after the end of this time slot. */
  def next: T
  
  /** Validate the next value and replace the current value with it. 
   */
  def validate(): self.type

  /** Write the current value to the underlying structure. The common case
   *  is to force this value to the physical world, but it could also do
   *  nothing.
   */
  def flush(): self.type
  
  /** Load the value from the underlying structure, typically the physical world.
   */
  def load(): self.type
  
  /** Set the initial value as an expression.
   *  Call `reset()` to push this value to the current state.
   */
  def setInit(e: Expr): self.type

  /** Set the current value to `v`. 
   *  The next value is also set.
   */
  def set(v: T): self.type
  def set(v: Value): self.type = set(tpe.toScalaValue(v))

  /** Set the next value to `v`.
   *  Call `validate()` to push this value to the current state.
   */
  def setNext(v: T): self.type
  def setNext(v: Value): self.type = setNext(tpe.toScalaValue(v))

  /** Interpret the initial expression and set the current value. */
  def reset(interpreter: Interpreter)(implicit context: Context): self.type

  def getPongType: Type = tpe.getPongType

  /** Get the reference of this property. */
  lazy val ref = PropertyRef(this)

  /** The PongType implicit used for conversions. */
  def tpe = implicitly[PongType[T]]
}

abstract class ConcreteProperty[T : PongType](val name: String, init: Expr, parent: GameObject) extends Property[T](parent) {
  override def toString = s"ConcreteProperty($name)"
  
  protected var _crt: T = _
  protected var _next: T = _  
  protected var _init: Expr = init
  protected var _snap: T = _

  /** Contains the history. The head corresponds to the most recent value. */
  protected val _history: RingBuffer[(Long, T)] = new RingBuffer(History.MAX_HISTORY_SIZE)

  def get = _crt
  def next = _next

  def snapshot() = _snap = _crt
  def revert() = _crt = _snap
  
  def validate() = {
    _crt = _next
    this
  }

  def setInit(e: Expr) = {
    _init = e
    this
  }

  def set(v: T) = {
    _crt = v
    _next = v
    this
  }

  def setNext(v: T) = {
    _next = v
    this
  }

  def reset(interpreter: Interpreter)(implicit context: Context) = {
    set(interpreter.eval(_init).as[T])
  }

  def save(t: Long): Unit = {
    if (_history.isEmpty || _history.last._2 != _crt) {
      _history += (t, tpe.clone(_crt))
    }
  }

  def restore(t: Long): Unit = {
    _history.findLast(_._1 <= t) match {
      case Some((time, value)) => set(value)
      case None => //set(_history.head._2)
        println("Property.scala", s"The timestamp $t doesn't exist in the history.")
    }
  }

  def clear(): Unit = {
    _history.clear()
  }

}

abstract class PhysicalProperty[T : PongType](name: String, init: Expr, parent: GameObject) 
  extends ConcreteProperty[T](name, init, parent) {
  
  override def toString = s"PhysicalProperty($name)"
  
  def flush() = {
    if (loader() != _crt) {
      flusher(_crt)
    }
    this
  }
  
  def load() = {
    set(loader())
    this
  }

  val flusher: T => Unit
  val loader: () => T
}

abstract class SimplePhysicalProperty[T : PongType](name: String, init: Expr, parent: GameObject) 
  extends ConcreteProperty[T](name, init, parent) {
  
  override def toString = s"SimplePhysicalProperty($name)"
  
  private var _lastFlushed: T = _

  def flush() = {
    if (_lastFlushed == null || _lastFlushed != _crt) {
      _lastFlushed = _crt
      flusher(_crt)
    }
    this
  }

  override def reset(interpreter: Interpreter)(implicit context: Context) = {
    _lastFlushed = null.asInstanceOf[T]
    super.reset(interpreter)
  }
  
  def load() = this

  val flusher: T => Unit
}

class SimpleProperty[T : PongType](name: String, init: Expr, parent: GameObject) extends ConcreteProperty[T](name, init, parent) {
  override def toString = s"SimpleProperty($name)"
  def flush() = this
  def load() = this
}

/**
 * Derivated properties such as dxLeft, dxRight, dyTop, dyBottom
 * They are not computed and cannot be resetted, but they can be overriden.
 */
/*class DerivatedProperty[T: PongType](val name: String, original: Property[T], transformation: T => T) extends Property[T] { self =>
  // Members declared in ch.epfl.lara.synthesis.kingpong.common.History
  def clear(): Unit  = {}
  def restore(t: Long): Unit = {}
  def save(t: Long): Unit = {}
  // Members declared in Property
  def flush(): self.type = this
  def get: T = transformation(original.get)
  def load(): self.type = this
  def next: T = transformation(original.next)
  def reset(interpreter: Interpreter)(implicit context: Context):self.type = this
  def set(v: T):self.type = this
  def setInit(e: Expr): self.type = this
  def setNext(v: T): self.type = this
  def validate(): self.type = this
  // Members declared in ch.epfl.lara.synthesis.kingpong.common.Snap
  def revert(): Unit = {}
  def snapshot(): Unit = {}
}*/