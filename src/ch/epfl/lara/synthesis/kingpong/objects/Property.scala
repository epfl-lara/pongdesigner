package ch.epfl.lara.synthesis.kingpong.objects

import ch.epfl.lara.synthesis.kingpong.common.History
import ch.epfl.lara.synthesis.kingpong.common.Snap
import ch.epfl.lara.synthesis.kingpong.common.RingBuffer
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.rules.Context

abstract class Property[T : PongType]() { self => 
  
  def parent: GameObject
  def name: String
  lazy val fullname = parent.name.get + name

  /** Current value. */
  def get: T

  /** Get the current value converted into an expression. */
  def getExpr: Expr = tpe.toExpr(get)

  /** Next value after the end of this time slot. */
  def next: T  

  /** Get the Pong type of this property. */
  def getPongType: Type = tpe.getPongType

  /** Get this property as an Expression. */
  def expr: Expr
  
  /** The PongType implicit used for conversions. */
  def tpe = implicitly[PongType[T]]
  
  /** Get an equivalent ephemeral copy. */
  def copyEphemeral(new_name: String): EphemeralProperty[T] = {
    new EphemeralProperty[T](new_name, parent)
  }
}

abstract class ROProperty[T : PongType](val name: String, val parent: GameObject) extends Property[T]

abstract class RWProperty[T : PongType]() extends Property[T] with AssignableProperty[T] { self => 
  
  /** Get the reference of this property. */
  lazy val expr = Variable(identifier)
  
  /** Set the current value to `v`. 
   *  The next value is also set.
   */
  def set(v: T): self.type
  def set(e: Expr): self.type = set(tpe.toScalaValue(e))  
}

trait AssignableProperty[T] { self: RWProperty[T] =>
  
  lazy val identifier = PropertyIdentifier(parent.identifier, name).setType(tpe.getPongType)
  
  /** Assign the given value to this property. 
   *  According to the implementation, this will modify the current 
   *  or next value.
   */
  def assign(v: T): self.type
  def assign(e: Expr): self.type = assign(tpe.toScalaValue(e))
}

trait HistoricalProperty[T] extends History { self: RWProperty[T]  =>
  
  private var _setNext: T => self.type = setNextInternal _
  
  /** Validate the next value and replace the current value with it. 
   */
  def validate(): self.type

  /** Set the initial value as an expression.
   *  Call `reset()` to push this value to the current state.
   */
  def setInit(e: Expr): self.type
  
  /** Set the next value to `v`.
   *  Call `validate()` to push this value to the current state.
   */
  def setNext(v: T): self.type = _setNext(v)
  def setNext(e: Expr): self.type = setNext(tpe.toScalaValue(e))
  
  protected def setNextInternal(v: T): self.type
  
  /** Write the current value to the underlying structure. The common case
   *  is to force this value to the physical world, but it could also do
   *  nothing.
   */
  def flush(): self.type
  
  /** Load the value from the underlying structure, typically the physical world.
   */
  def load(): self.type
  
  /** Interpret the initial expression and set the current value.
   */
  def reset(interpreter: Interpreter): self.type
  
  /** If activated, a call to `setNext` will set the current value instead
   *  of the next value. 
   */
  def setInstant(activate: Boolean) = {
    if (activate) 
      _setNext = set _
    else 
      _setNext = setNextInternal _
  }
  
  def assign(v: T) = setNext(v)
}

trait SnappableProperty[T] extends Snap { self: RWProperty[T] =>
  private var _snap: T = _
  def snapshot() = _snap = get
  def revert() = set(_snap)
}

/**
 * Property which will not be persistent and is only for one-turn computation.
 * Its value is disregarded afterwards
 */
class EphemeralProperty[T: PongType](val name: String, val parent: GameObject) 
  extends RWProperty[T]
  with SnappableProperty[T] { self =>
    
  private var value: T = _
  def get: T = value
  def next: T = value
  def set(v: T) = { 
    value = v
    this 
  }
  def assign(v: T) = set(v)
}

/**
 * Property with some history
 */
abstract class HistoricalRWProperty[T : PongType](val name: String, init: Expr, val parent: GameObject) 
  extends RWProperty[T] 
  with HistoricalProperty[T] with SnappableProperty[T] { self =>
  
  override def toString = s"ConcreteProperty($name)"
  
  protected var _crt: T = _
  protected var _next: T = _  
  protected var _init: Expr = init

  /** Contains the history. The head corresponds to the most recent value. */
  protected val _history: RingBuffer[(Long, T)] = new RingBuffer(History.MAX_HISTORY_SIZE)

  def get = _crt
  def next = _next
  
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
  
  protected def setNextInternal(v: T): self.type = {
    _next = v
    this
  }

  def reset(interpreter: Interpreter) = {
    val e = interpreter.evaluate(_init)
    set(tpe.toScalaValue(e))
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
  extends HistoricalRWProperty[T](name, init, parent) {
  
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
  extends HistoricalRWProperty[T](name, init, parent) {
  
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

class SimpleProperty[T : PongType](name: String, init: Expr, parent: GameObject) 
  extends HistoricalRWProperty[T](name, init, parent) {
  
  override def toString = s"SimpleProperty($name)"
  def flush() = this
  def load() = this
}