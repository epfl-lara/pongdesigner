package ch.epfl.lara.synthesis.kingpong.objects

import android.util.Log
import ch.epfl.lara.synthesis.kingpong.common._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.menus.MenuOptions

abstract class Property[@specialized T : PongType]() { self => 
  
  def parent: GameObject
  def name: String
  lazy val fullname = parent.name.get + name

  /** Current value. */
  def get: T

  /** Get the current value converted into an expression. */
  def getExpr: Expr = tpe.toExpr(get)

  /** Next value after the end of this time slot. */
  def next: T  
  
  /** Depending on the argument, get the prev or the next */
  def getPrevNext(implicit modify_policy: MenuOptions.Policy) = {
    if(modify_policy.modifiesCurrent) get else next
  }

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

/** All properties that are only readable. */
sealed trait ROProperty[T] extends Property[T]

/**
 * Property that references other ones. 
 * The field `expr` should only reference other properties, constants and arithmetic operations.
 */
abstract class AliasProperty[T : PongType](val name: String, val parent: GameObject) extends ROProperty[T]

/**
 * Constant property. The current and next values are always the same.
 * The expression of this property is just the value translated to an expression.
 */
class ConstProperty[T : PongType](val name: String, val parent: GameObject, value: T) extends ROProperty[T] {
  def get = value
  def next = value

  // We need a Select here since template could need an indirect reference to this property.
  val expr = Select(parent.expr, name).setType(getPongType)
}

/**
 * Read-only property that have a dedicated value (as opposed to `AliasProperty`).
 */
abstract class NamedProperty[T : PongType](val name: String, val parent: GameObject) extends ROProperty[T] {
  /** Get the reference of this property. */
  lazy val expr = Select(parent.expr, name).setType(getPongType)
}

abstract class RWProperty[T : PongType]() extends Property[T] with AssignableProperty[T] { self => 
  
  /** Get the reference of this property. */
  lazy val expr = Select(parent.expr, name).setType(getPongType)
  
  /** Set the current value to `v`. 
   *  The next value is also set.
   */
  def set(v: T): self.type
  def set(e: Expr): self.type = set(tpe.toScalaValue(e))

  /** Set the next value to `v`.
    *  Call `validate()` to push this value to the current state.
    */
  def setNext(v: T): self.type
  def setNext(e: Expr): self.type = setNext(tpe.toScalaValue(e))
  
  /** Depending on the argument, set the prev or the next */
  def setPrevNext(v: T)(implicit modify_policy: MenuOptions.Policy): self.type = {
    if(modify_policy.undoableModification) UndoRedo.recordSetPrevNext(this, v)
    if(modify_policy.modifiesNext) setNext(v)
    if(modify_policy.modifiesCurrent) set(v)
    self
  }
}

trait AssignableProperty[T] { self: RWProperty[T] =>
    
  /** Assign the given value to this property. 
   *  According to the implementation, this will modify the current 
   *  or next value.
   */
  def assign(v: T): self.type
  def assign(e: Expr): self.type = assign(tpe.toScalaValue(e))
}

trait HistoricalProperty[T] extends RWProperty[T] with History { self =>

  /** Validate the next value and replace the current value with it. 
   */
  def validate(): self.type

  /** Set the initial value as an expression.
   *  Call `reset()` to push this value to the current state.
   */
  def setInit(e: Expr): self.type
  
  /** Get the initial expression of this property. */
  def init: Expr

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
  
  def assign(v: T): self.type = setNext(v)
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
  parent.addEphemeralProperty(this)
  private var value: T = _
  def get: T = value
  def next: T = value
  def setNext(v: T) = {value = v; this} // Not a good idea to call it.
  def set(v: T) = { 
    value = v
    self 
  }
  def assign(v: T): self.type = set(v)
}

/**
 * Property with some history
 */
abstract class HistoricalRWProperty[T : PongType](val name: String, private var _init: Expr, val parent: GameObject) 
  extends RWProperty[T] 
  with HistoricalProperty[T] with SnappableProperty[T] { self =>
  
  override def toString = s"ConcreteProperty($name)"
  
  protected var _crt: T = _
  protected var _next: T = _  

  /** Contains the history. The head corresponds to the most recent value. */
  protected val _history: RingBuffer[(Long, T)] = new RingBuffer(History.MAX_HISTORY_SIZE)

  def get = _crt
  def next = _next
  def init = _init
  
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
  
  def setNext(v: T): self.type = {
    _next = v
    this
  }

  def reset(interpreter: Interpreter) = {
    val e = interpreter.evaluate(_init)
    set(tpe.toScalaValue(e))
  }

  def save(t: Int): Unit = {
    if (_history.isEmpty || _history.last._2 != _crt) {
      _history += ((t, tpe.clone(_crt)))
    }
  }

  def restore(t: Int): Unit = {
    _history.lastIndexWhere(_._1 <= t) match {
      case idx if idx >= 0 => 
        set(_history(idx)._2)
      case _ => 
        Log.w("kingpong", s"The timestamp $t (or older) doesn't exist in the history.")
    }
  }

  def clear(from: Int): Unit = {
    if (from <= 0) {
      _history.clear()
    } else {
      val idx = _history.lastIndexWhere(_._1 < from)
      _history.removeFrom(idx + 1)
    }
  }
  
}

abstract class PhysicalProperty[T : PongType](name: String, initial: Expr, parent: GameObject) 
  extends HistoricalRWProperty[T](name, initial, parent) {
  
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

abstract class SimplePhysicalProperty[T : PongType](name: String, initial: Expr, parent: GameObject) 
  extends HistoricalRWProperty[T](name, initial, parent) {
  
  override def toString = s"SimplePhysicalProperty($name)"
  
  private var _lastFlushed: T = _

  def flush() = {
    if (_lastFlushed == null || _lastFlushed != _crt) {
      _lastFlushed = _crt
      flusher(_crt)
    }
    this
  }

  override def reset(interpreter: Interpreter) = {
    _lastFlushed = null.asInstanceOf[T]
    super.reset(interpreter)
  }
  
  @inline def load() = this

  val flusher: T => Unit
}

class SimpleProperty[T : PongType](name: String, initial: Expr, parent: GameObject) 
  extends HistoricalRWProperty[T](name, initial, parent) {
  
  override def toString = s"SimpleProperty($name)"
  @inline def flush() = this
  @inline def load() = this
}