package ch.epfl.lara.synthesis.kingpong.objects

import scala.Dynamic
import scala.language.dynamics
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ HashMap => MMap }

import org.jbox2d.dynamics.BodyType

import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.common.History
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Snap
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.rules.Context
import ch.epfl.lara.synthesis.kingpong.Options.Event._

object GameObject {
  final val EphemeralEndings = "([a-zA-Z0-9_]*[^0-9])([0-9]+)$".r
}

abstract class GameObject(init_name: Expr) extends History with Snap { self =>
  def game: Game
  def className: String
  
  /** All properties mapped from their name. */
  private val _propertiesMap = MMap.empty[String, Property[_]]
  private val _writableProperties = ArrayBuffer.empty[RWProperty[_]]
  private val _historicalProperties =  ArrayBuffer.empty[HistoricalProperty[_]]
  private val _snappableProperties = ArrayBuffer.empty[SnappableProperty[_]]
  
  /** All properties. */
  def properties: Traversable[Property[_]] = _propertiesMap.values
  
  /** Only writable properties of this object. */
  def writableProperties: Traversable[RWProperty[_]] = _writableProperties
  
  /** Only historical properties of this object. */
  def historicalProperties: Traversable[HistoricalProperty[_]] = _historicalProperties
  
  /** Only snappable properties of this object. */
  def snappableProperties: Traversable[SnappableProperty[_]] = _snappableProperties
  
  private val _ephemeralPropertiesMap = MMap.empty[String, EphemeralProperty[_]]
  private val _ephemeralProperties = ArrayBuffer.empty[EphemeralProperty[_]]
  
  /** All ephemeral properties. */
  def ephemeralProperties: Traversable[EphemeralProperty[_]] = _ephemeralProperties
  /** Get a specific ephemeral property. */
  def getEphemeralProperty(name: String) = _ephemeralPropertiesMap.get(name)

  // --------------------------------------------------------------------------
  // Properties
  // --------------------------------------------------------------------------

  val name = simpleProperty[String]("name", init_name)
  def bottom: Property[Float]
  def top: Property[Float]
  def left: Property[Float]
  def right: Property[Float]
  def visible: Property[Boolean]
  def color: Property[Int]
  var tpe: BodyType = BodyType.STATIC
  def noVelocity = tpe == BodyType.STATIC
  def noVelocity_=(b: Boolean): Unit // TODO It means to change the way the shape is. Do it in subclasses

  // --------------------------------------------------------------------------
  // Existence
  // --------------------------------------------------------------------------  
  
  val creationTime = simpleProperty[Long]("creation time", -1)
  val deletionTime = simpleProperty[Long]("deletion time", Long.MaxValue)
  
  /** Checks whether this object exists at the given time. */
  def existsAt(time: Long) = creationTime.get <= time && (deletionTime.get < 0 || time < deletionTime.get)
  
  /**
   * Update the internal state according to the current time. Particularly, remove the object from the 
   * physical world if it doesn't exist and vice-versa. 
   */ 
  def setExistenceAt(time: Long): Boolean = {
    val exists = existsAt(time)
    if (!exists && _attachedToCategory) {
      category.remove(this)
      _attachedToCategory = false
    } else if (exists && !_attachedToCategory) {
      category.add(this)
      _attachedToCategory = true
    }
    exists
  }
  
  // --------------------------------------------------------------------------
  // Category
  // --------------------------------------------------------------------------

  private var _attachedToCategory = true
  private var _category: CategoryObject = DefaultCategory(this)
  def category = _category

  def setCategory(c: CategoryObject): self.type = {
    _category = c
    _attachedToCategory = true
    c.add(this)
    self
  }

  // --------------------------------------------------------------------------
  // Snapshot functions
  // --------------------------------------------------------------------------

  /** Do a snapshot on all properties. */
  def snapshot() = _snappableProperties.foreach { _.snapshot() }

  /** Revert to the latest snapshot for all properties. */
  def revert() = _snappableProperties.foreach { _.revert() }

  // --------------------------------------------------------------------------
  // History functions
  // --------------------------------------------------------------------------

  /** Save the current state to the history. */
  def save(t: Long) = _historicalProperties.foreach { _.save(t) }

  /** Restore the state from the specified discrete time. */
  def restore(t: Long) = _historicalProperties.foreach { _.restore(t) }

  /** Clear the history of this object. */
  def clear() = _historicalProperties.foreach { _.clear() }

  // --------------------------------------------------------------------------
  // Utility functions
  // --------------------------------------------------------------------------  

  /**
   * This method is called just before the physical world advances and after
   * the rules are evaluated. 
   */
  def preStep(ctx: Context): Unit = {
    setExistenceAt(ctx.time)
    // This is very low level for performance reasons since it is called
    // very often and we don't want allocation of anonymous functions.
    var i = 0
    val top = _historicalProperties.size
    while (i < top) {
      val p = _historicalProperties(i)
      p.validate() 
      p.flush()
      i += 1
    }
  }
  
  /**
   * This method is called directly after the physical world advances.
   */
  def postStep(ctx: Context): Unit = {
    // This is very low level for performance reasons since it is called
    // very often and we don't want allocation of anonymous functions.
    var i = 0
    val top = _historicalProperties.size
    while (i < top) {
      val p = _historicalProperties(i)
      p.load()
      p.save(ctx.time)
      i += 1
    }
  }
  
  /**
   * Validate the next values of the underlying structure
   *  and replace the current value with it.
   */
  def validate() = _historicalProperties.foreach { _.validate() }

  /**
   * Write the properties values to the underlying structure. The common case
   *  is to force these values to the physical world, but it could also do
   *  nothing.
   */
  def flush() = _historicalProperties.foreach { _.flush() }

  /**
   * Load the properties values from the underlying structure, typically
   *  the physical world.
   */
  def load() = _historicalProperties.foreach { _.load() }

  /**
   * Reset all properties to their initial values.
   */
  def reset(interpreter: Interpreter) = {
    _historicalProperties.foreach { _.reset(interpreter) }
  }
  
  /**
   * Get the corresponding expression for this object.
   */
  lazy val expr: Expr = ObjectLiteral(this)

  /**
   * //MIKAEL add comment
   */
  def copyPropertiesFrom(other: GameObject): self.type = {
    //MIKAEL check if this is always correct
    (_writableProperties zip other._writableProperties).foreach { case (v1, v2) => v1.set(v2.getExpr) }
    self
  }
  
  /** Get a specific property. */
  def getProperty(property: String): Option[Property[_]] = {
    _propertiesMap.get(property).orElse(_ephemeralPropertiesMap.get(property))
  }
  
  def get(property: String): Property[_] = getProperty(property) match {
    case Some(p) => p
    case None    => throw new Exception(s"$this does not have a $property property.")
  }
  
  def apply(property: String): Expr = get(property).expr

  def getAABB(): AABB

  def contains(pos: Vec2): Boolean

  // --------------------------------------------------------------------------
  // Protected functions
  // --------------------------------------------------------------------------  

  // Property with no relationship with the physical world
  protected def simpleProperty[T: PongType](name: String, init: Expr): HistoricalRWProperty[T] = {
    val p = new SimpleProperty[T](name, init, this)
    addProperty(p)
    p
  }

  // Property that can be pushed to the physical world
  protected def simplePhysicalProperty[@specialized T: PongType](name: String, init: Expr, f: T => Unit): HistoricalRWProperty[T] = {
    val p = new SimplePhysicalProperty[T](name, init, this) {
      val flusher = f
    }
    addProperty(p)
    p
  }

  // Property that can be both pushed to the physical world and retrieved
  protected def property[@specialized T: PongType](name: String, init: Expr, f: T => Unit, l: () => T): HistoricalRWProperty[T] = {
    val p = new PhysicalProperty[T](name, init, this) {
      val flusher = f
      val loader = l
    }
    addProperty(p)
    p
  }
  
  protected def readOnlyProperty[@specialized T: PongType](name: String, getF: () => T, nextF: () => T, exprF: () => Expr): ROProperty[T] = {
    val p = new ROProperty[T](name, this) {
      def get = getF()
      def next = nextF()
      def expr = exprF()
    }
    addProperty(p)
    p
  }
  
  protected def constProperty[@specialized T: PongType](name: String, constF: () => T): ROProperty[T] = {
    val p = new ROProperty[T](name, this) {
      def get = constF()
      def next = constF()
      def expr = tpe.toExpr(constF())
    }
    addProperty(p)
    p
  }
  
  protected def proxyProperty[T: PongType](property: Property[T]): ROProperty[T] = {
    val p = new ROProperty[T](property.name, this) {
      def get = property.get
      def next = property.next
      def expr = property.expr
    }
    addProperty(p)
    p
  }
  
  private def addProperty(property: Property[_]): Unit = {
    _propertiesMap(property.name) = property
    if (property.isInstanceOf[RWProperty[_]])
      _writableProperties += property.asInstanceOf[RWProperty[_]]
    if (property.isInstanceOf[SnappableProperty[_]])
      _snappableProperties += property.asInstanceOf[SnappableProperty[_]]
    if (property.isInstanceOf[HistoricalProperty[_]])
      _historicalProperties += property.asInstanceOf[HistoricalProperty[_]]
  }

  def getCopy(name: String): GameObject = {
    makecopy(name)
      .copyPropertiesFrom(this)
      .setCategory(this.category)
  }
  
  protected def makecopy(name: String): GameObject
  
  def selectableBy(xCursor: Float, yCursor: Float):Boolean = false
  def distanceSelection(x: Float, y: Float): Float = Float.MaxValue
}

trait Rectangular extends GameObject with Positionable {
  def width: Property[Float]
  def height: Property[Float]
  
  val bottom = readOnlyProperty (
    name  = "bottom", 
    getF  = () => y.get + height.get / 2f,
    nextF = () => y.next + height.next / 2f,
    exprF = () => y.expr + height.expr / 2f
  )
  
  val top = readOnlyProperty (
    name  = "top", 
    getF  = () => y.get - height.get / 2f,
    nextF = () => y.next - height.next / 2f,
    exprF = () => y.expr - height.expr / 2f
  )
  
  val left = readOnlyProperty (
    name  = "left", 
    getF  = () => x.get - width.get / 2f,
    nextF = () => x.next - width.next / 2f,
    exprF = () => x.expr - width.expr / 2f
  )
  
  val right = readOnlyProperty (
    name  = "right", 
    getF  = () => x.get + width.get / 2f,
    nextF = () => x.next + width.next / 2f,
    exprF = () => x.expr + width.expr / 2f
  )
  
  override def selectableBy(xCursor: Float, yCursor: Float):Boolean = {
    xCursor > left.get &&
    xCursor < right.get &&
    yCursor > top.get &&
    yCursor < bottom.get
  }
  
  override def distanceSelection(xCursor: Float, yCursor: Float):Float = {
    val dx = if(xCursor < left.next) {
      left.next - xCursor
    } else if(xCursor > right.next) {
      xCursor - right.next
    } else 0
    val dy = if(yCursor < top.next) {
      top.next - yCursor
    } else if(yCursor > bottom.next) {
      yCursor - bottom.next
    } else 0
    Math.sqrt(dx*dx + dy*dy).toFloat
  }
  
  override def toString = name.get
}

trait ResizableRectangular extends Rectangular {
  def width: RWProperty[Float]
  def height: RWProperty[Float]
}

trait Circular extends GameObject with Positionable {
  def radius: Property[Float]

  val bottom = readOnlyProperty (
    name  = "bottom", 
    getF  = () => y.get + radius.get,
    nextF = () => y.next + radius.next,
    exprF = () => y.expr + radius.expr
  )
  
  val top = readOnlyProperty (
    name  = "top", 
    getF  = () => y.get - radius.get,
    nextF = () => y.next - radius.next,
    exprF = () => y.expr - radius.expr
  )
  
  val left = readOnlyProperty (
    name  = "left", 
    getF  = () => x.get - radius.get,
    nextF = () => x.next - radius.next,
    exprF = () => x.expr - radius.expr
  )
  
  val right = readOnlyProperty (
    name  = "right", 
    getF  = () => x.get + radius.get,
    nextF = () => x.next + radius.next,
    exprF = () => x.expr + radius.expr
  )
  
  override def selectableBy(xCursor: Float, yCursor: Float): Boolean = {
    xCursor > left.next &&
    xCursor < right.next &&
    yCursor > top.next &&
    yCursor < bottom.next && xCursor*xCursor + yCursor*yCursor <= radius.next * radius.next
  }
  
  override def distanceSelection(xCursor: Float, yCursor: Float): Float = {
    val dx = xCursor-x.next
    val dy = yCursor-y.next
    val r = radius.next + selectableAreaRadius
    val res = Math.sqrt(dx*dx + dy*dy).toFloat - radius.next
    if(res < 0) 0 else res
  }
}

trait ResizableCircular extends Circular {
  def radius: RWProperty[Float]
}

trait Point extends GameObject with Positionable {
  val bottom = readOnlyProperty (
    name  = "bottom", 
    getF  = () => center.get.y,
    nextF = () => center.next.y,
    exprF = () => TupleSelect(center.expr, 2)
  )
  
  val top = readOnlyProperty (
    name  = "top", 
    getF  = () => center.get.y,
    nextF = () => center.next.y,
    exprF = () => TupleSelect(center.expr, 2)
  )
  
  val left = readOnlyProperty (
    name  = "left", 
    getF  = () => center.get.x,
    nextF = () => center.next.x,
    exprF = () => TupleSelect(center.expr, 1)
  )
  
  val right = readOnlyProperty (
    name  = "right", 
    getF  = () => center.get.x,
    nextF = () => center.next.x,
    exprF = () => TupleSelect(center.expr, 1)
  )
}

trait Positionable extends GameObject {
  def x: Property[Float]
  def y: Property[Float]

  val center = readOnlyProperty[Vec2] (
    name  = "center", 
    getF  = () => Vec2(x.get, y.get),
    nextF = () => Vec2(x.next, y.next),
    exprF = () => Tuple(Seq(x.expr, y.expr))
  )
  
  override def selectableBy(xCursor: Float, yCursor: Float): Boolean = {
     xCursor*xCursor + yCursor*yCursor <= selectableAreaRadius * selectableAreaRadius
  }
  
  override def distanceSelection(xCursor: Float, yCursor: Float): Float = {
    val dx = xCursor-x.next
    val dy = yCursor-y.next
    val r = selectableAreaRadius
    val res = Math.sqrt(dx*dx + dy*dy).toFloat
    if(res < 0) 0 else res
  }
  
  def bottom: Property[Float]
  def top: Property[Float]
  def left: Property[Float]
  def right: Property[Float]
}

trait Movable extends GameObject with Positionable {
  def x: RWProperty[Float]
  def y: RWProperty[Float]
}

trait Speed extends GameObject {
  def velocity: Property[Vec2]
}

trait SpeedSettable extends GameObject with Speed {
  def velocity: RWProperty[Vec2]
}

trait Directionable extends GameObject {
  def angle: Property[Float]
}

trait Rotationable extends Directionable {
  def angle: RWProperty[Float]
}

trait Colorable extends GameObject {
  def color: RWProperty[Int]
}

trait Visiblable extends GameObject {
  def visible: RWProperty[Boolean]
}