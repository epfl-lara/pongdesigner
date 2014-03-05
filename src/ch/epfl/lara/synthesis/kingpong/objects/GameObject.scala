package ch.epfl.lara.synthesis.kingpong.objects

import scala.collection.mutable.{ HashMap => MMap }
import ch.epfl.lara.synthesis.kingpong.common.History
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Snap
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.rules.Context
import scala.Dynamic
import scala.language.dynamics
import ch.epfl.lara.synthesis.kingpong.Game

object GameObject {
  final val EphemeralEndings = "([a-zA-Z0-9_]*[^0-9])([0-9]+)$".r
}

abstract class GameObject(init_name: Expr) extends History with Snap { self =>
  def game: Game
  def className: String
  
  private val _properties = MMap.empty[String, Property[_]]
  
  /** All properties. */
  def properties = _properties.values
  /** Get a specific property. */
  def getProperty(name: String) = _properties.get(name)
  
  //TODO performance issue?
  /** Only writable properties of this object. */
  def writableProperties = _properties.values.view.collect {
    case p: RWProperty[_] => p
  }
  
  private val _ephemeralProperties = MMap.empty[String, EphemeralProperty[_]]
  
  /** All ephemeral properties. */
  def ephemeralProperties = _ephemeralProperties.values
  /** Get a specific ephemeral property. */
  def getEphemeralProperty(name: String) = _ephemeralProperties.get(name)

  // --------------------------------------------------------------------------
  // Properties
  // --------------------------------------------------------------------------

  val name = simpleProperty[String]("name", init_name)
  def x: Property[Float]
  def y: Property[Float]
  def bottom: Property[Float]
  def top: Property[Float]
  def left: Property[Float]
  def right: Property[Float]
  def angle: Property[Float]
  def visible: Property[Boolean]
  def color: Property[Int]
    
  val center = readOnlyProperty[Vec2] (
    name  = "center", 
    getF  = Vec2(x.get, y.get),
    nextF = Vec2(x.next, y.next),
    exprF = VecExpr(x.expr, y.expr)
  )
  
  private var attachedToCategory = true
  def setExistenceAt(time: Long): Boolean = {
    val exists = existsAt(time)
    if (!exists && attachedToCategory) {
      category.remove(this)
      attachedToCategory = false
    } else if (exists && !attachedToCategory) {
      category.add(this)
      attachedToCategory = true
    }
    exists
  }
  def existsAt(time: Long) = creation_time.get <= time.toInt && time.toInt < deletion_time.get
  def doesNotYetExist(time: Long) = time.toInt < creation_time.get
  val creation_time = simpleProperty[Int]("creation_time", -1)
  val deletion_time = simpleProperty[Int]("deletion_time", Int.MaxValue)

  // --------------------------------------------------------------------------
  // Category
  // --------------------------------------------------------------------------

  private[this] var mCategory: CategoryObject = DefaultCategory(this)
  def category: CategoryObject = mCategory
  def category_=(c: CategoryObject): Unit = mCategory = c

  def setCategory(c: CategoryObject): self.type = {
    c add this
    self
  }

  // --------------------------------------------------------------------------
  // Snapshot functions
  // --------------------------------------------------------------------------

  /** Do a snapshot on all properties. */
  def snapshot() = writableProperties.foreach { _.snapshot() }

  /** Revert to the latest snapshot for all properties. */
  def revert() = writableProperties.foreach { _.revert() }

  // --------------------------------------------------------------------------
  // History functions
  // --------------------------------------------------------------------------

  /** Save the current state to the history. */
  def save(t: Long) = writableProperties.foreach { _.save(t) }

  /** Restore the state from the specified discrete time. */
  def restore(t: Long) = writableProperties.foreach { _.restore(t) }

  /** Clear the history of this object. */
  def clear() = writableProperties.foreach { _.clear() }

  // --------------------------------------------------------------------------
  // Utility functions
  // --------------------------------------------------------------------------  

  /**
   * Validate the next values of the underlying structure
   *  and replace the current value with it.
   */
  def validate() = writableProperties.foreach { _.validate() }

  /**
   * Write the properties values to the underlying structure. The common case
   *  is to force these values to the physical world, but it could also do
   *  nothing.
   */
  def flush() = writableProperties.foreach { _.flush() }

  /**
   * Load the properties values from the underlying structure, typically
   *  the physical world.
   */
  def load() = writableProperties.foreach { _.load() }

  /**
   * Reset all properties to their initial values.
   */
  def reset(interpreter: Interpreter)(implicit context: Context) = {
    writableProperties.foreach { _.reset(interpreter) }
  }
  
  /**
   * Get the corresponding expression for this object.
   */
  def expr = GameObjectRef(ObjectLiteral(this))

  /**
   * //MIKAEL add comment
   */
  def copyPropertiesFrom(other: GameObject, interpreter: Interpreter)(implicit context: Context) = {
    //MIKAEL check if this is always correct
    (writableProperties zip other.writableProperties).foreach { case (v1, v2) => v1.set(v2.getPongValue) }
  }
  
  def get(property: String): Expr = property match {
    case _ if _properties contains property =>
      _properties(property).expr
    
    case _ if _ephemeralProperties contains property =>
      _ephemeralProperties(property).expr
      
    case GameObject.EphemeralEndings(propertyName, extension) if _properties contains propertyName =>
      // Create a new ephemeral
      val res = _properties(propertyName).copyEphemeral(property)
      _ephemeralProperties(property) = res
      res.expr
    
    case _ =>
      throw new Exception(s"$this does not have a $property method")
  }
  
  def apply(property: String): Expr = get(property)

  /**
   * Abstract this object property to turn a call to method bottom to an expression reusable for other shapes.
   */
  //TODO does this can use the `get` method? (and thus lookup also in _ephemeralProperties)
  def getStructurally(property: String): Expr = _properties.get(property) match {
    case Some(p) => p.expr.structuralize()
    case None    => throw new Exception(s"$this does not have a $property method")
  }

  //TODO check that the property exists and is writable ?
  def update(property: String, arg: Expr): Stat = _properties(property).expr := arg

  def getAABB(): AABB

  def contains(pos: Vec2): Boolean

  // --------------------------------------------------------------------------
  // Protected functions
  // --------------------------------------------------------------------------  

  // Property with no relationship with the physical world
  protected def simpleProperty[T: PongType](name: String, init: Expr): RWProperty[T] = {
    val p = new SimpleProperty[T](name, init, this)
    _properties(p.name) = p
    p
  }

  // Property that can be pushed to the physical world
  protected def simplePhysicalProperty[T: PongType](name: String, init: Expr)(f: T => Unit): RWProperty[T] = {
    val p = new SimplePhysicalProperty[T](name, init, this) {
      val flusher = f
    }
    _properties(p.name) = p
    p
  }

  // Property that can be both pushed to the physical world and retrieved
  protected def property[T: PongType](name: String, init: Expr)(f: T => Unit)(l: () => T): RWProperty[T] = {
    val p = new PhysicalProperty[T](name, init, this) {
      val flusher = f
      val loader = l
    }
    _properties(p.name) = p
    p
  }
  
  protected def readOnlyProperty[T: PongType](name: String, getF: => T, nextF: => T, exprF: => Expr): ROProperty[T] = {
    val p = new ROProperty[T](name, this) {
      def get = getF
      def next = nextF
      def expr = exprF
    }
    _properties(p.name) = p
    p
  }
  
  protected def constProperty[T: PongType](name: String, constF: => T): ROProperty[T] = {
    val p = new ROProperty[T](name, this) {
      def get = constF
      def next = constF
      def expr = tpe.toExpr(constF)
    }
    _properties(p.name) = p
    p
  }
  
  protected def proxyProperty[T: PongType](property: Property[T]): ROProperty[T] = {
    val p = new ROProperty[T](property.name, this) {
      def get = property.get
      def next = property.next
      def expr = property.expr
    }
    _properties(p.name) = p
    p
  }


  def getCopy(name: String, interpreter: Interpreter)(implicit context: Context): GameObject = {
    val m = makecopy(name)
    m.copyPropertiesFrom(this, interpreter)
    m.setCategory(this.category)
  }
  protected def makecopy(name: String): GameObject
}

trait Rectangular { self: GameObject =>
  def width: Property[Float]
  def height: Property[Float]
  
  val bottom = readOnlyProperty (
    name  = "bottom", 
    getF  = y.get + height.get / 2,
    nextF = y.next + height.next / 2,
    exprF = y.expr + height.expr / 2
  )
  
  val top = readOnlyProperty (
    name  = "top", 
    getF  = y.get - height.get / 2,
    nextF = y.next - height.next / 2,
    exprF = y.expr - height.expr / 2
  )
  
  val left = readOnlyProperty (
    name  = "left", 
    getF  = x.get - width.get / 2,
    nextF = x.next - width.next / 2,
    exprF = x.expr - width.expr / 2
  )
  
  val right = readOnlyProperty (
    name  = "right", 
    getF  = x.get + width.get / 2,
    nextF = x.next + width.next / 2,
    exprF = x.expr + width.expr / 2
  )
}

trait Circular { self: GameObject =>
  def radius: Property[Float]

  val bottom = readOnlyProperty (
    name  = "bottom", 
    getF  = y.get + radius.get,
    nextF = y.next + radius.next,
    exprF = y.expr + radius.expr
  )
  
  val top = readOnlyProperty (
    name  = "top", 
    getF  = y.get - radius.get,
    nextF = y.next - radius.next,
    exprF = y.expr - radius.expr
  )
  
  val left = readOnlyProperty (
    name  = "left", 
    getF  = x.get - radius.get,
    nextF = x.next - radius.next,
    exprF = x.expr - radius.expr
  )
  
  val right = readOnlyProperty (
    name  = "right", 
    getF  = x.get + radius.get,
    nextF = x.next + radius.next,
    exprF = x.expr + radius.expr
  )
}

trait Point { self: GameObject =>
  val bottom = readOnlyProperty (
    name  = "bottom", 
    getF  = center.get.y,
    nextF = center.next.y,
    exprF = center.expr.y
  )
  
  val top = readOnlyProperty (
    name  = "top", 
    getF  = center.get.y,
    nextF = center.next.y,
    exprF = center.expr.y
  )
  
  val left = readOnlyProperty (
    name  = "left", 
    getF  = center.get.x,
    nextF = center.next.x,
    exprF = center.expr.x
  )
  
  val right = readOnlyProperty (
    name  = "right", 
    getF  = center.get.x,
    nextF = center.next.x,
    exprF = center.expr.x
  )
}
