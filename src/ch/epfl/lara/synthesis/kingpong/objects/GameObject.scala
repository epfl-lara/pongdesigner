package ch.epfl.lara.synthesis.kingpong.objects

import scala.Dynamic
import scala.language.dynamics
import scala.collection.mutable.{ HashMap => MMap }

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

object GameObject {
  final val EphemeralEndings = "([a-zA-Z0-9_]*[^0-9])([0-9]+)$".r
}

abstract class GameObject(init_name: Expr) extends History with Snap { self =>
  def game: Game
  def className: String
  
  private val _properties = MMap.empty[String, Property[_]]
  
  /** All properties. */
  def properties = _properties.values
  
  //TODO performance issue?
  /** Only writable properties of this object. */
  def writableProperties = _properties.values.view.collect {
    case p: RWProperty[_] => p
  }
  
  /** Only historical properties of this object. */
  def historicalProperties = _properties.values.view.collect {
    case p: HistoricalProperty[_] => p
  }
  
  /** Only snappable properties of this object. */
  def snappableProperties = _properties.values.view.collect {
    case p: SnappableProperty[_] => p
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
    exprF = Tuple(Seq(x.expr, y.expr))
  )

  // --------------------------------------------------------------------------
  // Existence
  // --------------------------------------------------------------------------  
  
  private var _creationTime: Long = 0
  def creationTime = _creationTime
  def deletionTime = simpleProperty[Long]("deletion time", -1)
  
  /**
   * Set the creation time. By default, it is set to 0.
   * The creation time should only be set once.
   */
  def setCreationTime(time: Long): self.type = {
    assert(time >= 0)
    _creationTime = time
    self
  }
  
  /** Checks whether this object exists at the given time. */
  def existsAt(time: Long) = _creationTime <= time && (deletionTime.get < 0 || time < deletionTime.get)
  
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
  def snapshot() = snappableProperties.foreach { _.snapshot() }

  /** Revert to the latest snapshot for all properties. */
  def revert() = snappableProperties.foreach { _.revert() }

  // --------------------------------------------------------------------------
  // History functions
  // --------------------------------------------------------------------------

  /** Save the current state to the history. */
  def save(t: Long) = historicalProperties.foreach { _.save(t) }

  /** Restore the state from the specified discrete time. */
  def restore(t: Long) = historicalProperties.foreach { _.restore(t) }

  /** Clear the history of this object. */
  def clear() = historicalProperties.foreach { _.clear() }

  // --------------------------------------------------------------------------
  // Utility functions
  // --------------------------------------------------------------------------  

  /**
   * This method is called just before the physical world advances and after
   * the rules are evaluated. 
   * @param time The time for the current step.
   */
  def preStep(time: Long, ctx: Context): Unit = {
    setExistenceAt(time)
    historicalProperties foreach { p =>
      p.validate() 
      p.flush()
    }
  }
  
  /**
   * This method is called directly after the physical world advances.
   * @param time The time for the current step.
   */
  def postStep(time: Long, ctx: Context): Unit = {
    historicalProperties foreach { p =>
      p.load() 
      p.save(time)
    }
  }
  
  /**
   * Validate the next values of the underlying structure
   *  and replace the current value with it.
   */
  def validate() = historicalProperties.foreach { _.validate() }

  /**
   * Write the properties values to the underlying structure. The common case
   *  is to force these values to the physical world, but it could also do
   *  nothing.
   */
  def flush() = historicalProperties.foreach { _.flush() }

  /**
   * Load the properties values from the underlying structure, typically
   *  the physical world.
   */
  def load() = historicalProperties.foreach { _.load() }

  /**
   * Reset all properties to their initial values.
   */
  def reset(interpreter: Interpreter) = {
    historicalProperties.foreach { _.reset(interpreter) }
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
    (writableProperties zip other.writableProperties).foreach { case (v1, v2) => v1.set(v2.getExpr) }
    self
  }
  
  /** Get a specific property. */
  def getProperty(property: String): Option[Property[_]] = property match {
    case _ if _properties contains property =>
      _properties.get(property)
    
    case _ if _ephemeralProperties contains property =>
      _ephemeralProperties.get(property)
      
//    case GameObject.EphemeralEndings(propertyName, extension) if _properties contains propertyName =>
//      // Create a new ephemeral
//      val res = _properties(propertyName).copyEphemeral(property)
//      _ephemeralProperties(property) = res
//      res.expr
    
    case _ => None
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
    _properties(p.name) = p
    p
  }

  // Property that can be pushed to the physical world
  protected def simplePhysicalProperty[T: PongType](name: String, init: Expr)(f: T => Unit): HistoricalRWProperty[T] = {
    val p = new SimplePhysicalProperty[T](name, init, this) {
      val flusher = f
    }
    _properties(p.name) = p
    p
  }

  // Property that can be both pushed to the physical world and retrieved
  protected def property[T: PongType](name: String, init: Expr)(f: T => Unit)(l: () => T): HistoricalRWProperty[T] = {
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


  def getCopy(name: String): GameObject = {
    makecopy(name)
      .copyPropertiesFrom(this)
      .setCategory(this.category)
  }
  
  protected def makecopy(name: String): GameObject
}

trait Rectangular extends GameObject {
  def width: Property[Float]
  def height: Property[Float]
  
  val bottom = readOnlyProperty (
    name  = "bottom", 
    getF  = y.get + height.get / 2f,
    nextF = y.next + height.next / 2f,
    exprF = y.expr + height.expr / 2f
  )
  
  val top = readOnlyProperty (
    name  = "top", 
    getF  = y.get - height.get / 2f,
    nextF = y.next - height.next / 2f,
    exprF = y.expr - height.expr / 2f
  )
  
  val left = readOnlyProperty (
    name  = "left", 
    getF  = x.get - width.get / 2f,
    nextF = x.next - width.next / 2f,
    exprF = x.expr - width.expr / 2f
  )
  
  val right = readOnlyProperty (
    name  = "right", 
    getF  = x.get + width.get / 2f,
    nextF = x.next + width.next / 2f,
    exprF = x.expr + width.expr / 2f
  )
}

trait Circular extends GameObject {
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

trait Point extends GameObject {
  val bottom = readOnlyProperty (
    name  = "bottom", 
    getF  = center.get.y,
    nextF = center.next.y,
    exprF = TupleSelect(center.expr, 2)
  )
  
  val top = readOnlyProperty (
    name  = "top", 
    getF  = center.get.y,
    nextF = center.next.y,
    exprF = TupleSelect(center.expr, 2)
  )
  
  val left = readOnlyProperty (
    name  = "left", 
    getF  = center.get.x,
    nextF = center.next.x,
    exprF = TupleSelect(center.expr, 1)
  )
  
  val right = readOnlyProperty (
    name  = "right", 
    getF  = center.get.x,
    nextF = center.next.x,
    exprF = TupleSelect(center.expr, 1)
  )
}

trait Movable extends GameObject {
  def x: RWProperty[Float]
  def y: RWProperty[Float]
}

