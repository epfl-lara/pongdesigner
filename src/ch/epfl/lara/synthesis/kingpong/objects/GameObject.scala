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

abstract class GameObject(init_name: Expr) extends WithPoint with History with Snap { self =>
  def game: Game
  def ref = GameObjectRef(ObjectLiteral(this))

  /** Main point for this object. Here set to the position. */
  protected def point = Vec2(x.get, y.get)

  private[this] var mProperties: MMap[String, Property[_]] = MMap.empty
  def properties = mProperties

  // --------------------------------------------------------------------------
  // Properties
  // --------------------------------------------------------------------------

  private[this] var mName: Property[String] = simpleProperty[String]("name", init_name)
  def name: Property[String] = mName

  def className: String
  def x: Property[Float]
  def y: Property[Float]
  def angle: Property[Float]
  def visible: Property[Boolean]
  def color: Property[Int]
  //def layer: Property[Int] // 0 means inside game, 1 means moves with the camera, -1 means its size is canvas-dependent.

  protected var attachedToCategory = true
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
  val creation_time: Property[Int] = simpleProperty[Int]("creation_time", -1)
  val deletion_time: Property[Int] = simpleProperty[Int]("deletion_time", Int.MaxValue)

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
  def snapshot() = properties.values.foreach { _.snapshot() }

  /** Revert to the latest snapshot for all properties. */
  def revert() = properties.values.foreach { _.revert() }

  // --------------------------------------------------------------------------
  // History functions
  // --------------------------------------------------------------------------

  /** Save the current state to the history. */
  def save(t: Long) = properties.values.foreach { _.save(t) }

  /** Restore the state from the specified discrete time. */
  def restore(t: Long) = properties.values.foreach { _.restore(t) }

  /** Clear the history of this object. */
  def clear() = properties.values.foreach { _.clear() }

  // --------------------------------------------------------------------------
  // Utility functions
  // --------------------------------------------------------------------------  

  /**
   * Validate the next values of the underlying structure
   *  and replace the current value with it.
   */
  def validate() = properties.values.foreach { _.validate() }

  /**
   * Write the properties values to the underlying structure. The common case
   *  is to force these values to the physical world, but it could also do
   *  nothing.
   */
  def flush() = properties.values.foreach { _.flush() }

  /**
   * Load the properties values from the underlying structure, typically
   *  the physical world.
   */
  def load() = properties.values.foreach { _.load() }

  /**
   * Reset all properties to their initial values.
   */
  def reset(interpreter: Interpreter)(implicit context: Context) = {
    properties.values.foreach { _.reset(interpreter) }
  }

  /**
   * Reset all properties to their initial values.
   */
  def copyPropertiesFrom(other: GameObject, interpreter: Interpreter)(implicit context: Context) = {
    (properties.values zip other.properties.values).foreach { case (v1, v2) => v1.set(v2.getPongValue) }
  }

  /**
   * Retrieves a property or anything else.
   */
  var ephemeralProperties = MMap[String, EphemeralProperty[_]]()
  def get(property: String) = this(property)
  protected def apply(property: String): Expr = { //PropertyRefTrait
    if (properties contains property) properties(property).ref else {
      property match {
        case "bottom" =>
          this match {
            case r: Rectangular => this("y") + this("height") / 2
            case c: Circle => this("y") + this("radius")
            case _ => throw new Exception(s"$this does not have a $property method")
          }
        case "top" =>
          this match {
            case r: Rectangular => this("y") - this("height") / 2
            case c: Circle => this("y") - this("radius")
            case _ => throw new Exception(s"$this does not have a $property method")
          }
        case "left" =>
          this match {
            case r: Rectangular => this("x") - this("width") / 2
            case c: Circle => this("x") - this("radius")
            case _ => throw new Exception(s"$this does not have a $property method")
          }
        case "right" =>
          this match {
            case r: Rectangular => this("x") + this("width") / 2
            case c: Circle => this("x") + this("radius")
            case _ => throw new Exception(s"$this does not have a $property method")
          }
        case "center" =>
          VecExpr(List(this("x"), this("y")))
        case property =>
          // Maybe a temporary property.
          if (ephemeralProperties contains property) {
            ephemeralProperties(property).ref
          } else {
            property match {
              case GameObject.EphemeralEndings(propertyName, extension) if properties contains propertyName =>
                // Create a new ephemeral
                val res = properties(propertyName).copyEphemeral(property)
                ephemeralProperties(property) = res
                res.ref
              case _ =>
                throw new Exception(s"$this does not have a $property method")
            }
          }
      }
    }
  }

  /**
   * Abstract this object property to turn a call to method bottom to an expression reusable for other shapes.
   */
  def structurally(c: PropertyIndirect, property: String): Expr = {
    if (properties contains property) PropertyIndirect(c.indirectObject, property) else {
      property match {
        case "bottom" =>
          this match {
            case _: Rectangular => PropertyIndirect(c.indirectObject, "y") + PropertyIndirect(c.indirectObject, "height") / 2
            case _: Circle => PropertyIndirect(c.indirectObject, "y") + PropertyIndirect(c.indirectObject, "radius")
            case _ => throw new Exception(s"$this does not have a $property method")
          }
        case "top" =>
          this match {
            case _: Rectangular => PropertyIndirect(c.indirectObject, "y") - PropertyIndirect(c.indirectObject, "height") / 2
            case _: Circle => PropertyIndirect(c.indirectObject, "y") - PropertyIndirect(c.indirectObject, "radius")
            case _ => throw new Exception(s"$this does not have a $property method")
          }
        case "left" =>
          this match {
            case _: Rectangular => PropertyIndirect(c.indirectObject, "x") - PropertyIndirect(c.indirectObject, "width") / 2
            case _: Circle => PropertyIndirect(c.indirectObject, "x") - PropertyIndirect(c.indirectObject, "radius")
            case _ => throw new Exception(s"$this does not have a $property method")
          }
        case "right" =>
          this match {
            case _: Rectangular => PropertyIndirect(c.indirectObject, "x") + PropertyIndirect(c.indirectObject, "width") / 2
            case _: Circle => PropertyIndirect(c.indirectObject, "x") + PropertyIndirect(c.indirectObject, "radius")
            case _ => throw new Exception(s"$this does not have a $property method")
          }
        case "center" =>
          VecExpr(List(PropertyIndirect(c.indirectObject, "x"), PropertyIndirect(c.indirectObject, "y")))
        case _ =>
          throw new Exception(s"$this does not have a $property method")
      }
    }
  }

  //def selectDynamic(methodName: String): PropertyRefTrait = properties(methodName).ref
  //def updateDynamic(property: String)(arg: Expr): Stat = update(property, arg)
  def update(property: String, arg: Expr): Stat = properties(property).ref := arg
  //override def update
  def getAABB(): AABB

  def contains(pos: Vec2): Boolean

  // --------------------------------------------------------------------------
  // Protected functions
  // --------------------------------------------------------------------------  

  // Property with no relationship with the physical world
  protected def simpleProperty[T: PongType](name: String, init: Expr): Property[T] = {
    val p = new SimpleProperty[T](name, init, this)
    properties += (name -> p)
    p
  }

  // Property that can be pushed to the physical world
  protected def simplePhysicalProperty[T: PongType](name: String, init: Expr)(f: T => Unit): Property[T] = {
    val p = new SimplePhysicalProperty[T](name, init, this) {
      val flusher = f
    }
    properties += (name -> p)
    p
  }

  // Property that can be both pushed to the physical world and retrieved
  protected def property[T: PongType](name: String, init: Expr)(f: T => Unit)(l: () => T): Property[T] = {
    val p = new PhysicalProperty[T](name, init, this) {
      val flusher = f
      val loader = l
    }
    properties += (name -> p)
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
  val width: Property[Float]
  val height: Property[Float]
}

