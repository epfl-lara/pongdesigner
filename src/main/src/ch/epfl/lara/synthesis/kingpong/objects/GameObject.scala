package ch.epfl.lara.synthesis.kingpong.objects

import scala.collection.mutable.{Map => MMap}

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

abstract class GameObject(init_name: Expr) extends WithPoint with History with Snap { self =>
  
  /** Main point for this object. Here set to the position. */
  protected def point = Vec2(x.get, y.get)

  private[this] var mProperties: MMap[String, Property[_]] = MMap.empty
  def properties = mProperties
  
  // --------------------------------------------------------------------------
  // Properties
  // --------------------------------------------------------------------------
  
  private[this] var mName: Property[String] = simpleProperty[String]("name", init_name)  
  def name: Property[String] = mName
  
  private[this] var mDeleted: Property[Boolean] = simpleProperty[Boolean]("deleted", false)
  
  def x: Property[Float]
  def y: Property[Float]
  def angle: Property[Float]
  def visible: Property[Boolean]

  // --------------------------------------------------------------------------
  // Category
  // --------------------------------------------------------------------------

  private[this] var mCategory: Category = DefaultCategory(this)
  def category: Category = mCategory
  def category_=(c: Category): Unit = mCategory = c
    
  def setCategory(c: Category): self.type = {
    c add this
    self
  }

  // --------------------------------------------------------------------------
  // Snapshot functions
  // --------------------------------------------------------------------------

  /** Do a snapshot on all properties. */
  def snapshot() = properties.values.foreach {_.snapshot()}
  
  /** Revert to the latest snapshot for all properties. */
  def revert() = properties.values.foreach {_.revert()}

  // --------------------------------------------------------------------------
  // History functions
  // --------------------------------------------------------------------------

  /** Save the current state to the history. */
  def save(t: Long) = properties.values.foreach {_.save(t)}

  /** Restore the state from the specified discrete time. */
  def restore(t: Long) = properties.values.foreach {_.restore(t)}

  /** Clear the history of this object. */
  def clear() = properties.values.foreach {_.clear()}

  // --------------------------------------------------------------------------
  // Utility functions
  // --------------------------------------------------------------------------  
  
  /** Validate the next values of the underlying structure 
   *  and replace the current value with it. 
   */
  def validate() = properties.values.foreach {_.validate()}

  /** Write the properties values to the underlying structure. The common case
   *  is to force these values to the physical world, but it could also do
   *  nothing.
   */
  def flush() = properties.values.foreach {_.flush()}
  
  /** Load the properties values from the underlying structure, typically 
   *  the physical world.
   */
  def load() = properties.values.foreach {_.load()}

  /** Reset all properties to their initial values.
   */
  def reset(interpreter: Interpreter)(implicit context: Context) = 
    properties.values.foreach {_.reset(interpreter)}

  /**
   * Retrieves a property or anything else.
   */
  def apply(property: String): Expr = { //PropertyRefTrait
    if(properties contains property) properties(property).ref else {
      property match {
        case "bottom" => 
          this match {
            case r: Rectangle => this("y") + this("height") / 2
            case c: Circle => this("y") + this("radius")
            case _ => throw new Exception(s"$this does not have a $property method")
          }
        case "top" =>
          this match {
            case r: Rectangle => this("y") - this("height") / 2
            case c: Circle => this("y") - this("radius")
            case _ => throw new Exception(s"$this does not have a $property method")
          }
        case "left" =>
          this match {
            case r: Rectangle => this("x") - this("width") / 2
            case c: Circle => this("x") - this("radius")
            case _ => throw new Exception(s"$this does not have a $property method")
          }
        case "right" =>
          this match {
            case r: Rectangle => this("x") + this("width") / 2
            case c: Circle => this("x") + this("radius")
            case _ => throw new Exception(s"$this does not have a $property method")
          }
        case "center" =>
          Vec2Expr(this("x"), this("y"))
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

  protected def simpleProperty[T : PongType](name: String, init: Expr): Property[T] = {
    val p = new SimpleProperty[T](name, init)
    properties += (name -> p)
    p
  }

  protected def simplePhysicalProperty[T : PongType](name: String, init: Expr)(f: T => Unit): Property[T] = {
    val p = new SimplePhysicalProperty[T](name, init) {
      val flusher = f
    }
    properties += (name -> p)
    p
  }

  protected def property[T : PongType](name: String, init: Expr)(f: T => Unit)(l: () => T): Property[T] = {
    val p = new PhysicalProperty[T](name, init) {
      val flusher = f
      val loader = l
    }
    properties += (name -> p)
    p
  }
  
  def getCopy(name : String): GameObject = {
    val m = makecopy(name)
    m.setCategory(this.category)
  }
  protected def makecopy(name: String): GameObject
}

trait Rectangular { self: GameObject =>
  val width: Property[Float]
  val height: Property[Float]  
}

