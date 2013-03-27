package ch.epfl.lara.synthesis.kingpong.objects

import scala.collection.mutable.{Map => MMap}

import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._

abstract class GameObject(init_name: Expr) extends WithPoint with Timed { self =>
  
  /** Main point for this object. Here set to the position. */
  protected def point = Vec2(x.get, y.get)

  val properties: MMap[String, Property[_]] = MMap.empty
  
  // --------------------------------------------------------------------------
  // Properties
  // --------------------------------------------------------------------------
  
  val name: Property[String] = simpleProperty[String]("name", init_name)
  
  val x: Property[Float]
  val y: Property[Float]
  val angle: Property[Float]
  val visible: Property[Boolean]

  // --------------------------------------------------------------------------
  // Category
  // --------------------------------------------------------------------------

  var category: Category = DefaultCategory(this)
  
  def withCategory(c: Category): self.type = {
    c add this
    category = c
    self
  }

  // --------------------------------------------------------------------------
  // Timed functions
  // --------------------------------------------------------------------------  
    
  /** Do a snapshot on all properties. */
  def snapshot() = properties.values.foreach {_.snapshot()}
  
  /** Revert to the latest snapshot for all properties. */
  def revert() = properties.values.foreach {_.revert()}
  
  // --------------------------------------------------------------------------
  // Utility functions
  // --------------------------------------------------------------------------  
  
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
  def reset(implicit interpreter: Interpreter) = 
    properties.values.foreach {_.reset}

  def apply(property: String): PropertyRef = properties(property).ref

  def getAABB(): AABB
  
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
}

trait Rectangular { self: GameObject =>
  val width: Property[Float]
  val height: Property[Float]  
}

