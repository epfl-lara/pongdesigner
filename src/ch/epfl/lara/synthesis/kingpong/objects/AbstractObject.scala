package ch.epfl.lara.synthesis.kingpong.objects

import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import org.jbox2d.collision.shapes.PolygonShape
import org.jbox2d.collision.shapes.CircleShape
import org.jbox2d.collision.shapes.Shape
import ch.epfl.lara.synthesis.kingpong.rules.Context
import ch.epfl.lara.synthesis.kingpong.rules.Events

abstract class AbstractObject(init_name: Expr, 
                              init_x: Expr,
                              init_y: Expr,
                              init_angle: Expr,
                              init_visible: Expr, 
                              init_color: Expr
                             ) extends GameObject(init_name) {
  
  val x = simpleProperty[Float]("x", init_x)
  val y = simpleProperty[Float]("y", init_y)
  val angle = simpleProperty[Float]("angle", init_angle)
  val visible = simpleProperty[Boolean]("visible", init_visible)
  val color = simpleProperty[Int]("color", init_color)
  
  def getShape: Shape
}


case class Box[T : PongType](val game: Game,
                        init_name: Expr, 
                        init_x: Expr,
                        init_y: Expr,
                        init_angle: Expr,
                        init_width: Expr, 
                        init_height: Expr, 
                        init_value: Expr,
                        init_visible: Expr,
                        init_color: Expr
                       ) extends AbstractObject(init_name, init_x, init_y, init_angle, init_visible, init_color)
                         with Rectangular {
  
  def className = "Box[" + implicitly[PongType[T]].getPongType.toString() + "]"
  // --------------------------------------------------------------------------
  // Properties
  // --------------------------------------------------------------------------
  
  val width = simpleProperty[Float]("width", init_width)
  val height = simpleProperty[Float]("height", init_height)
  val value = simpleProperty[T]("value", init_value)

  // --------------------------------------------------------------------------
  // Utility functions
  // --------------------------------------------------------------------------  
  
  def getAABB = {
    val bottomLeft = Vec2(x.get, y.get)
    val upperRight = bottomLeft add Vec2(width.get, height.get)
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }
  
  private val shape = new PolygonShape()
  
  //MIKAEL we can remove this initialization, don't we ?
  shape.setAsBox(game.typeCheckAndEvaluate[Float](init_width)/2,
                 game.typeCheckAndEvaluate[Float](init_height)/2)

  def getShape = {
    shape.setAsBox(width.get/2, height.get/2, Vec2(x.get, y.get), 0f)
    shape
  }

  def contains(pos: Vec2) = getAABB.contains(pos)
  
  def makecopy(name: String): GameObject = {
    this.copy[T](init_name = name)
  }
}

trait InputManager {
  
  def collectInput(from: Context)

}

/**
 * Input class already defining some methods
 */
case class Joystick(val game: Game,
                        init_name: Expr, 
                        init_x: Expr,
                        init_y: Expr,
                        init_angle: Expr,
                        init_radius: Expr, 
                        init_visible: Expr,
                        init_color: Expr
                       ) extends AbstractObject(init_name, init_x, init_y, init_angle, init_visible, init_color) 
                         with InputManager {
  
  def className = "Joystick"
  
  // --------------------------------------------------------------------------
  // Properties
  // --------------------------------------------------------------------------
  
  val radius = simpleProperty[Float]("radius", init_radius)
  val relative_x = simpleProperty[Float]("relative_x", 0)
  val relative_y = simpleProperty[Float]("relative_y", 0)
  val jump = simpleProperty[Boolean]("jump", false)
  val right = simpleProperty[Boolean]("right", false)
  val left = simpleProperty[Boolean]("left", false)
  
  def collectInput(from: Context) = {
    import Events._
    
    def updateRelativeCoords(dx: Float, dy: Float) = {
      relative_x.set(dx)
      relative_y.set(dy)
      jump.set(dy < -radius.get / 2)
      right.set(dx > radius.get / 2)
      left.set(dx < -radius.get / 2)
    }
    
    def updateAbsoluteCoords(at: Vec2) = {
      updateRelativeCoords( at.x - x.get, at.y - y.get)
    }
    
    from.events foreach {
      case FingerMove(_, at, objs) if objs contains this =>
        if(contains(at)) updateAbsoluteCoords(at) else updateRelativeCoords(0, 0)
      case FingerDown(at, objs) if objs contains this => updateAbsoluteCoords(at)
      case FingerUp(at, objs) if objs contains this => updateRelativeCoords(0, 0)
      case _ =>
    }
  } 

  // --------------------------------------------------------------------------
  // Utility functions
  // --------------------------------------------------------------------------  
  
  def getAABB = {
    val center = Vec2(x.get, y.get)
    val bottomLeft = center add Vec2(-radius.get, -radius.get)
    val upperRight = center add Vec2(radius.get, radius.get)
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }
  
  private val shape = new CircleShape()
  // MIKAEL here we assume the radius will never change, is it ok ?
  shape.setRadius(game.typeCheckAndEvaluate[Float](init_radius))

  def getShape = {
    shape.m_p.set(x.get, y.get)
    shape
  }

  // MIKEAL shouldn't we use the shape instead of the AABB ?
  def contains(pos: Vec2) = getAABB.contains(pos)
  
  def makecopy(name: String): GameObject = {
    this.copy(init_name = name)
  }
}


case class PathMovement(val game: Game,
                        init_name: Expr, 
                        init_x: Expr,
                        init_y: Expr,
                        init_angle: Expr,
                        init_visible: Expr,
                        init_color: Expr
                       ) extends AbstractObject(init_name, init_x, init_y, init_angle, init_visible, init_color) {
  

  def className = "Movement"

  def contains(pos: Vec2): Boolean = false
  def getAABB(): AABB = ???
  protected def makecopy(name: String): GameObject = ???
  def getShape: Shape = ???
}


case class Cell2D(val game: Game, init_name: Expr) extends GameObject(init_name) {
  var left: Cell2D = null
  var top: Cell2D = null
  var right: Cell2D = null
  var bottom: Cell2D = null
  var content: GameObject = null

  def className = "Cell2D"

   def angle: Property[Float] = ???
   def contains(pos: Vec2): Boolean = ???
   def getAABB(): AABB = ???
   protected def makecopy(name: String): GameObject = ???
   def visible: 
 Property[Boolean] = ???
 def x: 
 Property[Float] = ???
 def y: 
 Property[Float] = ???
 def color: Property[Int] = ???
}

case class Array2D(val game: Game, init_name: Expr,
    init_x: Expr,
    init_y: Expr,
    init_angle: Expr,
    init_height: Expr,
    init_value: Expr,
    init_visible: Expr,
    init_color: Expr,
    init_size: Expr
    ) extends AbstractObject(init_name, init_x, init_y, init_angle, init_visible, init_color) {
 
 def className = "Array2D"
 def getShape = ???
  
  val size = simpleProperty[Int]("size", init_size)
 def contains(pos:  Vec2): Boolean = ???
 def getAABB(): 
 AABB = ???
 protected def makecopy(name: String): 
 GameObject = ???
}
