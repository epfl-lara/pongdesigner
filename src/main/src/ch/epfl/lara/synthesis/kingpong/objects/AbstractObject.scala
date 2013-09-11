package ch.epfl.lara.synthesis.kingpong.objects

import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import scala.Dynamic
import scala.language.dynamics
import org.jbox2d.collision.shapes.PolygonShape
import org.jbox2d.collision.shapes.Shape

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


case class Box[T : PongType](protected val game: Game,
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
  
  def className = s"Box"
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
  shape.setAsBox(game.typeCheckAndEvaluate[Float](init_width)/2,
                 game.typeCheckAndEvaluate[Float](init_height)/2)

  def getShape = {
    shape.setAsBox(width.get/2, height.get/2, Vec2(x.get, y.get), 0f)
    shape
  }

  def contains(pos: Vec2) = getAABB.contains(pos)
  
  def makecopy(name: String): GameObject = {
    this.copy[T](init_name=StringIsExpr(name))
  }
}

case class Cell2D(protected val game: Game, init_name: Expr) extends GameObject(init_name) {
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
}

case class Array2D(protected val game: Game, init_name: Expr,
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












