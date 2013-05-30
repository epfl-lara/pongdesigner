package ch.epfl.lara.synthesis.kingpong.objects

import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._

abstract class AbstractObject(init_name: Expr, 
                              init_x: Expr,
                              init_y: Expr,
                              init_angle: Expr,
                              init_visible: Expr
                             ) extends GameObject(init_name) {
  
  val x = simpleProperty[Float]("x", init_x)
  val y = simpleProperty[Float]("y", init_y)
  val angle = simpleProperty[Float]("angle", init_angle)
  val visible = simpleProperty[Boolean]("visible", init_visible)  
}

class Box[T : PongType](init_name: Expr, 
                        init_x: Expr,
                        init_y: Expr,
                        init_angle: Expr,
                        init_width: Expr, 
                        init_height: Expr, 
                        init_value: Expr,
                        init_visible: Expr
                       ) extends AbstractObject(init_name, init_x, init_y, init_angle, init_visible)
                         with Rectangular {

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

  def contains(pos: Vec2) = getAABB.contains(pos)

}
