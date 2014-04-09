package ch.epfl.lara.synthesis.kingpong.objects

import org.jbox2d.collision.shapes.PolygonShape
import org.jbox2d.collision.shapes.CircleShape
import org.jbox2d.collision.shapes.Shape
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.rules.Context
import ch.epfl.lara.synthesis.kingpong.rules.Events
import scala.collection.mutable.ArrayBuffer


/**
 * An element drawn at a specific time.
 */
case class DrawingElement(time: Int, xFrom: Float, yFrom: Float, xTo: Float, yTo: Float, width: Float, color: Int) {
  var next: Option[DrawingElement] = None
  var pred: Option[DrawingElement] = None
}


/**
 * Provides time-dependent drawing facilities for presentations.
 */
case class DrawingObject(val game: Game,
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
                         with Rectangular
                         with Movable
                         with Rotationable
                         with Visiblable
                         with Colorable{
  def className = "DrawingObject"
  
  val width = simpleProperty[Float]("width", init_width)
  val height = simpleProperty[Float]("height", init_height)
  val drawings = ArrayBuffer[List[DrawingElement]]() // Records all drawings.
  
  // --------------------------------------------------------------------------
  // Utility functions
  // --------------------------------------------------------------------------  
  
  def getAABB = {
    val bottomLeft = Vec2(x.get, y.get)
    val upperRight = bottomLeft add Vec2(width.get, height.get)
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }
  
  private val shape = new PolygonShape()
  
  def getShape = {
    shape.setAsBox(width.get/2, height.get/2, Vec2(x.get, y.get), 0f)
    shape
  }

  def contains(pos: Vec2) = getAABB.contains(pos)
  
  def makecopy(name: String): GameObject = {
    this.copy(init_name = name)
  }
}

