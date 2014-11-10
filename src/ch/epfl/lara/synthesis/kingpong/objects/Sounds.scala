package ch.epfl.lara.synthesis.kingpong.objects

import org.jbox2d.collision.shapes.PolygonShape

import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._

class SoundTTS (
    val game: Game,
    init_name: Expr, 
    init_x: Expr,
    init_y: Expr,
    init_angle: Expr,
    init_width: Expr, 
    init_height: Expr,
    init_visible: Expr,
    init_color: Expr,
    init_language: Expr,
    init_text: Expr,
    init_time: Expr,
    planned: GameObject.IsPlanned
    ) extends AbstractObject(init_name, init_x, init_y, init_angle, init_visible, init_color, planned)
      with ResizableRectangular
      with Movable
      with Visiblable
      with Colorable
      with Directionable with FixedRectangularContains with Timeable with ValueTextable {

  val width = simpleProperty[Float]("width", init_width)
  val height = simpleProperty[Float]("height", init_height)
  val value = simpleProperty[String]("value", init_text)
  val language = simpleProperty[String]("language", init_language)
  
  val time = simpleProperty[Int]("time", init_time)
  val played = simpleProperty[Boolean]("played", false)

  // --------------------------------------------------------------------------
  // Utility functions
  // --------------------------------------------------------------------------  
  
  def getAABB = {
    val bottomLeft = Vec2(x.get - width.get/2, y.get - height.get/2)
    val upperRight = bottomLeft add Vec2(width.get, height.get)
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }
  
  private val shape = new PolygonShape()
  
  def getShape = {
    shape.setAsBox(width.get/2, height.get/2, Vec2(x.get, y.get), 0f)
    shape
  }

  def rawCopy(f: HistoricalProperty[_] => Expr) = {
    new SoundTTS(game, f(name), f(x), f(y), f(angle), f(width), f(height), f(visible), f(color), f(language),
                 f(value), f(time), game.isCopyingPlanned)
  }
}
