package ch.epfl.lara.synthesis.kingpong.objects

import scala.collection.mutable.{HashSet => MSet}

import org.jbox2d.dynamics.BodyType

import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.ColorConstants
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.rules.Events._

object Category {
  def apply(name: String)(
      angle: Expr = 0,
      width: Expr = 1,
      height: Expr = 1,
      radius: Expr = 0.5,
      value: Expr = 0,
      randomMinValue: Expr = 0,
      randomMaxValue: Expr = 1,
      visible: Expr = true,
      velocity: Expr = Vec2(0, 0),
      angularVelocity: Expr = 0,
      density: Expr = 1,
      friction: Expr = 0.2,
      restitution: Expr = 0.5,
      fixedRotation: Expr = true,
      color: Expr = ColorConstants.black,
      sensor: Expr = false,
      tpe: BodyType = BodyType.DYNAMIC,
      stroke_width: Expr = 3f,
      color_drawing: Expr = ColorConstants.black)(implicit game: Game): CategoryObject = {
    new CategoryObject(game, name, angle, width, height, radius, value, randomMinValue, randomMaxValue, 
        visible, velocity, angularVelocity, density, friction, restitution, fixedRotation, color, sensor, tpe, stroke_width, color_drawing)
  }
}

trait Category {
  def game: Game
  def name: String
  def objects: Traversable[GameObject]
}

class CategoryObject(
    val game: Game,
    val name: String,
    val angle: Expr,
    val width: Expr,
    val height: Expr,
    val radius: Expr,
    val value: Expr,
    val randomMinValue: Expr,
    val randomMaxValue: Expr,
    val visible: Expr,
    val velocity: Expr,
    val angularVelocity: Expr,
    val density: Expr,
    val friction: Expr,
    val restitution: Expr,
    val fixedRotation: Expr,
    val color: Expr,
    val sensor: Expr,
    val tpe: BodyType,
    val stroke_width: Expr,
    val color_drawing: Expr)
    extends NotNull with Category { self =>

  private val _children = MSet.empty[GameObject]
  
  def objects: Traversable[GameObject] = _children

  def add(o: GameObject): self.type = {
    _children += o
    this
  }

  def remove(o: GameObject): self.type = {
    _children -= o
    this
  }
}

object DefaultCategory {
  def apply(o: GameObject): CategoryObject = Category(o.name.get)()(o.game).add(o)
  def apply(o: String, g: Game): CategoryObject = Category(o)()(g)
}
