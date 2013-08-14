package ch.epfl.lara.synthesis.kingpong.objects

import scala.collection.mutable.{Set => MSet, Map => MMap}
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import org.jbox2d.dynamics._

object Category{
  def apply(name: String)(angle: Expr = 0,
      width: Expr = 1,
      height: Expr = 1,
      radius: Expr = 0.5,
      value: Expr = 0,
      visible: Expr = true,
      velocity: Expr = Vec2(0, 0),
      angularVelocity: Expr = 0,
      density: Expr = 1,
      friction: Expr = 0.2,
      restitution: Expr = 0.5,
      fixedRotation: Expr = true,
      color: Expr = 0xFF000000,
      tpe: BodyType = BodyType.DYNAMIC): Category = {
    new Category(name)(angle, width, height, radius, value, visible, velocity, angularVelocity, density, friction, restitution, fixedRotation, color, tpe)
  }
  
}

class Category(name: String)(val angle: Expr = 0,
  val width: Expr = 1,
  val height: Expr = 1,
  val radius: Expr = 0.5,
  val value: Expr = 0,
  val visible: Expr = true,
  val velocity: Expr = Vec2(0, 0),
  val angularVelocity: Expr = 0,
  val density: Expr = 1,
  val friction: Expr = 0.2,
  val restitution: Expr = 0.5,
  val fixedRotation: Expr = true,
  val color: Expr = 0xFF000000,
  val tpe: BodyType = BodyType.DYNAMIC) { self =>

  private val _children = MSet.empty[GameObject]
  
  def objects: Iterable[GameObject] = _children

  def add(o: GameObject): self.type = {
    _children += o
    o.category = this
    this
  }

  def remove(o: GameObject): self.type = {
    _children -= o
    this
  }

  /*
  private def resolveProperties() {
    properties.clear()
    val keys = _children.map(_.properties.keySet).foldLeft(Set.empty[String]) {_ intersect _}
    properties ++= keys.map { k =>
      val p = _children.map(_.properties).foldLeft(Set.empty[Property[_]]){_ + _(k)}
      k -> CategoryPropertyRef(p)
    }
  }
  */
}

object DefaultCategory {
  def apply(o: GameObject): Category = new Category(o.name.get)().add(o)
}