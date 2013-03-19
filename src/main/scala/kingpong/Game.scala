package ch.epfl.lara.synthesis.kingpong

import scala.collection.mutable.{Set => MSet}

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._

//TODO remove when useless
import org.jbox2d.dynamics.BodyType


trait Game extends TypeChecker with Interpreter { self => 
  implicit protected val game = self

  val world: PhysicalWorld

  private val _objects = MSet.empty[GameObject]

  def objects = _objects.toSeq //TODO warning for performances ?
  
  def add(o: GameObject) = _objects add o

  def typeCheckAndEvaluate[T : PongType](e: Expr): T = {
    typeCheck(e, implicitly[PongType[T]].getPongType)
    evaluate[T](e)
  }

  def circle(name: Expr,
             x: Expr,
             y: Expr,
             angle: Expr = 0,
             radius: Expr = 20, //TODO centralized default values
             visible: Expr = true,
             density: Expr = 0.5,
             friction: Expr = 1,
             restitution: Expr = 1,
             fixedRotation: Expr = true,
             tpe: BodyType = BodyType.DYNAMIC)
            (implicit game: Game): Circle = {
    val c = new Circle(game, name, x, y, angle, radius, visible, density, friction, restitution, fixedRotation, tpe)
    game add c
    c
  }

  def rectangle(name: Expr,
                x: Expr,
                y: Expr,
                angle: Expr = 0,
                init_width: Expr = 20,
                init_height: Expr = 20,
                visible: Expr = true,
                density: Expr = 0.5,
                friction: Expr = 1,
                restitution: Expr = 1,
                fixedRotation: Expr = true,
                tpe: BodyType = BodyType.DYNAMIC)
               (implicit game: Game): Rectangle = {
    val r = new Rectangle(game, name, x, y, angle, init_width, init_height, visible, density, friction, restitution, fixedRotation, tpe)
    game add r
    r
  }
}


class SimpleGame extends Game {

  val world = new PhysicalWorld(Vec2(0, 0))
  
  
  val c = circle("Circle 1", 200, 50, radius = 50)

}