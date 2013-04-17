package ch.epfl.lara.synthesis.kingpong

import scala.collection.mutable.{Set => MSet}

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.rules.Rules._

//TODO remove when useless
import org.jbox2d.dynamics.BodyType

import android.util.Log

trait Game extends TypeChecker with Interpreter { self => 
  implicit protected val game = self

  val world: PhysicalWorld

  private val _objects = MSet.empty[GameObject]
  private val _rules = MSet.empty[Rule]

  /** All objects in this game. */
  def objects = _objects.iterator

  /** All the rules in this game. */
  def rules = _rules.iterator
  
  def add(o: GameObject) = _objects add o
  def add(rule: Rule) {
    typeCheck(rule)
    _rules add rule
  }

  def typeCheckAndEvaluate[T : PongType](e: Expr): T = {
    typeCheck(e, implicitly[PongType[T]].getPongType)
    eval(e).as[T]
  }

  def circle(name: Expr,
             x: Expr,
             y: Expr,
             radius: Expr = 0.5, //TODO centralized default values
             visible: Expr = true,
             density: Expr = 1,
             friction: Expr = 0.1,
             restitution: Expr = 0.9,
             fixedRotation: Expr = true,
             tpe: BodyType = BodyType.DYNAMIC)
            (implicit game: Game): Circle = {
    val c = new Circle(game, name, x, y, radius, visible, density, friction, restitution, fixedRotation, tpe)
    c.reset
    c.flush()
    game add c
    c
  }

  def rectangle(name: Expr,
                x: Expr,
                y: Expr,
                angle: Expr = 0,
                width: Expr = 1,
                height: Expr = 1,
                visible: Expr = true,
                density: Expr = 1,
                friction: Expr = 0.1,
                restitution: Expr = 0.9,
                fixedRotation: Expr = true,
                tpe: BodyType = BodyType.DYNAMIC)
               (implicit game: Game): Rectangle = {
    val r = new Rectangle(game, name, x, y, angle, width, height, visible, density, friction, restitution, fixedRotation, tpe)
    r.reset
    r.flush()
    game add r
    r
  }

  def whenever(cond: Expr)(actions: Seq[Stat]): Unit = {
    game add new Whenever(cond, toSingleStat(actions))
  }

  def once(cond: Expr)(actions: Seq[Stat]): Unit = {
    game add new Once(cond, toSingleStat(actions))
  }

  def on(cond: Expr)(actions: Seq[Stat]): Unit = {
    game add new On(cond, toSingleStat(actions))
  }


  /** Perform a step. */
  private[kingpong] def update(): Unit = {
    world.step()
    objects foreach {_.load()}
    //rules foreach {_.evaluate}
    //objects foreach {_.flush()}
  }

  private def toSingleStat(stats: Seq[Stat]): Stat = stats match {
    case Seq()  => NOP
    case Seq(s) => s
    case _      => Block(stats)
  }

}

class EmptyGame() extends Game {
  val world = new PhysicalWorld(Vec2(0, 1f))

  rectangle("Rectangle 1", 2, 0, width = 1, height = 1, fixedRotation = false)

  circle("Circle 1", 3, 2, radius = 1, fixedRotation = false)
  circle("Circle 2", 2.5, 4, radius = 0.5, fixedRotation = false)
  
  rectangle("Rectangle 2", 5, 8, width = 10, height = 1, tpe = BodyType.STATIC)

}
