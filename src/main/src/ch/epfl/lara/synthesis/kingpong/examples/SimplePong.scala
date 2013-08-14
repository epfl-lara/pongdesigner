package ch.epfl.lara.synthesis.kingpong

import scala.collection.mutable.{Set => MSet}
import scala.math.Numeric$DoubleIsFractional$

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.History
import ch.epfl.lara.synthesis.kingpong.common.RingBuffer
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.rules.Rules._
import ch.epfl.lara.synthesis.kingpong.rules.Events._
import ch.epfl.lara.synthesis.kingpong.rules.Context
import scala.Dynamic
import scala.language.dynamics

//TODO remove when useless
import org.jbox2d.dynamics.BodyType

import android.util.Log

class SimplePong() extends Game {
  val world = new PhysicalWorld(Vec2(0, 0f))

  val borders = Category("Borders")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
  val blocks = Category("Blocks")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
  val paddles = Category("Paddles")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
  val balls = Category("Balls")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.DYNAMIC)
  val duplicators = Category("duplicators")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
  val scores = Category("Scores")()
  val cat2 = Category("Static objects")()

  rectangle(borders)(name="Border1", x=2.5, y=0, width=4.8, height=0.1)
  rectangle(borders)(name="Border2", x=0, y=5, width=0.1, height=10)
  rectangle(borders)(name="Border3", x=5, y=5, width=0.1, height=10)
  rectangle(duplicators)(name="BallDuplicator1", x=1.5, y=1.5, width=1, height=1, color=red)
  val paddle1 = rectangle(paddles)(name="Paddle1", x=2.5, y=9.5, width=1, height=0.2)
  for(j <- 0 until 4) { // Appears as for (i, j) in [0,4]x[0,3]:
    for(i <- 0 until 3) { // 
      rectangle(blocks)(name=s"Block${i}_$j", x = 1.5+i, y=2.5+0.5*j, width = 0.9, height = 0.45, color=colorful(j))
    }
  }

  circle(balls)(name="Ball1", x=2.5, y=8.5, radius = 0.25, velocity=Vec2(0, -5.0f))

  val score = intbox(scores)("Score1", x=2, y=5, value = 0, width=1, height=1)
  
  //val base = rectangle("Base", 0, 8, width = 20, height = 0.5, tpe = BodyType.STATIC, category=cat2)

  val r1 = foreach(balls)("ball"){
    whenever(on(paddle1("bottom") < obj("ball")("top"))) { Seq(
      score("value") -= 1,
      obj("ball")("x").reset(),
      obj("ball")("y").reset()
    )}
  }
  
  /** In the game, it appears as a python-like language:
   *    on Paddle1.bottom > Ball1.y:
   *      decrease Score1.value by 1
   *      reset Ball1.x
   *      reset Ball1.y
   * 
   *  And could have been constructed by
   *    1) Selecting a paddle and a ball as conditions
   *    The system
   *    If there are multiple balls, the systems asks if this rule apply for this ball or for all balls.
   *    2) Selecting an alignment condition, either current or pre-defined (here the ball is below the paddle)
   *    3) change the score value by -1
   *    4) Push the reset position button on the ball somewhere in its menu.
   *    5) Validate.
   *  
   *  If any of the three objects (Ball1, Paddle1, Score1) is duplicated, then the following lines are automatically added:
   *  
   *  for ball in Balls:
   *  for paddle in Paddles:
   *  for score in Scores:
   *    if paddle above ball:
   *      decrease score.value by 1
   *      reset ball.x
   *      reset ball.y
   *      
   *  I would like to write alignment code such that:
   *  
   *      ball.y = choose( ball.y => ball.bottom == paddle.bottom )
   *  where bottom is in fact replaced by y+radius in the first case and y+height in the second.
   *  
   *  Invariants: Object within boundaries.
   *     x' = x + k && 0 <= k <= 3, k max
   *     x' <= 10
   *  Produces the code:
   *     x' = max(x+3, 10)
   */

  val r2 = foreach(balls, blocks)("ball", "block") {
    whenever(Collision(obj("ball"), obj("block"))) { Seq(
      score("value") += 1, // TODO operator that takes constants.
      obj("block")("visible") = false
    )}  
  }

  val r3 = foreach(balls)("ball") { whenever(FingerDownOver(obj("ball"))) { Seq(
    obj("ball")("radius") += 0.1
  )}}
  
  val r4 = whenever(FingerMoveOver(paddle1)){ Seq(
    paddle1("x") += Val("dx")
  )}
  
  val r5 = foreach(duplicators, balls)("duplicator", "ball"){
    whenever(Collision(obj("duplicator"), obj("ball"))){ Seq(
      obj("ball").copy("ball2")(Seq(
        obj("ball2")("x") += 0.25,
        obj("ball")("x") -= 0.25,
        obj("duplicator").delete()
      ))
    )}
  }

  register(r1)
  register(r2)
  register(r3)
  register(r4)
  //register(r5)
}
