package ch.epfl.lara.synthesis.kingpong.examples

import scala.collection.mutable.{Set => MSet}
import scala.math.Numeric$DoubleIsFractional$
import scala.Dynamic
import scala.language.dynamics

import org.jbox2d.dynamics.BodyType

import android.util.Log

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.ColorConstants._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.rules.Events._
import ch.epfl.lara.synthesis.kingpong.rules.Context

class SimplePong extends Game {
  val world = new PhysicalWorld(Vec2(0, 0f))

  val borders = Category("Borders")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
  val blocks = Category("Blocks")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
  val paddles = Category("Paddles")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
  val balls = Category("Balls")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.DYNAMIC)
  val duplicators = Category("duplicators")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
  val scores = Category("Scores")()
  val cat2 = Category("Static objects")()
  val states = Category("states")()

  val Border1 = rectangle(borders)(name="Border1", x=2.5, y=0, width=4.8, height=0.1)
  val Border2 = rectangle(borders)(name="Border2", x=0, y=5, width=0.1, height=10)
  val Border3 = rectangle(borders)(name="Border3", x=5, y=5, width=0.1, height=10)
  val Border4 = rectangle(borders)(name="Border4", x=2.5, y= -0.1, width=4.8, height=0.1)
  rectangle(duplicators)(name="BallDuplicator1", x=2.5, y=6, width=1, height=1, color=red)
  val paddle1 = rectangle(paddles)(name="Paddle1", x=2.5, y=9.5, width=1, height=0.2)
  
  //Put this into an expression.
  
  for(j <- 0 until 4) { // Appears as for (i, j) in [0,4]x[0,3]:
    for(i <- 0 until 3) { // 
      rectangle(blocks)(name=s"Block${i}_$j", x = 1.5+i, y=2.5+0.5*j, width = 0.9, height = 0.45, color=colorful(j))
    }
  }

  val Ball1 = circle(balls)(name="Ball1", x=2.5, y=8.5, radius = 0.25, velocity=Vec2(0, -5.0f))

  val score = intbox(scores)("Score1", x=1, y=8, value = 0, width=1, height=0.5)
  
  val started = booleanbox(states)("Started", x=1, y=9, value=false, height=0.5)
  
  //val base = rectangle("Base", 0, 8, width = 20, height = 0.5, tpe = BodyType.STATIC, category=cat2)

  val r1 = foreach(balls) { ball =>
    whenever(ball.top >= paddle1.bottom) ( // == below
      score.value -= 1,
      started.value := false,
      ball.x := 2.5,
      ball.y := 8.5
    )
  }
  
  val r1bis = foreach(balls) { ball =>
    whenever(Collision(ball, paddle1)) ( // == collides
      ball.velocity += Tuple(Seq((ball.x - paddle1.x) * 5, 0))
    )
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

  val r2 = foreach(balls, blocks, scores) { (ball, block, score) =>
    whenever(Collision(ball, block)) (
      score.value += 1,
      ball.color := block.color,
      block.visible := false
    )
  }
  
  val r22 = foreach(balls, borders) { (ball, border) =>
    whenever(Collision(ball, border)) (
      border.color := ball.color
    )  
  }
  
  val r4 = fingerMoveOver(paddle1) { move =>
    paddle1.x += move._2._1 - move._1._1
  }

  val r5 = foreach(duplicators, balls) { (duplicator, ball) =>
    whenever(Collision(duplicator, ball)) {
      copy(ball) { copy => Seq(
        copy.x += 0.25,
        copy.velocity += Vec2(0.5f, 1f),
        ball.x -= 0.25,
        duplicator.visible := false
      )}
    }
  }
  
  val r6 = foreach(balls) { ball =>
    whenever(!started.value) (
      ball.x := paddle1.x,
      
      //TODO takes care of choose in examples
      
      //ball.y := Choose(List(ball.y), ball.bottom =:= paddle1.top),
      ball.velocity := Vec2(0, 0)
      // Should replace by obj("ball")("y") - obj("ball")("radius") and solved
    )
  }
  
  val r7 = foreach(balls) { (ball) =>
    whenever(!started.value && IsFingerUpOver(paddle1)) (
      started.value := true,
      ball.velocity := Vec2(0, -5.0f)
    )
  }
  
  
  val r8 = Block( NOP
//     paddle1.x := Choose(List(paddle1.x),
//                         (paddle1.left >= Border2.right) && // toRightOfAtMost
//                         (paddle1.right <= Border3.left)),  // toLeftOfAtMost
//     Seq(Border4.x, Border4.width) := Choose(List(Border4.x, Border4.width), 
//                                             (Border4.left =:= Ball1.left) && // alignLeft
//                                             (Border4.right =:= paddle1.right))    // alignRight
  )

  register(r1)
  register(r1bis)
  register(r2)
  register(r22)
  //register(r3)
  register(r8)
  register(r5)
  register(r6)
  register(r7)
  register(r4)
}
