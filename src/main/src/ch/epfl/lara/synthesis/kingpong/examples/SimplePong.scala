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
  val world = new PhysicalWorld(Vec2(0, 1.5f))

  val borders = Category("Borders")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
  val blocks = Category("Blocks")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
  val paddles = Category("Paddles")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
  val balls = Category("Balls")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.DYNAMIC)
  val duplicators = Category("duplicators")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
  val scores = Category("Scores")()
  val cat2 = Category("Static objects")()

  rectangle(borders)(name="Border1", x=2.5, y=0.5, width=5, height=1)
  rectangle(borders)(name="Border2", x=0.5, y=5, width=1, height=10)
  rectangle(borders)(name="Border3", x=4.5, y=5, width=1, height=10)
  rectangle(duplicators)(name="BallDuplicator1", x=1.5, y=1.5, width=1, height=1)
  val paddle1 = rectangle(paddles)(name="paddle1", x=2.5, y=9.5, width=2, height=1)
  for(j <- 0 until 4) {
    for(i <- 0 until 3) {
      rectangle(blocks)(name=s"Block${i}_$j", x = 1.5+i, y=2.5+j, width = 1, height = 1)
    }
  }

  circle(balls)(name="Ball1", x=2.5, y=8.5, radius = 0.25)

  val score = intbox(scores)("Score1", x=2, y=5, value = 0)
  
  //val base = rectangle("Base", 0, 8, width = 20, height = 0.5, tpe = BodyType.STATIC, category=cat2)

  val r1 = foreach(balls)("ball"){
    whenever(paddle1("bottom") < obj("ball")("y")) { Seq(
      score("value") -= 1,
      obj("ball")("x") = obj("ball").init_x,
      obj("ball")("y") = obj("ball").init_y
    )}
  }

  val r2 = foreach(balls, blocks)("ball", "block") {
    whenever(Collision(obj("ball"), obj("block"))) { Seq(
      score("value") += 1,
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
}
