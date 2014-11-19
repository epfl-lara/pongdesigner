package ch.epfl.lara.synthesis.kingpong.examples

import scala.collection.mutable.{Set => MSet}
import scala.math.Numeric$DoubleIsFractional$

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
import org.jbox2d.dynamics.BodyType

import android.util.Log

class PlatformGame extends Game {
  val world = new PhysicalWorld(Vec2(0, 15f))

  val borders = Category("Borders")(friction=0, restitution=0.1, fixedRotation=true, tpe=BodyType.STATIC)
  val coins= Category("Coins")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC, color=orange, sensor=true)
  val characters = Category("Characters")(friction=0.5, restitution=0.1, fixedRotation=true, tpe=BodyType.DYNAMIC)
  val io = Category("IO")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.DYNAMIC)
  val balls = Category("Balls")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.DYNAMIC)
  //val Ball1 = circle(balls)(name="Ball1", x=8.5, y=7.5, radius = 0.4, velocity=Vec2(0, 0.0f))
  
  rectangle(borders)(name="Border1", x=0, y=10, width=1, height=21)
  rectangle(borders)(name="Border2", x=16, y=10, width=1, height=21)
  rectangle(borders)(name="Border3", x=8, y=0, width=17, height=1)
  rectangle(borders)(name="Border4", x=8, y=20, width=17, height=1)
  rectangle(borders)(name="Border5", x=3, y=16.5, width=5, height=6)  
  rectangle(borders)(name="Border6", x=3, y=9.5, width=4, height=1)

  rectangle(borders)(name="Platform", x=7.5, y=11.5, width=2, height=1)
  
  rectangle(borders)(name="Platform2", x=4.5, y=6, width=8, height=1)
  rectangle(borders)(name="Platform2", x=14, y=5, width=3, height=1)
  
  rectangle(borders)(name="Border7", x=13.5, y=15.5, width=2, height=6)
  
  val stretching = rectangle(borders)(name="Border8", x=10.5, y=12.5, width=2, height=14)
  
  val player = character(characters)(name="SuperMaria", x=1.5, y=12, width=1, height=2, color=colorful(0))
  
  circle(coins)(name="Coin1", x=1.5, y=7.5, radius=0.5)
  circle(coins)(name="Coin2", x=3, y=7.5, radius=0.5)
  circle(coins)(name="Coin3", x=1.5, y=4.5, radius=0.5)
  circle(coins)(name="Coin4", x=3, y=4.5, radius=0.5)
  circle(coins)(name="Coin5", x=1.5, y=2, radius=0.5)
  circle(coins)(name="Coin6", x=3, y=2, radius=0.5)
  circle(coins)(name="Coin7", x=6.5, y=4.5, radius=0.5)  
  circle(coins)(name="Coin8", x=6.5, y=2, radius=0.5)
  circle(coins)(name="Coin9", x=7.5, y=9.5, radius=0.5)
  
  val j = joystick(io)(name="joystick", x=3, y=17, radius=2.5, color=white)
  
  val r1 = whenever(j.jump && player.grounded)(
      player.velocity := Tuple(Seq(TupleSelect(player.velocity, 1), -12))
  )
  
  val r2 = whenever(j.isRight)(
      player.velocity := Tuple(Seq(3, TupleSelect(player.velocity, 2))),
      player.friction := 0f
  )
  
  val r3 = whenever(j("isLeft"))(
      player.velocity := Tuple(Seq(-3, TupleSelect(player.velocity, 2))),
      player.friction := 0f
  )
  
  val r4 = whenever(!j.isLeft && !j.isRight)(
      player.friction := 1f
  )
  
  register(r1)
  register(r2)
  register(r3)
  register(r4)
}
