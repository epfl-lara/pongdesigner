package ch.epfl.lara.synthesis.kingpong.examples

import org.jbox2d.dynamics.BodyType
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.PhysicalWorld
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.objects._

class TestGame extends Game {
  val world = new PhysicalWorld(Vec2(0, 1.5f))

  val cat = Category("Moving objects")()
  val catAdd = Category("Moving objects2")()

  val rect1 = rectangle(cat)(name="Rectangle 1", x=2, y=0, width = 1, height = 1, fixedRotation = false)
  val rect2 = rectangle(cat)(name="Rectangle 2", x=3.4, y=0, width = 1, height = 2, fixedRotation = false)

  circle(cat)("Circle 1", 3, 2, radius = 1, fixedRotation = false)
  val c2 = circle(catAdd)("Circle 2", 2.5, 4, radius = 0.5, fixedRotation = false)
  
  val score = intbox(Category("scores")())("Score", 1, 1, value = 0)

  val cat2 = Category("Static objects")()
  val catArray = Category("Array objects")()

  val base = rectangle(cat2)("Base", 0, 8, width = 20, height = 0.5, tpe = BodyType.STATIC)

  val arr = array(catArray)("MyArray", 1.3, 4)
  
  
  val r1 = foreach(cat)("o"){ foreach(base.category)("base") {
    whenever(obj("base")("y") < obj("o")("y"))(
      obj("o")("y") := 0, 
      obj("o")("velocity") := Vec2(0, 0)
    )
  }}

  val r2 = foreach(cat, cat2)("o1", "o2") { foreach(score.category)("score") {
    whenever(Collision(obj("o1"), obj("o2"))) (
      obj("score")("value") += 1
    )
  }}

  val r3 = foreach(catAdd)("c2") {
    whenever(FingerDownOver(obj("c2")))(
    obj("c2")("radius") += 0.1
  )}
  
  val cell = arr.cells(1)(1)
  
  val r4 = whenever(Contains(cell, rect1))(
      rect1("x") := cell("x"),
      rect1("y") := cell("y"),
      rect1("velocity") := Vec2(0, 0)
    )

  register(r1)
  register(r2)
  register(r3)
  register(r4)

}
