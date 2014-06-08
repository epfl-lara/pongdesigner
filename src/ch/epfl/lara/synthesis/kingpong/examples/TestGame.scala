package ch.epfl.lara.synthesis.kingpong.examples

import org.jbox2d.dynamics.BodyType
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.PhysicalWorld
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._

class TestGame extends Game {
  val world = new PhysicalWorld(Vec2(0, 3.5f))

  val moving_objects = Category("Moving_objects")()
  val moving_objects2 = Category("Moving_objects2")()

  val rect1 = rectangle(moving_objects)(name="Rectangle 1", x=2, y=0, width = 1, height = 1, fixedRotation = false)
  val rect2 = rectangle(moving_objects)(name="Rectangle 2", x=3.4, y=0, width = 1, height = 2, fixedRotation = false)

  circle(moving_objects)("Circle 1", 3, 2, radius = 1, fixedRotation = false)
  val c2 = circle(moving_objects2)("Circle 2", 2.5, 4, radius = 0.5, fixedRotation = false)
  
  val score = intbox(Category("scores")())("Score", -10, 1, value = 0)

  val cat2 = Category("Static objects")()
  val catArray = Category("Array objects")()

  val base = rectangle(cat2)("Base", 0, 8, width = 20, height = 0.5, tpe = BodyType.STATIC)

  val arr = array(catArray)("MyArray", 1.3, 4, 2, 3)
  
  //val soundtts = soundTTS(cat2)(name="soundtts",x= -10.5f, y=4f, time= 1, language="en", text="Welcome to Pong Designer, made by Lara at E P F L !")
  
  //val soundtts2 = soundTTS(cat2)(name="soundtts2",x= -10.5f, y=5f, time= 150, language="fr", text="Bienvenue à Pongue Dizaïneur réalisé par Lara, à l'eupéheffelle !")
  
  val r1 = foreach(moving_objects) { obj =>
    whenever(base.y < obj.y)(
      obj.y := 0, 
      obj.velocity := Vec2(0, 0)
    )
  }
  
  val r2 = foreach(moving_objects, cat2, score.category) { (o1, o2, score) =>
    whenever(Collision(o1, o2)) (
      score.value += 1
    )
  }
  
  val r3 = foreach(moving_objects2) { obj =>
    whenever(IsFingerDownOver(obj)) {
      obj.radius += 0.1
    } 
  }
  
  val cell = arr.cells(1)(1)
  
  val r4 = whenever(Contains(cell, rect1))(
      (rect1.x, rect1.y) := (cell.x, cell.y),
      rect1.velocity := Vec2(0, 0)
    )

  register(r1)
  register(r2)
  register(r3)
  register(r4)

}
