package ch.epfl.lara.synthesis.kingpong.examples

import org.jbox2d.dynamics.BodyType
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.PhysicalWorld
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._
import android.graphics.Color

class ProofConceptGame extends Game {
  val world = new PhysicalWorld(Vec2(0, 3.5f))

  val array_objects = Category("falling_objects")()
  //val moving_objects2 = Category("Moving_objects2")()

  val rect1 = rectangle(array_objects)(name="Rect1", x=2, y= -2, width = 1, height = 1, fixedRotation = true)
  val rect2 = rectangle(array_objects)(name="Rect2", x=1, y= -3, width = 1, height = 1, fixedRotation = true)
  val rect3 = rectangle(array_objects)(name="Rect3", x=0, y= -4, width = 1, height = 1, fixedRotation = true)
  val rect4 = rectangle(array_objects)(name="Rect4", x= -1, y= -5, width = 1, height = 1, fixedRotation = true)
  val rect5 = rectangle(array_objects)(name="Rect5", x= -2, y= -6, width = 1, height = 1, fixedRotation = true)
  
  val cat2 = Category("Static objects")()
  val catArray = Category("Array objects")()

  val base = rectangle(cat2)("Base", 0, 8, width = 20, height = 0.5, tpe = BodyType.STATIC)
  val base2 = rectangle(cat2)("Base2", 0, -8.5, width = 20, height = 0.5, tpe = BodyType.STATIC)
  val mushroom = rectangle(cat2)("mushroom", 0, 1, width = 1, height = 1, color = Color.RED, fixedRotation = true)

  val arr = array(catArray)("MyArray", 0, 0, 5, 1)
  
  val scores = Category("scores")()
  val score = intbox(scores)("Collisions", -10, 2, height=1, value = 0)
  
  val score2 = intbox(scores)("Touches", -10, 3, height=1, value = 0)
  
  val label = stringbox(scores)("instructions", -10, -7, "Press on the falling objects!")
  
  //val soundtts = soundTTS(cat2)(name="soundtts",x= -10.5f, y=4f, time= 1, language="en", text="Welcome to Pong Designer, made by Lara at E P F L !")
  //val soundtts2 = soundTTS(cat2)(name="soundtts2",x= -10.5f, y=5f, time= 150, language="fr", text="Bienvenue à Pongue Dizaïneur réalisé par Lara, à l'eupéheffelle !")

  val cell = arr.cells(4)(0)
  
  // Rule allowing the objects to continue to fall down. The score.value is absolute and needs to be corrected relatively.
  // e.g. by selecting the fix button, select the score change, and then by selecting the right statement.
  val r1 = foreach(array_objects) { b =>
    whenever(Collision(b, base))(
        b.y := -6,
        (score.value := 1) orElse (score.value := score.value + 1),
        b.velocity := Vec2(0, 0)
    )
  }
  
  // Rule to be corrected by providing a new example (e.g. selecting a collision, augmenting the score to 2 and accepting.)
  val r2 = foreach(array_objects) { b =>
    whenever(isFingerDownOver(b) && b.color =:= Color.BLACK)(
        (score2.value := 1) orElse (score2.value := score2.value + 1),
        b.color := Color.GREEN
    )
  }
  
  // Rule to generalize so that all objects snap to the array.
  val r3 = foreach(cell.category) { cell =>
    whenever(Contains(cell, rect1))(
      (rect1.x, rect1.y) := (cell.x, cell.y),
      rect1.velocity := Vec2(0, 0)
    )
  }
  
  // A growing mushroom if it hits the floor. Then pops after growth.
  val r4 = iif(mushroom.top < 0)(
    mushroom.height := 1,
	  mushroom.width := 1
  ) Else (
    whenever(Colliding(mushroom, base) && mushroom.bottom > base.top - 1 )(
	    let("t", mushroom.top) { t =>
	      let("yh", choose(mushroom.y, mushroom.height)(mushroom.bottom =:= base.top && mushroom.top =:= t - 0.05)){ yh =>
	        Seq(mushroom.y := yh._1,
	        mushroom.height := yh._2,
	        mushroom.width := integerLiteralOne / mushroom.height,
	        mushroom.velocity := Vec2(0, -0.01f))
	      }
	    }
    )
  )
    
  
  register(r1)
  register(r2)
  register(r3)
  register(r4)
}
