package ch.epfl.lara.synthesis.kingpong.test

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong._

class SimpleGameSuite extends FunSuite with BeforeAndAfter {
 
  implicit var game: Game = _

  before {
    game = new Game {
      val world = new PhysicalWorld(Vec2(0, 0))
      
      val cat = new Category("Category 1")
      val c1 = circle("Circle 1", 200, 50, radius = 50).withCategory(cat)
      val c2 = circle("Circle 2", 40, 50, radius = 40).withCategory(cat)
      
      once (c1("x") < 42) { Seq(
        c1("visible") := false,
        c2("angle") := c1("x") + 2
      )}
    }
  }

  test("SimpleGame construction") {
    assert(game.objects.exists(_("name").getPongValue == StringV("Circle 1")))
    assert(game.objects.exists(_("radius").getPongValue == FloatV(40f)))
  }

}
