package ch.epfl.lara.synthesis.kingpong.test

import org.scalatest._

import org.jbox2d.dynamics.BodyType

import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.expression.TreeOps._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.rules.EmptyContext
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.PhysicalWorld

class PrettyPrinterSuite extends FlatSpec with Matchers {

  val i1 = IntegerLiteral(1)
  val i2 = IntegerLiteral(2)
  val i3 = IntegerLiteral(3)
  val i4 = IntegerLiteral(4)
  
  val interpreter = new Interpreter {
    def initGC() = new EmptyContext {}
    def initRC() = RecContext(Map.empty)
  }

  
  "PrettyPrinter" should "print" in {
    val e = Block(If(i1 < i2, 2, 3))
    val out = PrettyPrinterExtended.print(List(e)).c.toString
    //TODO test
//    println(out)
  }
  
  it should "print foreach" in {
    val g = new Game() {
      val world = new PhysicalWorld(Vec2(0, 0f))
      val cat = Category("Cat")()
      val e = foreach(cat) { obj =>
        obj.x += 2
      }
    }
    
    val out = PrettyPrinterExtended.print(List(g.e)).c.toString
    //TODO test
//    println(out)
  }
  
}