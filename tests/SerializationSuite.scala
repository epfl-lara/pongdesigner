package ch.epfl.lara.synthesis.kingpong.test

import org.scalatest._

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.PhysicalWorld
import ch.epfl.lara.synthesis.kingpong.serialization.GameSerializer

class SerializationSuite extends FlatSpec with Matchers {

  val game = new Game() {
    val world = new PhysicalWorld(Vec2(0, 0f))

    val rule1 = If(true, 1, 2)

    register(rule1)

  }

  "GameSerializer" should "TEST" in {

    val json = GameSerializer.exprToJson(game.rule1)
    println(json.toString)
    val expr = GameSerializer.jsonToExpr(json)
    println(expr)

    expr shouldBe game.rule1

  }
}
