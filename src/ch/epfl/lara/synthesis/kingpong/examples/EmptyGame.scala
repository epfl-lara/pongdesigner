package ch.epfl.lara.synthesis.kingpong.examples

import org.jbox2d.dynamics.BodyType
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.PhysicalWorld
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._

class EmptyGame extends Game {
  val world = new PhysicalWorld(Vec2(0, 3.5f))

  val cat2 = Category("Static objects")()
  val catArray = Category("Array objects")()

  val base = rectangle(cat2)("Base", 0, 8.25, width = 20, height = 0.5, tpe = BodyType.STATIC)
  val base2 = rectangle(cat2)("Base2", 0, -8.25, width = 20, height = 0.5, tpe = BodyType.STATIC)
}
