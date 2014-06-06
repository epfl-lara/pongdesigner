package ch.epfl.lara.synthesis.kingpong.examples

import org.jbox2d.dynamics.BodyType
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.PhysicalWorld
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.ColorConstants._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._

class MatricesAlgorithms extends Game {
  val world = new PhysicalWorld(Vec2(0, 0))

  val arrays = Category("arrays")()
  val labels = Category("labels")()
  val pointers = Category("pointers")()
  val buttons = Category("buttons")(radius = 0.5, color = green, tpe = BodyType.STATIC)
  val valuesMax = Category("values max")()

  val arrayMax = array(arrays)("Array", 0, 0, 5, 1)
  val crtMax = intbox(labels)("Maximum", -3, -1, value = 0)
  val ptrMax = intbox(pointers)("Pointer", -3, 1.5, value = 0)
  val btnMax = circle(buttons)("Button", arrayMax.left - 1, arrayMax.y)

  for(i <- 0 until arrayMax.numColumns.get) {
    intbox(valuesMax)(" ", arrayMax.cells(i)(0).x, arrayMax.cells(i)(0).y, value = i%4)
  }

  val r1 = whenever(isFingerDownOver(btnMax) && ptrMax.value < arrayMax.numColumns)(
    let("cell", Apply(arrayMax, ptrMax.value, 0)) { cell =>
      let("value", find(valuesMax) {Contains(cell, _)}) { value =>
        whenever(notNull(value) && value.value > crtMax.value) (
          crtMax.value := value.value
        )
      }
    },
    ptrMax.value += 1
  )
  
  register(r1)

}
