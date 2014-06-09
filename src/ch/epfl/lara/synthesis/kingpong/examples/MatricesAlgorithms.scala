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
  val buttons = Category("buttons")(radius = 0.5, color = green, tpe = BodyType.STATIC)
  val values = Category("values")()

  val arr = array(arrays)("Array", 0, 0, 5, 2)
  val ptr = intbox(labels)("Pointer", arr.left, arr.top - 0.5, value = 0)
  val btn = circle(buttons)("Button", arr.left - 1, arr.y)

  val crtMax = intbox(labels)("Maximum", arr.right + 0.5, arr.cell(0, 0).y, value = 0)
  val toFind = intbox(labels)("To find", arr.left, arr.bottom + 1, value = 2)
  val crtFound = intbox(labels)("Found index", arr.right + 0.5, arr.cell(0, 1).y, value = -1)

  for(i <- 0 until arr.numColumns.get) {
    intbox(values)(" ", arr.cells(i)(0).x, arr.cells(i)(0).y, value = i%4)
    intbox(values)(" ", arr.cells(i)(1).x, arr.cells(i)(1).y, value = 4-i%4)
  }

  val advancePointerRule = whenever(isFingerDownOver(btn) && ptr.value < arr.numColumns) (
    ptr.value += 1
  )

  val findMaxRule = whenever(isFingerDownOver(btn)) (
    let("cell", Apply(arr, ptr.value, 0)) { cell =>
      let("value", find(values) {Contains(cell, _)}) { value =>
        whenever(notNull(value) && value.value > crtMax.value) (
          crtMax.value := value.value
        )
      }
    }
  )

  val searchRule = whenever(isFingerDownOver(btn)) (
    let("cell", Apply(arr, ptr.value, 1)) { cell =>
      let("value", find(values) {Contains(cell, _)}) { value =>
        whenever(notNull(value) && value.value =:= toFind.value) (
          crtFound.value := ptr.value
        )
      }
    }
  )

  register(advancePointerRule)
  register(findMaxRule)
  register(searchRule)

}
