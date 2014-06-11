package ch.epfl.lara.synthesis.kingpong.examples

import ch.epfl.lara.synthesis.kingpong.{Game, PhysicalWorld}
import ch.epfl.lara.synthesis.kingpong.common.ColorConstants._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._
import org.jbox2d.dynamics.BodyType

class BalancedParentheses extends Game {
  val world = new PhysicalWorld(Vec2(0, 0))

  val arrays = Category("arrays")()
  val labels = Category("labels")()
  val buttons = Category("buttons")(radius = 0.5, color = green, tpe = BodyType.STATIC)
  val values = Category("values")()

  val arr = array(arrays)("Array", 0, 0, 6, 1)
  val ptr = intbox(labels)("Pointer", arr.left, arr.top - 0.5, value = 0)
  val btn = circle(buttons)("Button", arr.left - 1, arr.y)


  /* state:
   * 0  = finding closing parenthesis on the right
   * 1  = finding opening parenthesis on the left
   * 2  = success, writing success on the last cell
   * -1 = error state, writing error on the first cell
   */
  val state = intbox(labels)("State", arr.left, arr.bottom + 0.5, value = 0)


  val entries = List(-2, 0, 1, 0, 1, -1)

  for((e, i) <- entries.zipWithIndex) {
    intbox(values)(" ", arr.cell(i, 0).x, arr.cell(i, 0).y, value = e)
  }

  val stateFindClosing = whenever(isFingerDownOver(btn) && state.value =:= 0) (
    let("cell", arr.cell(ptr.value, 0)) { cell =>
      let("value", find(values)(_ in cell)) { value =>
        whenever(notNull(value)) (
          whenever(value.value =:= 1 ) (
            value.value := 2,
            state.value := 1
          ) otherwise (
            whenever(value.value =:= 0 || value.value =:= 2 || value.value =:= -2) (
              ptr.value += 1
            ) otherwise (
              state.value := 2
            )
          )
        )
      }
    }
  )

  val stateFindOpening = whenever(isFingerDownOver(btn) && state.value =:= 1) (
    let("cell", arr.cell(ptr.value, 0)) { cell =>
      let("value", find(values)(_ in cell)) { value =>
        whenever(notNull(value) && value.value =:= 0 ) (
          value.value := 2,
          state.value := 0
        ) otherwise (
          whenever(value.value =:= 2) (
            ptr.value -= 1
          ) otherwise (
            state.value := -1
          )
        )
      }
    }
  )

  val stateSuccess = whenever(isFingerDownOver(btn) && state.value =:= 2) (
    let("cell", arr.cell(ptr.value, 0)) { cell =>
      let("value", find(values)(_ in cell)) { value =>
        value.color := green
      }
    }
  )

  val stateError = whenever(isFingerDownOver(btn) && state.value =:= -1) (
    let("cell", arr.cell(ptr.value, 0)) { cell =>
      let("value", find(values)(_ in cell)) { value =>
        value.color := red
      }
    }
  )

  register(stateFindClosing)
  register(stateFindOpening)
  register(stateSuccess)
  register(stateError)

}
