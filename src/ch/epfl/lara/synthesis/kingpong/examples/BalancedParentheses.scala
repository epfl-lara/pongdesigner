package ch.epfl.lara.synthesis.kingpong.examples

import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.{R, Game, PhysicalWorld}
import ch.epfl.lara.synthesis.kingpong.common.ColorConstants._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._

class BalancedParentheses extends Game {
  val world = new PhysicalWorld(Vec2(0, 0))

  val RAD_OPEN  = (Math.PI/2).toFloat
  val RAD_CLOSE = (Math.PI*3/2).toFloat

  val START = (e: Proxy) => e.color =:= R.drawable.bm_trashcan
  val END   = (e: Proxy) => e.color =:= R.drawable.bm_gear
  val OPEN  = (e: Proxy) => (e.color =:= R.drawable.bm_banana) && (e.angle =:= RAD_OPEN)
  val CLOSE = (e: Proxy) => (e.color =:= R.drawable.bm_banana) && (e.angle =:= RAD_CLOSE)
  val EMPTY = (e: Proxy) => e.color =:= R.drawable.bm_bing

  val arrays = Category("arrays")()
  val labels = Category("labels")()
  val values = Category("values")(color = R.drawable.bm_banana, width = 0.8, height = 0.6)

  val arr = array(arrays)("Array", 0, 0, 6, 1, cellHeight = 2)
  val ptr = rectangle(labels)("Pointer", arr.cell(0, 0).x, arr.cell(0, 0).top + 0.5, width = 0.7, height = 0.7,
    velocity = Vec2(1f, 0f), color = R.drawable.bm_arrow_down)

  /* state:
   * 0  = finding closing parenthesis on the right
   * 1  = finding opening parenthesis on the left
   * 2  = success, writing success on the last cell
   * -1 = error state, writing error on the first cell
   */
  val state = intbox(labels)("State", arr.left, arr.bottom + 0.5, value = 0)

  val entries = List(0, 0, 1, 1)

  rectangle(values)("start", arr.cell(0, 0).x, arr.cell(0, 0).bottom - 0.5, height = 0.8, color = R.drawable.bm_trashcan)
  for((e, i) <- entries.zipWithIndex.map{case (e, i) => (e, i+1)}) {
    val a = if (e == 0) RAD_OPEN else RAD_CLOSE
    rectangle(values)(s"value $i", arr.cell(i, 0).x, arr.cell(i, 0).bottom - 0.5, angle = a)
  }
  rectangle(values)("end", arr.cell(arr.numColumns.get - 1, 0).x, arr.cell(arr.numColumns.get - 1, 0).bottom - 0.5,
    height = 0.8, color = R.drawable.bm_gear)


  val stateFindClosing = whenever(state.value =:= 0) (
    let("cell", ContainingCell(arr, ptr)) { cell =>
      let("value", find(values)(_ in cell)) { value =>
        whenever(notNull(value)) (
          whenever(CLOSE(value)) (
            value.color := R.drawable.bm_bing,
            value.angle := 0,
            state.value := 1,
            ptr.velocity *= -1
          ) otherwise whenever(!(OPEN(value) || EMPTY(value) || START(value))) (
            state.value := 2,
            ptr.velocity *= 0
          )
        )
      }
    }
  )

  val stateFindOpening = whenever(state.value =:= 1) (
    let("cell", ContainingCell(arr, ptr)) { cell =>
      let("value", find(values)(_ in cell)) { value =>
        whenever(notNull(value)) (
          whenever(OPEN(value)) (
            value.color := R.drawable.bm_bing,
            value.angle := 0,
            state.value := 0,
            ptr.velocity *= -1
          ) otherwise whenever(!EMPTY(value)) (
            state.value := -1,
            ptr.velocity *= 0
          )
        )
      }
    }
  )

  val stateSuccess = whenever(state.value =:= 2) (
    let("cell", ContainingCell(arr, ptr)) { cell =>
      let("value", find(values)(_ in cell)) { value =>
        value.color := green
      }
    }
  )

  val stateError = whenever(state.value =:= -1) (
    let("cell", ContainingCell(arr, ptr)) { cell =>
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
