package ch.epfl.lara.synthesis.kingpong.examples

import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.{R, Game, PhysicalWorld}
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._

class BalancedParentheses extends Game {
  val world = new PhysicalWorld(Vec2(0, 0))

  val RAD_OPEN  = (Math.PI/2).toFloat
  val RAD_CLOSE = (Math.PI*3/2).toFloat

  val EMPTY = (e: Proxy) => (e.color =:= R.drawable.bm_trashcan) && (e.angle =:= 0)
  val START = (e: Proxy) => (e.color =:= R.drawable.bm_bing) && (e.angle =:= 0)
  val OPEN  = (e: Proxy) => (e.color =:= R.drawable.bm_banana) && (e.angle =:= RAD_OPEN)
  val CLOSE = (e: Proxy) => (e.color =:= R.drawable.bm_banana) && (e.angle =:= RAD_CLOSE)

  val arrays = Category("arrays")()
  val labels = Category("labels")()
  val values = Category("values")(color = R.drawable.bm_banana, width = 0.8, height = 0.6, linearDamping = 0.5)
  
  val arr = array(arrays)("array", 0, 0, 10, 1, cellHeight = 2)
  val ptr = rectangle(labels)("ptr", arr.cell(0, 0).x, arr.cell(0, 0).top + 0.5, width = 0.7, height = 0.7,
    velocity = Vec2(2f, 0f), color = R.drawable.bm_arrow_down)

  /* state:
   * 0 = finding closing parenthesis on the right
   * 1 = finding opening parenthesis on the left
   * 2 = going left to the start, error if finding an opening parenthesis
   * 3 = error state, writing error on the first cell
   * 4 = halt
   */
  val state = intbox(labels)("state", arr.left, arr.bottom + 0.5, value = 0)

  val entries = List(0, 0, 1, 0, 0, 1, 1, 1)

  rectangle(values)("start", arr.cell(0, 0).x, arr.cell(0, 0).bottom - 0.5, height = 0.8, color = R.drawable.bm_bing)
  for((e, i) <- entries.zipWithIndex.map{case (e, i) => (e, i+1)}) {
    val a = if (e == 0) RAD_OPEN else RAD_CLOSE
    rectangle(values)(s"value $i", arr.cell(i, 0).x, arr.cell(i, 0).bottom - 0.5, angle = a)
  }

  val stateFindClosing = whenever(state.value =:= 0) (
    let("cell", ContainingCell(arr, ptr)) { cell =>
      let("value", find(values)(_ in cell)) { value =>
        whenever(notNull(value)) (
          ApplyForce(value, ptr.velocity * 0.5f),
          whenever(CLOSE(value) && ptr.x >= cell.center._1) (
            value.color := R.drawable.bm_trashcan,
            value.angle := 0,
            state.value := 1,
            ptr.velocity *= -1
          )
        ) otherwise whenever(ptr.x >= cell.center._1) (
          state.value := 2,
          ptr.velocity *= -1
        )
      }
    }
  )

  val stateFindOpening = whenever(state.value =:= 1) (
    let("cell", ContainingCell(arr, ptr)) { cell =>
      let("value", find(values)(_ in cell)) { value =>
        whenever(notNull(value)) (
          ApplyForce(value, ptr.velocity * 0.5f),
          whenever(ptr.x <= cell.center._1) (
            whenever(OPEN(value)) (
              value.color := R.drawable.bm_trashcan,
              value.angle := 0,
              state.value := 0,
              ptr.velocity *= -1
            ) otherwise whenever(!EMPTY(value)) (
              state.value := 3
            )
          )
        )
      }
    }
  )

  val stateSlidingLeftRule = whenever(state.value =:= 2 || state.value =:= 3) (
    let("cell", ContainingCell(arr, ptr)) { cell =>
      let("value", find(values)(_ in cell)) { value =>
        whenever(notNull(value)) (
          ApplyForce(value, ptr.velocity * 0.5f),
          whenever(ptr.x <= cell.center._1) (
            whenever(OPEN(value)) (
              state.value := 3
            ) otherwise whenever(!EMPTY(value)) (
              let("c", whenever(state.value =:= 2) (R.drawable.bm_ok) otherwise R.drawable.bm_not_ok) { c => Seq(
                value.color := c,
                state.value := 4,
                ptr.velocity *= 0
              )}
            )
          )
        )
      }
    }
  )

  val snappingRule = foreach(values) { value =>
    let("cell", ContainingCell(arr, value)) { cell =>
      whenever(notNull(cell)) (
        ApplyForce(value, (cell.center + Vec2(0, 0.5f) - value.center) * 5f)
      )
    }
  }

  register(stateFindClosing)
  register(stateFindOpening)
  register(stateSlidingLeftRule)
  register(snappingRule)

}
