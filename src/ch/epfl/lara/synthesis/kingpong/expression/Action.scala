package ch.epfl.lara.synthesis.kingpong.expression
import ch.epfl.lara.synthesis.kingpong.expression.Trees._

sealed trait Action {
  def line: Int
  def column: Int
  def length: Int
  var hovered = false
  var x1 = 0f
  var y1 = 0f
  var x2 = 0f
  var y2 = 0f
  def testHovering(x: Float, y: Float): Boolean = {
    if(x >= x1 && x <= x2 && y >= y1 && y <= y2) {
      hovered = true
    }
    hovered
  }
}

case class ActionModifyConstant(e: Expr, line: Int, column: Int, length: Int) extends Action
case class ActionModifyColor(e: Expr, line: Int, column: Int, length: Int) extends Action
case class ActionChangeParallelCode(e: ParExpr, line: Int, column: Int, length: Int) extends Action {
  var direction = 1
}
case class ActionDeleteLineNumber(representation: String, line: Int, column: Int, length: Int) extends Action
