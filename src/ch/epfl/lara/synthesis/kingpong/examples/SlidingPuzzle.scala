package ch.epfl.lara.synthesis.kingpong.examples

import org.jbox2d.dynamics.BodyType
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.PhysicalWorld
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.ColorConstants._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._

class SlidingPuzzle extends Game {
  val world = new PhysicalWorld(Vec2(0, 0))

  val arrays = Category("Arrays")()
  val pieces = Category("Pieces")(width = 0.9, height = 0.9)
  val borders = Category("Borders")(tpe = BodyType.STATIC, color = red)
  val wins = Category("Wins")()
  
  val gameboard = array(arrays)("MyArray", x = 0, y = 0, columns = 3, rows = 3)
  val win = intbox(wins)("win", x = -1, y = -3, visible = false)
  
  // Pieces
  for (col <- 0 until 3; row <- 0 until 3 if !(col == 0 && row == 0)) {
    rectangle(pieces)(
      name = "piece[" + col + "," + row + "]", 
      x = gameboard.cells(col)(row).x, 
      y = gameboard.cells(col)(row).y,
      color = colorful(col))
  }
  
  // Borders
  val top    = rectangle(borders)("top", x = 0.25, y = -1.75, width = 3.5, height = 0.5)
  val right  = rectangle(borders)("right", x = 1.75, y = 0.25, width = 0.5, height = 3.5)
  val bottom = rectangle(borders)("bottom", x = -0.25, y = 1.75, width = 3.5, height = 0.5)
  val left   = rectangle(borders)("left", x = -1.75, y = -0.25, width = 0.5, height = 3.5)

  
  // Moving pieces rule
  val r1 = foreach(pieces) { piece =>
    fingerMoveOver(piece) { move => Seq(
      //debug("Move by %s.", move.expr),
      piece.velocity := (move._2 - move._1) * 10,
      piece.x += move._2._1 - move._1._1,
      piece.y += move._2._2 - move._1._2
    )}
  }
  
  // Snapping pieces rule
  val r2 = foreach(gameboard.cellsCategory, pieces) { (cell, piece) => 
    whenever(Contains(cell, piece) && !isFingerMoveOver(piece)) (
      piece.velocity := (cell.center - piece.center) * 4
    )
  }
  
  //TODO Winning condition rule
  val r3 = If(forall(gameboard.cellsCategory) { cell =>
    If(Row(cell) =:= 0 && Column(cell) =:= 0, 
        true, 
        false)
  },
    win.visible := true,
    win.visible := false
  )
 
  register(r1)
  register(r2)
  register(r3)

}
