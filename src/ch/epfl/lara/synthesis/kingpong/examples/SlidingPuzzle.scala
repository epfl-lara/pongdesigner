package ch.epfl.lara.synthesis.kingpong.examples

import org.jbox2d.dynamics.BodyType
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.PhysicalWorld
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.ColorConstants._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._

class SlidingPuzzle extends Game {
  val world = new PhysicalWorld(Vec2(0, 0))

  val arrays = Category("Arrays")()
  val pieces = Category("Pieces")(width = 0.9, height = 0.9, restitution = 0.1, linearDamping = 2)
  val borders = Category("Borders")(tpe = BodyType.STATIC, color = red)
  val wins = Category("Wins")()
  
  val gameboard = array(arrays)("MyArray", x = 0, y = 0, columns = 3, rows = 3)
  val win = booleanbox(wins)("win", x = -1, y = -3, value = false)
  
  // Pieces
  val piecesObj = for (i <- 0 until 8) yield {
    val col = i % 3
    val row = i / 3
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
      ApplyForce(piece, (move._2 - move._1) * 80)

      //piece.velocity := (move._2 - move._1) * 15
//      piece.x += move._2._1 - move._1._1,
//      piece.y += move._2._2 - move._1._2
    )}
  }
  
  // Snapping pieces rule
  /*val r2 = foreach(gameboard.cellsCategory, pieces) { (cell, piece) => 
    whenever(Contains(cell, piece) && !isFingerMoveOver(piece)) (
        let("force",  (cell.center - piece.center)) { force => 
        let("norm2", call("norm2", force)) { norm2 =>
          ApplyForce(piece,  force * 10 * 0.01 / (norm2 + 0.01))
        } }
      //piece.velocity := (cell.center - piece.center) * 4
    )
  }*/
  
  // Snap the cells to the center of the cells.
  val r2bix = foreach(gameboard.cellsCategory, pieces) { (cell, piece) =>
    whenever((ContainsTotally(cell, piece) || (call("norm", piece.velocity) =:= 0 && Contains(cell, piece))) && !isFingerMoveOver(piece)) { Seq(
      piece.x := cell.x,
      piece.y := cell.y
    )}
  }
  
  // Snap the speed to the axis.
  val r2ter = foreach(pieces) { piece =>
    let("vx", call("abs", TupleSelect(piece.velocity, 1))) { vx =>
    let("vy", call("abs", TupleSelect(piece.velocity, 2))) { vy =>
      Seq(whenever(vx > vy) {
        piece.velocity *= Tuple(Seq(1, 0))
      } otherwise whenever(vy > vx) {
        piece.velocity *= Tuple(Seq(0, 1))
      })
    }
    }
  }
  
  // Winning condition rule
  val r3 = If(
    Contains(gameboard.cells(0)(0), piecesObj(0)) &&
    Contains(gameboard.cells(1)(0), piecesObj(1)) &&
    Contains(gameboard.cells(2)(0), piecesObj(2)) &&
    Contains(gameboard.cells(0)(1), piecesObj(3)) &&
    Contains(gameboard.cells(1)(1), piecesObj(4)) &&
    Contains(gameboard.cells(2)(1), piecesObj(5)) &&
    Contains(gameboard.cells(0)(2), piecesObj(6)) &&
    Contains(gameboard.cells(1)(2), piecesObj(7)),
    win.value := true,
    win.value := false
  )
 
  register(r1)
  //register(r2)
  register(r2bix)
  register(r2ter)
  register(r3)

}
