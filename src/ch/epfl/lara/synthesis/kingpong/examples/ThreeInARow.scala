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

class ThreeInARow extends Game {
  val world = new PhysicalWorld(Vec2(0, 0))

  val arrays = Category("Arrays")()
  val pieces = Category("Pieces")(width = 0.9, height = 0.9)
  val borders = Category("Borders")(tpe = BodyType.STATIC, color = red)
  val counters = Category("Counters")()
  
  val gameboard = array(arrays)("MyArray", x = 0, y = 0, columns = 6, rows = 10)
  
  // First piece
  rectangle(pieces)(
    name = "piece", 
    x = gameboard.cells(gameboard.numColumns.get/2)(0).x, 
    y = gameboard.cells(gameboard.numColumns.get/2)(0).y,
    velocity = Tuple(0f, 3f),
    color = blue,
    restitution = 0,
    friction = 0)
    
  // Borders
  val right  = rectangle(borders)("right", x = gameboard.right + 0.5, y = gameboard.y, width = 1, height = gameboard.height, friction = 0)
  val left   = rectangle(borders)("left", x = gameboard.left - 0.5, y = gameboard.y, width = 1, height = gameboard.height, friction = 0)
  val bottom = rectangle(borders)("bottom", x = gameboard.x, y = gameboard.bottom + 0.5, width = gameboard.width + 2, height = 1, restitution = 0)
  
  // Counter
  val counter = intbox(counters)("pieces", 
      x = gameboard.left,
      y = gameboard.top - 0.5,
      value = 1)
  
  // Moving pieces rule
  val r1 = foreach(pieces) { piece =>
    fingerMoveOver(piece) { move => Seq(
      piece.velocity := Tuple((move._2._1 - move._1._1) * 15, piece.velocity._2)
    )}
  }
   
  // Snapping pieces rule
  val r2 = foreach(gameboard.cellsCategory, pieces) { (cell, piece) => 
    whenever(Contains(cell, piece) &&
             ((Row(cell) =:= (gameboard.numRows - 1)) || 
              (!forall(pieces)(p => !Contains(Apply(gameboard, Column(cell), Row(cell) + 1), p))
             ))) (
      piece.velocity := (cell.center - piece.center) * 10
    )
  }
  
  // Spawning piece rule
  val r3 = let("piece", find(pieces)(_.name =:= "piece")) { piece =>
    //TODO this test should be an expression "IsNull" or "NotNull"
    whenever(piece =!= ObjectLiteral(null)) (
      foreach(gameboard.cellsCategory) { cell =>
        whenever(Contains(cell, piece) &&
                 ((Row(cell) =:= (gameboard.numRows - 1)) || 
                  (!forall(pieces)(p => !Contains(Apply(gameboard, Column(cell), Row(cell) + 1), p))
                 ))) (
          piece.name := "still",
          counter.value += 1,
          copy(piece) { clone => Seq(
            clone.name := "piece",
            clone.x := gameboard.cell(gameboard.numColumns.get / 2, 0).x, 
            clone.y := gameboard.cell(gameboard.numColumns.get / 2, 0).y,
            clone.velocity := Tuple(0f, 3f)
          )}
        )
      }
    )
  }
 
  register(r1)
  register(r2)
  register(r3)

}
