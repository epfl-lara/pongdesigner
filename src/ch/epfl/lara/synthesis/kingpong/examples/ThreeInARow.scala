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
  val world = new PhysicalWorld(Vec2(0, 3))

  val arrays = Category("Arrays")()
  val pieces = Category("Pieces")(width = 0.9, height = 0.9)
  val borders = Category("Borders")(tpe = BodyType.STATIC, color = red, friction = 0)
  val counters = Category("Counters")()
  val randoms = Category("Randoms")()
  
  val gameboard = array(arrays)("MyArray", x = 0, y = 0, columns = 6, rows = 10)
  val random = randomGenerator(randoms)("random", x = gameboard.right + 2, y = gameboard.y, minValue = 0, maxValue = gameboard.numColumns-1)
  
  // First piece
  rectangle(pieces)(
    name = "piece", 
    x = gameboard.cells(gameboard.numColumns.get/2)(0).x, 
    y = gameboard.cells(gameboard.numColumns.get/2)(0).y,
    color = blue,
    restitution = 0,
    friction = 0)
    
  // Borders
  rectangle(borders)("right", x = gameboard.right + 0.5, y = gameboard.y, width = 1, height = gameboard.height)
  rectangle(borders)("left", x = gameboard.left - 0.5, y = gameboard.y, width = 1, height = gameboard.height)
  rectangle(borders)("bottom", x = gameboard.x, y = gameboard.bottom + 0.5, width = gameboard.width + 2, height = 1)
  
  // Counter
  val counter = intbox(counters)("score", 
      x = gameboard.left,
      y = gameboard.top - 0.5,
      value = 0)
  
  // Moving pieces rule
  val r1 = foreach(pieces) { piece =>
    fingerMoveOver(piece) { move => Seq(
      piece.velocity := Tuple((move._2._1 - move._1._1) * 15, piece.velocity._2)
    )}
  }
   
  // Snapping pieces rule
  val r2 = foreach(pieces) { piece =>
    let("cell", ContainingCell(gameboard, piece)) { cell =>
      whenever(notNull(cell) && 
               ((Row(cell) =:= (gameboard.numRows - 1)) || 
                (!forall(pieces)(p => !Contains(Apply(gameboard, Column(cell), Row(cell) + 1), p))
               ))) (
        piece.velocity := (cell.center - piece.center) * 10
      )
    }
  }
  
  // Spawning piece rule
  val r3 = let("piece", find(pieces)(_.name =:= "piece")) { piece =>
    let("cell", ContainingCell(gameboard, piece)) { cell =>
      whenever(notNull(cell) && notNull(piece) &&
               ((Row(cell) =:= (gameboard.numRows - 1)) || 
                (!forall(pieces)(p => !Contains(Apply(gameboard, Column(cell), Row(cell) + 1), p))
               ))) (
        piece.name := "still",
        copy(piece) { clone => Seq(
          clone.name := "piece",
          let("col", random.value) { col => Seq(
            clone.x := gameboard.cell(col, 0).x, 
            clone.y := gameboard.cell(col, 0).y
          )},
          clone.color := If(random.value % 3 =:= 0, blue, If(random.value % 3 =:= 1, green, purple))
        )},
        
        // Removing pieces rule
        foreach(pieces) { centerPiece =>
          let("centerCell", ContainingCell(gameboard, centerPiece)) { centerCell =>
            let("leftCell", "rightCell", 
                gameboard.cell(Column(centerCell) - 1, Row(centerCell)), gameboard.cell(Column(centerCell) + 1, Row(centerCell))) { (leftCell, rightCell) =>
              let("leftPiece", "rightPiece", 
                  find(pieces)(Contains(leftCell, _)), find(pieces)(Contains(rightCell, _))) { (leftPiece, rightPiece) =>
                whenever(notNull(leftPiece) && notNull(rightPiece) && 
                         leftPiece.color =:= centerPiece.color && rightPiece.color =:= centerPiece.color) (
                  counter.value += 1,
                  Delete(leftPiece),
                  Delete(centerPiece),
                  Delete(rightPiece)
                )
              }
            }
          }
        }
      )
    }
  }
  
  
  
//  // Removing pieces rule
//  val r4 = foreach(gameboard.cellsCategory) { cell =>
//    whenever(Column(cell) > 0 && 
//            Column(cell) < gameboard.numColumns - 1) (
//                
//      let("centerPiece", find(pieces)(Contains(cell, _))) { centerPiece =>
//        whenever(notNull(centerPiece)) (
//          let("leftCell", "rightCell", 
//              gameboard.cell(Column(cell) - 1, Row(cell)), gameboard.cell(Column(cell) + 1, Row(cell))) { (leftCell, rightCell) =>
//            let("leftPiece", "rightPiece", 
//                find(pieces)(Contains(leftCell, _)), find(pieces)(Contains(rightCell, _))) { (leftPiece, rightPiece) =>
//              whenever(notNull(leftPiece) && notNull(rightPiece) && 
//                       leftPiece.color =:= centerPiece.color && rightPiece.color =:= centerPiece.color) (
//                counter.value += 1,
//                Delete(leftPiece),
//                Delete(centerPiece),
//                Delete(rightPiece)
//              )
//            }
//          }
//        )
//      }
//    )
//  }
    
  register(r1)
  register(r2)
  register(r3)
//  register(r4)

}
