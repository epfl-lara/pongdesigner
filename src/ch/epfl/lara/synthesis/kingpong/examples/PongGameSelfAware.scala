package ch.epfl.lara.synthesis.kingpong.examples

import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.ast._
/**
 * Simplest version of Pong game - where the score is to be added.
 */

class PongGameSelfAware extends Game {
  import GameShapes._

  /**
   * Game static values
   */
  layoutWidth = 480
  layoutHeight = 750

  /** Game Layouts */
  var arena1 = Arena() named "arena1"
  val paddle1 = Rectangle(200, 10, 100, 40) named "paddle1" initialize(EConstant(200), EConstant(10), EConstant(100), EConstant(40))
  paddle1.noVelocity = true
  arena1 += paddle1
  val paddle2 = Rectangle(200, layoutHeight-50, 100, 40) named "paddle2" initialize(EConstant(200), ELayoutHeight() + EConstant(50), EConstant(100), EConstant(40))
  paddle2.noVelocity = true
  arena1 += paddle2
  val wall1 = Rectangle.fromBounds(0, 50, 20, layoutHeight-50) named "wall1" initialize(EConstant(0), EConstant(50), EConstant(20), ELayoutHeight() - EConstant(100))  
  wall1.noVelocity = true
  arena1 += wall1
  val wall2 = Rectangle.fromBounds(460, 50, 480, layoutHeight-50) named "wall2" initialize(EConstant(460), EConstant(50), EConstant(20), ELayoutHeight() - EConstant(100))
  wall2.noVelocity = true
  arena1 += wall2
  val ball = Circle(layoutWidth/2, layoutHeight/2 + 20, 30) named "ball" initialize(ELayoutWidth() / EConstant(2), ELayoutHeight() / EConstant(2) + EConstant(20), EConstant(30))
  ball.noVelocity = false
  ball.velocity_x = 0.1f
  ball.velocity_y = 0.0f
  arena1 += ball
  val scorePlayer1 = IntegerBox(25, 310, 40, 35, 0) named "scorePlayer1" initialize(EConstant(25), EConstant(310), EConstant(40), EConstant(35), EConstant(0))
  arena1 += scorePlayer1
  val scorePlayer2 = IntegerBox(25, 450, 40, 35, 0) named "scorePlayer2" initialize(EConstant(25), EConstant(450), EConstant(40), EConstant(35), EConstant(0))
  arena1 += scorePlayer2
  
  /**
   * Scene selection
   */
  setCurrentArena(arena1)
    init_actions += EApply(ESelect(ETop(), "setCurrentArena"), List(EIdent("arena1")))
  
  /**
   * Rules to be guessed.
   */
  WhenEver(ball.y - ball.radius > layoutHeight) {
    ball.x = layoutWidth / 2
    ball.y = layoutHeight / 2
    scorePlayer1 += 1
  }
    init_rules += WhenEverRule(EApply(ESelect(EApply(ESelect(ESelect(EIdent("ball"), "y"), "$minus"), List(ESelect(EIdent("ball"), "radius"))), "$greater"), List(EIdent("layoutWidth"))),
        List(EApply(ESelect(EIdent("ball"), "x_$eq"), List(EApply(ESelect(EIdent("layoutWidth"), "$div"), List(EConstant(2))))),
             EApply(ESelect(EIdent("ball"), "y_$eq"), List(EApply(ESelect(EIdent("layoutHeight"), "$div"), List(EConstant(2))))),
             EApply(ESelect(EIdent("scorePlayer1"), "$plus$eq"), List(EConstant(1)))
             )
    )
  WhenFingerMovesOn(ball) { (xFrom, yFrom, xTo, yTo) =>
    if(xFrom != xTo || yFrom != yTo) {
      val v = ball.velocity
      ball.velocity_x = xTo - xFrom
      ball.velocity_y = yTo - yFrom
      ball.velocity = v
    }
  }
    init_rules += WhenFingerMovesOnRule(
        EIdent("ball"),
        List(EIdent("xFrom"), EIdent("yFrom"), EIdent("xTo"), EIdent("yTo")),
        List(IfCode(
            EApply(ESelect(EApply(ESelect(EIdent("xFrom"), "$bang$eq"), List(EIdent("xTo"))), "$bar$bar"), List(EApply(ESelect(EIdent("yFrom"), "$bang$eq"), List(EIdent("yTo"))))),
            List(ValDefCode(EIdent("v"), ESelect(EIdent("ball"), "velocity")),
                 EApply(ESelect(EIdent("ball"), "velocity_x_$eq"), List(EApply(ESelect(EIdent("xTo"), "$minus"), List(EIdent("xFrom"))))),
                 EApply(ESelect(EIdent("ball"), "velocity_y_$eq"), List(EApply(ESelect(EIdent("yTo"), "$minus"), List(EIdent("yFrom"))))),
                 EApply(ESelect(EIdent("ball"), "velocity_$eq"), List(EIdent("v")))
            ),
            List()
            ))
    )
  WhenFingerMovesOn(paddle1) { (xFrom, yFrom, xTo, yTo) =>
    paddle1.x += xTo - xFrom
  }
  WhenFingerMovesOn(paddle2) { (xFrom, yFrom, xTo, yTo) =>
    paddle2.x += xTo - xFrom
  }
  init_rules += WhenFingerMovesOnRule(
    EIdent("paddle2"),
    List(EIdent("xFrom"), EIdent("yFrom"), EIdent("xTo"), EIdent("yTo")),
    List(EApply(ESelect(ESelect(EIdent("paddle2"), "x"), "$plus$eq"), List(EApply(ESelect(EIdent("xTo"), "$minus"),List(EIdent("xFrom"))))))
   )
    WhenEver (ball.y + ball.radius < 0) {
      ball.x = layoutWidth / 2
      ball.y = layoutHeight / 2
      scorePlayer2 += 1
    }
    val new_rule = WhenEverRule(EApply(ESelect(EApply(ESelect(ESelect(EIdent("ball"), "y"), "$plus"), List(ESelect(EIdent("ball"), "radius"))), "$less"), List(EConstant(0))),
        List(EApply(ESelect(EIdent("ball"), "x_$eq"), List(EApply(ESelect(EIdent("layoutWidth"), "$div"), List(EConstant(2))))),
             EApply(ESelect(EIdent("ball"), "y_$eq"), List(EApply(ESelect(EIdent("layoutHeight"), "$div"), List(EConstant(2))))),
             EApply(ESelect(EIdent("scorePlayer2"), "$plus$eq"), List(EConstant(1)))
             )
    )
    init_rules += new_rule
  //added_whenever_rules += new_rule
  
  WhenEver(paddle1.x < 0) {
    paddle1.x = 0
  }
    init_rules += WhenEverRule(EApply(ESelect(ESelect(EIdent("paddle1"), "x"), "$less"), List(EConstant(0))),
        List(EApply(ESelect(EIdent("paddle1"), "x_$eq"), List(EConstant(0))))    
    )
  WhenEver(paddle1.x + paddle1.width > layoutWidth) {
    paddle1.x = layoutWidth - paddle1.width
  }
  WhenEver(paddle2.x < 0) {
    paddle2.x = 0
  }
  WhenEver(paddle2.x + paddle2.width > layoutWidth) {
    paddle2.x = layoutWidth - paddle2.width
  }
  Camera.x = 0
  Camera.y = 0
  Camera.width = layoutWidth
  Camera.height = layoutHeight
}