package ch.epfl.lara.synthesis.kingpong.examples

import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.ast._

class PongFibonacci extends Game {
  /**
   * Game static values
   */
  layoutWidth = 480
  layoutHeight = 750

  /**
   * Game Layouts
   */
  val arena1 = Arena() named "arena1"
  val wall = Rectangle(0, 0, 25, 750) named "wall"
  wall.noVelocity = true
  arena1 += wall
  val wall1 = Rectangle(455, 0, 25, 750) named "wall1"
  wall1.noVelocity = true
  arena1 += wall1
  val wall2 = Rectangle(25, 0, 430, 25) named "wall2"
  wall2.noVelocity = true
  arena1 += wall2
  val wall3 = Rectangle(25, 725, 430, 25) named "wall3"
  wall3.noVelocity = true
  arena1 += wall3
  val wall4 = Rectangle(26.339f, 202.575f, 422, 30) named "wall4"
  wall4.noVelocity = true
  arena1 += wall4
  val ball = Circle(219.588f, 115.068f, 50) named "ball"
  ball.velocity_x = 0.25f
  ball.velocity_y = 0
  arena1 += ball
  val score = IntegerBox(100, 385, 60, 60, 1) named "score"
  arena1 += score
  val score1 = IntegerBox(95, 510, 60, 60, 2) named "score1"
  arena1 += score1
  val score2 = IntegerBox(285, 540, 145, 155, 0) named "score2"
  arena1 += score2
  
  WhenCollisionBetween(ball, wall1) {
    score2.value = score.value + score1.value
  }.represents(List(EApply(ESelect(ESelect(EIdentShape(score2), "value"), "$eq"), List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus"), List(ESelect(EIdentShape(score1), "value")))))))
  WhenCollisionBetween(ball, wall) {
    score.value = score1.value
  }.represents(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"), List(ESelect(EIdentShape(score1), "value")))))
  WhenIntegerChanges(score) { (newValue) =>
    score1.value = score2.value
  }.represents(List(EApply(ESelect(ESelect(EIdentShape(score1), "value"), "$eq"), List(ESelect(EIdentShape(score2), "value")))))
  Camera.x = 0
  Camera.y = 0
  Camera.width = layoutWidth
  Camera.height = layoutHeight
}