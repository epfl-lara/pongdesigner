package ch.epfl.lara.synthesis.kingpong.examples

import ch.epfl.lara.synthesis.kingpong.Game

class PongEscape extends Game {
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
  val wall4 = Rectangle(39.932f, 306.691f, 120, 120) named "wall4"
  wall4.velocity_x = 0.0f
  wall4.velocity_y = 0.228f
  arena1 += wall4
  val wall8 = Rectangle(171.516f, 168.801f, 120, 120) named "wall8"
  wall8.velocity_x = 0.0f
  wall8.velocity_y = 0.228f
  arena1 += wall8
  val wall9 = Rectangle(155.295f, 39.926f, 120, 120) named "wall9"
  wall9.velocity_x = 0.0f
  wall9.velocity_y = 0.228f
  arena1 += wall9
  val wall10 = Rectangle(286.533f, 437.64f, 120, 120) named "wall10"
  wall10.velocity_x = 0.0f
  wall10.velocity_y = 0.228f
  arena1 += wall10
  val ball1 = Circle(115.529f, 685.88f, 30) named "ball1"
  ball1.noVelocity = true
  ball1.color = -19655
  arena1 += ball1
  val textBox = TextBox(88.224f, 340.986f, 240, 60, "") named "textBox"
  textBox.color = -55698
  arena1 += textBox
  val score = IntegerBox(layoutWidth - 50, 60, 10, 30, 0) named "score"
  arena1 += score
  val increment = IntegerBox(layoutWidth + 50, 60, 10, 30, 1) named "increment"
  arena1 += increment
  
  WhenCollisionBetween(wall3, wall4) {
    wall4.x = randomInterval(330.0f, 26.0f) // 1 more
    wall4.y = 28 // 1 more
    wall4.angle = 0 // 1 more
    score.value = score.value + increment.value
  }
  WhenCollisionBetween(wall3, wall8) {
    wall8.x = randomInterval(330.0f, 26.0f) // 1 more
    wall8.y = 28 // 1 more
    wall8.angle = 0 // 1 more
    score.value = score.value + increment.value
  }
  WhenCollisionBetween(wall3, wall9) {
    wall9.x = randomInterval(330.0f, 26.0f) // 1 more
    wall9.y = 28 // 1 more
    wall9.angle = 0 // 1 more
    score.value = score.value + increment.value
  }
  WhenCollisionBetween(wall3, wall10) {
    wall10.x = randomInterval(330.0f, 26.0f) // 1 more
    wall10.y = 28 // 1 more
    wall10.angle = 0 // 1 more
    score.value = score.value + increment.value
  }
  WhenFingerMovesOn(ball1) { (xFrom, yFrom, xTo, yTo) => 
    ball1.x += xTo - xFrom // 2 more
  }
  WhenCollisionBetween(ball1, wall4) {
    ball1.x = -85 // 1 more
    ball1.y = 785 // 1 more
    textBox.text = "Game over"
    increment.value = 0
  }
  WhenCollisionBetween(ball1, wall8) {
    ball1.x = -85 // 1 more
    ball1.y = 785 // 1 more
    textBox.text = "Game over"
    increment.value = 0
  }
  WhenCollisionBetween(ball1, wall9) {
    ball1.x = -85 // 1 more
    ball1.y = 785 // 1 more
    textBox.text = "Game over"
    increment.value = 0
  }
  WhenCollisionBetween(ball1, wall10) {
    ball1.x = -85 // 1 more
    ball1.y = 785 // 1 more
    textBox.text = "Game over"
    increment.value = 0
  }
  WhenCollisionBetween(ball1, wall) {
    ball1.x = 55 // 1 more
    ball1.y = 685 // 1 more
  }
  WhenCollisionBetween(ball1, wall1) {
    ball1.x = 425 // 1 more
    ball1.y = 685 // 1 more
  }
  Camera.x = 0
  Camera.y = 0
  Camera.width = layoutWidth
  Camera.height = layoutHeight
}