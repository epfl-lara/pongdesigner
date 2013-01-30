package ch.epfl.lara.synthesis.kingpong

class PongMaze extends Game {
  /**
   * Game static values
   */
  var screenWidth = 480
  var screenHeight = 750

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
  val wall4 = Rectangle(108.858f, 103.776f, 350, 15) named "wall4"
  wall4.noVelocity = true
  arena1 += wall4
  val ball = Circle(400, 60, 25) named "ball"
  ball.noVelocity = true
  arena1 += ball
  val wall41 = Rectangle(15, 205, 350, 15) named "wall41"
  wall41.noVelocity = true
  arena1 += wall41
  val wall411 = Rectangle(345, 210, 15, 145) named "wall411"
  wall411.noVelocity = true
  arena1 += wall411
  val wall412 = Rectangle(115, 445, 350, 15) named "wall412"
  wall412.noVelocity = true
  arena1 += wall412
  val wall4111 = Rectangle(220, 315, 15, 145) named "wall4111"
  wall4111.noVelocity = true
  arena1 += wall4111
  val wall41111 = Rectangle(0, 320, 120, 15) named "wall41111"
  wall41111.noVelocity = true
  arena1 += wall41111
  val wall5 = Rectangle(638.906f, 217.184f, 20, 250) named "wall5"
  wall5.noVelocity = true
  arena1 += wall5
  val wall51 = Rectangle(750, 220, 20, 250) named "wall51"
  wall51.noVelocity = true
  arena1 += wall51
  val wall511 = Rectangle(655, 215, 105, 25) named "wall511"
  wall511.noVelocity = true
  arena1 += wall511
  val wall5111 = Rectangle(650, 460, 105, 25) named "wall5111"
  wall5111.noVelocity = true
  arena1 += wall5111
  val ball1 = Circle(708.59f, 370.424f, 30) named "ball1"
  ball1.velocity_x = 0.0f
  ball1.velocity_y = -0.145f
  arena1 += ball1
  val wall4121 = Rectangle(15, 585, 350, 15) named "wall4121"
  wall4121.noVelocity = true
  arena1 += wall4121
  val ball2 = Circle(85, 665, 55) named "ball2"
  ball2.noVelocity = true
  arena1 += ball2
  val ball3 = Circle(204.621f, 653.978f, 25) named "ball3"
  ball3.color = -15230084
  arena1 += ball3
  val textBox = TextBox(240, 375, 240, 60, "Go") named "textBox"
  textBox.noVelocity = true
  arena1 += textBox
  
  def resetBall() = {
    ball.x = 417 // 1 more
    ball.y = 66 // 1 more
    ball.velocity = 0 // 1 more
  }
  
  WhenCollisionBetween(ball, wall) {
    resetBall()
  }
  WhenCollisionBetween(ball, wall1) {
    resetBall()
  }
  WhenCollisionBetween(ball, wall2) {
    resetBall()
  }
  WhenCollisionBetween(ball, wall3) {
    resetBall()
  }
  WhenCollisionBetween(ball, wall4) {
    resetBall()
  }
  WhenCollisionBetween(ball, wall41) {
    resetBall()
  }
  WhenCollisionBetween(ball, wall411) {
    resetBall()
  }
  WhenCollisionBetween(ball, wall412) {
    resetBall()
  }
  WhenCollisionBetween(ball, wall4111) {
    resetBall()
  }
  WhenCollisionBetween(ball, wall41111) {
    resetBall()
  }
  WhenCollisionBetween(ball1, wall511) {
    wall41111.x = 125 // 1 more
  }
  WhenCollisionBetween(ball1, wall5111) {
    wall41111.x = -5 // 1 more
  }
  WhenCollisionBetween(ball, wall4121) {
    resetBall()
  }
  WhenFingerMovesOn(ball2) { (xFrom, yFrom, xTo, yTo) =>
    ball.x += xTo - xFrom // 3 more
    ball.y += yTo - yFrom // 3 more
  }
  WhenCollisionBetween(ball, ball3) {
    textBox.x = 85 // 1 more
    textBox.y = 495 // 1 more
    textBox.text = "You won !"
  }
}