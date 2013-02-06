package ch.epfl.lara.synthesis.kingpong;


/**
 * The pong game
 * Goal: To write such a game in less than 1 minute.
 */
class PongGameComplete extends Game {
  import GameShapes._

  /**
   * Game static values
   */
  var screenWidth = 480
  var screenHeight = 750
  
  /**
   * Arenas
   */
  val arena1 = Arena()
  val arena2 = Arena()
  
  /** Game Layouts */
  val paddle1 = Rectangle(200, 10, 100, 40)
  paddle1.noVelocity = true
  arena1 += paddle1
  val paddle2 = Rectangle(200, screenHeight-50, 100, 40)
  paddle2.noVelocity = true
  arena1 += paddle2
  val temp1 = Rectangle.fromBounds(0, 50, 20, screenHeight-50)
  temp1.noVelocity = true
  arena1 += temp1
  val temp2 = Rectangle.fromBounds(460, 50, 480, screenHeight-50)
  temp2.noVelocity = true
  arena1 += temp2
  val ball = Circle(screenWidth/2, screenHeight/2 + 20, 40)
  ball.noVelocity = false
  ball.velocity_x = 0.1f
  ball.velocity_y = 0.1f
  arena1 += ball
  val ball2 = Circle(screenWidth/2 + 5, screenHeight/2 - 20, 30)
  ball2.noVelocity = false
  ball2.color = 0xFFFF0000
  ball2.velocity_x = 0.1f
  ball2.velocity_y = -0.1f
  arena1 += ball2
  val obstacle = Circle(screenWidth / 2, screenHeight*3/4, 10)
  obstacle.noVelocity = true
  arena1 += obstacle
  val obstacle2 = Circle(screenWidth / 2, screenHeight*1/4, 10)
  obstacle2.noVelocity = true
  arena1 += obstacle2
  val displayedScoreForPlayer1 = IntegerBox(25, 360, 40, 35, 0)
  arena1 += displayedScoreForPlayer1
  val displayedScoreForPlayer2 = IntegerBox(25, 400, 40, 35, 0)
  arena1 += displayedScoreForPlayer2
  
  val collisionCounter = IntegerBox(screenWidth - 50, screenHeight/2, 40, 35, 0)
  arena1 += collisionCounter
  
  val gameWonForPlayer1Text = TextBox(10, 50, 400, 50, "Player ^ won !")
  gameWonForPlayer1Text.visible = false
  arena2 += gameWonForPlayer1Text
  val gameWonForPlayer2Text = TextBox(10, screenHeight-30, 400, 50, "Player V won !")
  gameWonForPlayer2Text.visible = false
  arena2 += gameWonForPlayer2Text
  val replayButton = TextBox(10, screenHeight/2, 400, 50, "Play King Pong")
  arena2 += replayButton
  
  /**
   * Scene selection
   */
  setCurrentArena(arena2)
  
  /**
   * Rules
   */
  WhenEver (ball.y + ball.radius < 0) {
    ball.x = screenWidth / 2
    ball.y = screenHeight / 2
    ball.velocity = ball.velocity * 0.5f
    displayedScoreForPlayer2+=1
  }
  WhenEver(ball.y - ball.radius > screenHeight) {
    ball.x = screenWidth / 2
    ball.y = screenHeight / 2
    ball.velocity = ball.velocity * 0.5f
    displayedScoreForPlayer1+=1
  }
  WhenEver (ball2.y + ball2.radius < 0) {
    ball2.x = screenWidth / 2
    ball2.y = screenHeight / 2
    ball2.velocity = ball2.velocity * 0.5f
    paddle1.width = (paddle1.width * 0.9).toInt
  }
  WhenEver(ball2.y - ball2.radius > screenHeight) {
    ball2.x = screenWidth / 2
    ball2.y = screenHeight / 2
    ball2.velocity = ball2.velocity * 0.5f
    paddle2.width = (paddle2.width * 0.9).toInt
  }
  WhenEver(displayedScoreForPlayer1.value >= 10) {
    gameWonForPlayer1Text.visible = true
    gameWonForPlayer2Text.visible = false
    setCurrentArena(arena2)
  }
  WhenEver(displayedScoreForPlayer2.value >= 10) {
    gameWonForPlayer1Text.visible = false
    gameWonForPlayer2Text.visible = true
    setCurrentArena(arena2)
  }
  WhenIntegerChanges(displayedScoreForPlayer1) { (oldValue, newValue) =>
    ball.velocity *= 1.05f
  }
  WhenIntegerChanges(displayedScoreForPlayer2) { (oldValue, newValue) =>
    ball.velocity *= 1.05f
  }
  WhenCollisionBetween(ball, paddle1) {
    ball.velocity_x *= 1.03f
  }
  WhenCollisionBetween(ball, paddle2) {
    ball.velocity_x *= 1.03f
  }
  WhenFingerDownOn(replayButton) {
    displayedScoreForPlayer1.value = 0
    displayedScoreForPlayer2.value = 0
    setCurrentArena(arena1)
  }
  WhenFingerMovesOn(paddle1) { (xFrom, yFrom, xTo, yTo) =>
    paddle1.x += xTo - xFrom
  }
  WhenFingerMovesOn(paddle2) { (xFrom, yFrom, xTo, yTo) =>
    paddle2.x += xTo - xFrom
  }
  WhenEver(paddle1.x < 0) {
    paddle1.x = 0
  }
  WhenEver(paddle1.x + paddle1.width > screenWidth) {
    paddle1.x = screenWidth - paddle1.width
  }
  WhenEver(paddle2.x < 0) {
    paddle2.x = 0
  }
  WhenEver(paddle2.x + paddle2.width > screenWidth) {
    paddle2.x = screenWidth - paddle2.width
  }
  WhenFingerDownOn(ball) {
    ball.velocity *= 1.3f
  }
  WhenFingerDownOn(ball2) {
    ball2.velocity *= 1.3f
  }
  Camera.x = 0
  Camera.y = 0
  Camera.width = screenWidth
  Camera.height = screenHeight
}
