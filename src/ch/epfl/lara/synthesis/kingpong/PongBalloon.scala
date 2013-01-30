package ch.epfl.lara.synthesis.kingpong;

class PongBalloon extends Game {
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
  val wall4 = Rectangle(22.518f, 349.256f, 425, 25) named "wall4"
  wall4.noVelocity = true
  wall4.color = -16777216
  arena1 += wall4
  val wall5 = Rectangle(153.858f, 636.441f, 20, 80) named "wall5"
  wall5.color = -55698
  arena1 += wall5
  val textBox = TextBox(39.849f, 654.028f, 400, 60, "") named "textBox"
  textBox.color = -55698
  arena1 += textBox
  val ball = Circle(389.18f, 580.344f, 50) named "ball"
  ball.noVelocity = true
  ball.color = -55698
  arena1 += ball
  val ball1 = Circle(182.155f, 212.307f, 30) named "ball1"
  ball1.velocity_x = 0.226f
  ball1.velocity_y = -0.17f
  ball1.color = -15230084
  arena1 += ball1
  val score = IntegerBox(40.87f, 405.117f, 60, 60, 0) named "score"
  arena1 += score
  val ball2 = Circle(318.141f, 172.301f, 30) named "ball2"
  ball2.velocity_x = -0.106f
  ball2.velocity_y = 0.068f
  ball2.color = -15230084
  arena1 += ball2
  val ball3 = Circle(213.692f, 85.801f, 30) named "ball3"
  ball3.velocity_x = -0.022f
  ball3.velocity_y = 0.106f
  ball3.color = -15230084
  arena1 += ball3
  val ball4 = Circle(377.04f, 271.164f, 30) named "ball4"
  ball4.velocity_x = 0.066f
  ball4.velocity_y = -0.059f
  ball4.color = -15230084
  arena1 += ball4
  val ball5 = Circle(71.031f, 289.616f, 30) named "ball5"
  ball5.velocity_x = 0.097f
  ball5.velocity_y = -0.23f
  ball5.color = -15230084
  arena1 += ball5
  val ball6 = Circle(92.755f, 134.322f, 30) named "ball6"
  ball6.velocity_x = 0.116f
  ball6.velocity_y = 0.193f
  ball6.color = -15230084
  arena1 += ball6
  
  NoCollisionBetween(ball, wall5)
  NoCollisionBetween(wall4, wall5)
  WhenFingerUpOn(ball) {
    wall5.angle = -180 // 1 more
    wall5.velocity = 0.403f
    textBox.visible = false
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$eq"),List(EConstantNumber(-180.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$plus$eq"),List(EConstantNumber(-180.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "velocity"), "$eq"),List(EConstantNumber(0.40370923))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(textBox), "visible"), "$eq"),List(EConstantBoolean(false)))))))
  WhenCollisionBetween(wall2, wall5) {
    wall5.x = 221 // 1 more
    wall5.y = 632 // 1 more
    wall5.angle = -180 // 1 more
    wall5.velocity = 0
    textBox.visible = true
    score.value += -1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$eq"),List(EConstantNumber(221.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$plus$eq"),List(EConstantNumber(68.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "y"), "$eq"),List(EConstantNumber(632.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "y"), "$plus$eq"),List(EConstantNumber(615.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$eq"),List(EConstantNumber(-180.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$plus$eq"),List(EConstantNumber(-180.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "velocity"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(textBox), "visible"), "$eq"),List(EConstantBoolean(true))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(EConstantNumber(-1.0))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EConstantNumber(-1.0)))))))
  WhenFingerMovesOn(textBox) { (xFrom, yFrom, xTo, yTo) =>
    wall5.x += xTo - xFrom // 2 more
    wall5.angle = -180 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$plus$eq"),List(EApply(ESelect(EIdent("xTo"), "$minus"),List(EIdent("xFrom"))))), EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$eq"),List(EConstantNumber(115.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$plus$eq"),List(EConstantNumber(-106.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$eq"),List(EConstantNumber(-180.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$plus$eq"),List(EConstantNumber(-180.0)))))))
  WhenCollisionBetween(ball1, wall5) {
    wall5.x = 234 // 1 more
    wall5.y = 631 // 1 more
    wall5.angle = -180 // 1 more
    wall5.velocity = 0
    textBox.visible = true
    ball1.x = 636 // 1 more
    ball1.y = 397 // 1 more
    ball1.angle = 0 // 1 more
    ball1.velocity = 0
    score.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$eq"),List(EConstantNumber(234.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$plus$eq"),List(EConstantNumber(81.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "y"), "$eq"),List(EConstantNumber(631.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "y"), "$plus$eq"),List(EConstantNumber(405.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$eq"),List(EConstantNumber(-180.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$plus$eq"),List(EConstantNumber(-90.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "velocity"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(textBox), "visible"), "$eq"),List(EConstantBoolean(true))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball1), "x"), "$eq"),List(EConstantNumber(636.0))), EApply(ESelect(ESelect(EIdentShape(ball1), "x"), "$plus$eq"),List(EConstantNumber(454.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball1), "y"), "$eq"),List(EConstantNumber(397.0))), EApply(ESelect(ESelect(EIdentShape(ball1), "y"), "$plus$eq"),List(EConstantNumber(185.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball1), "angle"), "$eq"),List(EConstantNumber(0.0))), EApply(ESelect(ESelect(EIdentShape(ball1), "angle"), "$plus$eq"),List(EConstantNumber(-150.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball1), "velocity"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball2, wall5) {
    wall5.x = 234 // 1 more
    wall5.y = 631 // 1 more
    wall5.angle = -180 // 1 more
    wall5.velocity = 0
    textBox.visible = true
    ball2.x = 636 // 1 more
    ball2.y = 397 // 1 more
    ball2.angle = 0 // 1 more
    ball2.velocity = 0
    score.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$eq"),List(EConstantNumber(234.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$plus$eq"),List(EConstantNumber(81.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "y"), "$eq"),List(EConstantNumber(631.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "y"), "$plus$eq"),List(EConstantNumber(405.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$eq"),List(EConstantNumber(-180.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$plus$eq"),List(EConstantNumber(-90.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "velocity"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(textBox), "visible"), "$eq"),List(EConstantBoolean(true))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball2), "x"), "$eq"),List(EConstantNumber(636.0))), EApply(ESelect(ESelect(EIdentShape(ball2), "x"), "$plus$eq"),List(EConstantNumber(454.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball2), "y"), "$eq"),List(EConstantNumber(397.0))), EApply(ESelect(ESelect(EIdentShape(ball2), "y"), "$plus$eq"),List(EConstantNumber(185.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball2), "angle"), "$eq"),List(EConstantNumber(0.0))), EApply(ESelect(ESelect(EIdentShape(ball2), "angle"), "$plus$eq"),List(EConstantNumber(-150.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball2), "velocity"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball3, wall5) {
    wall5.x = 234 // 1 more
    wall5.y = 631 // 1 more
    wall5.angle = -180 // 1 more
    wall5.velocity = 0
    textBox.visible = true
    ball3.x = 636 // 1 more
    ball3.y = 397 // 1 more
    ball3.angle = 0 // 1 more
    ball3.velocity = 0
    score.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$eq"),List(EConstantNumber(234.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$plus$eq"),List(EConstantNumber(81.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "y"), "$eq"),List(EConstantNumber(631.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "y"), "$plus$eq"),List(EConstantNumber(405.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$eq"),List(EConstantNumber(-180.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$plus$eq"),List(EConstantNumber(-90.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "velocity"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(textBox), "visible"), "$eq"),List(EConstantBoolean(true))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball3), "x"), "$eq"),List(EConstantNumber(636.0))), EApply(ESelect(ESelect(EIdentShape(ball3), "x"), "$plus$eq"),List(EConstantNumber(454.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball3), "y"), "$eq"),List(EConstantNumber(397.0))), EApply(ESelect(ESelect(EIdentShape(ball3), "y"), "$plus$eq"),List(EConstantNumber(185.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball3), "angle"), "$eq"),List(EConstantNumber(0.0))), EApply(ESelect(ESelect(EIdentShape(ball3), "angle"), "$plus$eq"),List(EConstantNumber(-150.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball3), "velocity"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball4, wall5) {
    wall5.x = 234 // 1 more
    wall5.y = 631 // 1 more
    wall5.angle = -180 // 1 more
    wall5.velocity = 0
    textBox.visible = true
    ball4.x = 636 // 1 more
    ball4.y = 397 // 1 more
    ball4.angle = 0 // 1 more
    ball4.velocity = 0
    score.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$eq"),List(EConstantNumber(234.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$plus$eq"),List(EConstantNumber(81.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "y"), "$eq"),List(EConstantNumber(631.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "y"), "$plus$eq"),List(EConstantNumber(405.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$eq"),List(EConstantNumber(-180.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$plus$eq"),List(EConstantNumber(-90.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "velocity"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(textBox), "visible"), "$eq"),List(EConstantBoolean(true))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball4), "x"), "$eq"),List(EConstantNumber(636.0))), EApply(ESelect(ESelect(EIdentShape(ball4), "x"), "$plus$eq"),List(EConstantNumber(454.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball4), "y"), "$eq"),List(EConstantNumber(397.0))), EApply(ESelect(ESelect(EIdentShape(ball4), "y"), "$plus$eq"),List(EConstantNumber(185.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball4), "angle"), "$eq"),List(EConstantNumber(0.0))), EApply(ESelect(ESelect(EIdentShape(ball4), "angle"), "$plus$eq"),List(EConstantNumber(-150.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball4), "velocity"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball5, wall5) {
    wall5.x = 234 // 1 more
    wall5.y = 631 // 1 more
    wall5.angle = -180 // 1 more
    wall5.velocity = 0
    textBox.visible = true
    ball5.x = 636 // 1 more
    ball5.y = 397 // 1 more
    ball5.angle = 0 // 1 more
    ball5.velocity = 0
    score.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$eq"),List(EConstantNumber(234.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$plus$eq"),List(EConstantNumber(81.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "y"), "$eq"),List(EConstantNumber(631.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "y"), "$plus$eq"),List(EConstantNumber(405.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$eq"),List(EConstantNumber(-180.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$plus$eq"),List(EConstantNumber(-90.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "velocity"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(textBox), "visible"), "$eq"),List(EConstantBoolean(true))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball5), "x"), "$eq"),List(EConstantNumber(636.0))), EApply(ESelect(ESelect(EIdentShape(ball5), "x"), "$plus$eq"),List(EConstantNumber(454.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball5), "y"), "$eq"),List(EConstantNumber(397.0))), EApply(ESelect(ESelect(EIdentShape(ball5), "y"), "$plus$eq"),List(EConstantNumber(185.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball5), "angle"), "$eq"),List(EConstantNumber(0.0))), EApply(ESelect(ESelect(EIdentShape(ball5), "angle"), "$plus$eq"),List(EConstantNumber(-150.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball5), "velocity"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall5) {
    wall5.x = 234 // 1 more
    wall5.y = 631 // 1 more
    wall5.angle = -180 // 1 more
    wall5.velocity = 0
    textBox.visible = true
    ball6.x = 636 // 1 more
    ball6.y = 397 // 1 more
    ball6.angle = 0 // 1 more
    ball6.velocity = 0
    score.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$eq"),List(EConstantNumber(234.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "x"), "$plus$eq"),List(EConstantNumber(81.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "y"), "$eq"),List(EConstantNumber(631.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "y"), "$plus$eq"),List(EConstantNumber(405.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$eq"),List(EConstantNumber(-180.0))), EApply(ESelect(ESelect(EIdentShape(wall5), "angle"), "$plus$eq"),List(EConstantNumber(-90.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall5), "velocity"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(textBox), "visible"), "$eq"),List(EConstantBoolean(true))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball6), "x"), "$eq"),List(EConstantNumber(636.0))), EApply(ESelect(ESelect(EIdentShape(ball6), "x"), "$plus$eq"),List(EConstantNumber(454.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball6), "y"), "$eq"),List(EConstantNumber(397.0))), EApply(ESelect(ESelect(EIdentShape(ball6), "y"), "$plus$eq"),List(EConstantNumber(185.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball6), "angle"), "$eq"),List(EConstantNumber(0.0))), EApply(ESelect(ESelect(EIdentShape(ball6), "angle"), "$plus$eq"),List(EConstantNumber(-150.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball6), "velocity"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EConstantNumber(1.0)))))))
}