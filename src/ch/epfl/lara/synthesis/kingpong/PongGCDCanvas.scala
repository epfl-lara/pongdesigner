package ch.epfl.lara.synthesis.kingpong;

class PongGCDCanvas extends Game {
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
  val goal = IntegerBox(120, 200, 40, 40, 15) named "goal"
  goal.noVelocity = true
  goal.color = -19655
  arena1 += goal
  val goal1 = IntegerBox(240, 200, 40, 40, 9) named "goal1"
  goal1.noVelocity = true
  goal1.color = -19655
  arena1 += goal1
  val wall4 = Rectangle(89.337f, 146.784f, 93, 35) named "wall4"
  arena1 += wall4
  val wall5 = Rectangle(212.194f, 144.23f, 93, 35) named "wall5"
  arena1 += wall5
  val wall6 = Rectangle(91.263f, 260.817f, 93, 35) named "wall6"
  arena1 += wall6
  val wall7 = Rectangle(206.346f, 260.977f, 93, 35) named "wall7"
  arena1 += wall7
  val textBox = TextBox(327.74f, 188.41f, 100, 70, "Go") named "textBox"
  arena1 += textBox
  val score = IntegerBox(120, 375, 40, 40, 0) named "score"
  score.noVelocity = true
  arena1 += score
  val score1 = IntegerBox(240, 375, 40, 40, 0) named "score1"
  score1.noVelocity = true
  arena1 += score1
  val remainder = IntegerBox(340, 375, 40, 40, 0) named "remainder"
  remainder.noVelocity = true
  arena1 += remainder
  val zero = IntegerBox(405, 55, 40, 40, 0) named "zero"
  zero.noVelocity = true
  arena1 += zero
  val textBox1 = TextBox(75.937f, 521.634f, 175, 60, "gcd =") named "textBox1"
  textBox1.color = -19655
  arena1 += textBox1
  val total = IntegerBox(260.432f, 538.588f, 40, 40, 0) named "total"
  total.color = -19655
  total.visible = false
  arena1 += total
  val textBox2 = TextBox(302.017f, 433.269f, 110, 45, "step") named "textBox2"
  arena1 += textBox2
  
  WhenFingerDownOn(textBox) {
    score.value = goal.value // 13 more
    score1.value = goal1.value // 11 more
    total.visible = false
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(ESelect(EIdentShape(goal), "value"))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal), "value"), "$minus"),List(ESelect(EIdentShape(total), "value"))))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal), "value"), "$plus"),List(ESelect(EIdentShape(total), "value"))))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal), "value"), "$minus"),List(ESelect(EIdentShape(zero), "value"))))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal), "value"), "$plus"),List(ESelect(EIdentShape(zero), "value"))))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal), "value"), "$minus"),List(ESelect(EIdentShape(remainder), "value"))))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal), "value"), "$plus"),List(ESelect(EIdentShape(remainder), "value"))))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal), "value"), "$minus"),List(ESelect(EIdentShape(score1), "value"))))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal), "value"), "$plus"),List(ESelect(EIdentShape(score1), "value"))))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal), "value"), "$minus"),List(ESelect(EIdentShape(score), "value"))))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal), "value"), "$plus"),List(ESelect(EIdentShape(score), "value"))))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(ESelect(EIdentShape(goal), "value"))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(EConstantNumber(15.0))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EConstantNumber(15.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(score1), "value"), "$eq"),List(ESelect(EIdentShape(goal1), "value"))), EApply(ESelect(ESelect(EIdentShape(score1), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal1), "value"), "$minus"),List(ESelect(EIdentShape(total), "value"))))), EApply(ESelect(ESelect(EIdentShape(score1), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal1), "value"), "$plus"),List(ESelect(EIdentShape(total), "value"))))), EApply(ESelect(ESelect(EIdentShape(score1), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal1), "value"), "$minus"),List(ESelect(EIdentShape(zero), "value"))))), EApply(ESelect(ESelect(EIdentShape(score1), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal1), "value"), "$plus"),List(ESelect(EIdentShape(zero), "value"))))), EApply(ESelect(ESelect(EIdentShape(score1), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal1), "value"), "$minus"),List(ESelect(EIdentShape(remainder), "value"))))), EApply(ESelect(ESelect(EIdentShape(score1), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal1), "value"), "$plus"),List(ESelect(EIdentShape(remainder), "value"))))), EApply(ESelect(ESelect(EIdentShape(score1), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal1), "value"), "$minus"),List(ESelect(EIdentShape(score1), "value"))))), EApply(ESelect(ESelect(EIdentShape(score1), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(goal1), "value"), "$plus"),List(ESelect(EIdentShape(score1), "value"))))), EApply(ESelect(ESelect(EIdentShape(score1), "value"), "$plus$eq"),List(ESelect(EIdentShape(goal1), "value"))), EApply(ESelect(ESelect(EIdentShape(score1), "value"), "$plus$eq"),List(EConstantNumber(9.0))), EApply(ESelect(ESelect(EIdentShape(score1), "value"), "$eq"),List(EConstantNumber(9.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "visible"), "$eq"),List(EConstantBoolean(false)))))))
  WhenFingerDownOn(wall6) {
    goal.value += -1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(goal), "value"), "$plus$eq"),List(EConstantNumber(-1.0))), EApply(ESelect(ESelect(EIdentShape(goal), "value"), "$eq"),List(EConstantNumber(14.0)))))))
  WhenFingerDownOn(wall7) {
    goal1.value += -1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(goal1), "value"), "$plus$eq"),List(EConstantNumber(-1.0))), EApply(ESelect(ESelect(EIdentShape(goal1), "value"), "$eq"),List(EConstantNumber(8.0)))))))
  WhenFingerDownOn(wall5) {
    goal1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(goal1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(goal1), "value"), "$eq"),List(EConstantNumber(10.0)))))))
  WhenFingerDownOn(wall4) {
    goal.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(goal), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(goal), "value"), "$eq"),List(EConstantNumber(16.0)))))))
}