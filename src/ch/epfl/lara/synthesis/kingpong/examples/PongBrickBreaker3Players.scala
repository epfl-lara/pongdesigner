package ch.epfl.lara.synthesis.kingpong.examples

import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.ast._

class PongBrickBreaker3Players extends Game {
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
  val ball = Circle(437.441f, 67.749f, 20) named "ball"
  ball.velocity_x = -0.207f
  ball.velocity_y = -0.003f
  ball.color = -55698
  arena1 += ball
  val ball1 = Circle(77.145f, 674.021f, 20) named "ball1"
  ball1.velocity_x = 0.169f
  ball1.velocity_y = 0.01f
  ball1.color = -19655
  arena1 += ball1
  val ennemy = Circle(240, 330, 60) named "ennemy"
  ennemy.noVelocity = true
  ennemy.color = -16777216
  arena1 += ennemy
  val player = Circle(125, 55.137f, 50) named "player"
  player.noVelocity = true
  player.color = -55698
  arena1 += player
  val player2 = Circle(365, 700, 50) named "player2"
  player2.noVelocity = true
  player2.color = -19655
  arena1 += player2
  val player1 = Circle(438.854f, 250, 50) named "player1"
  player1.noVelocity = true
  player1.color = -15230084
  arena1 += player1
  val ball6 = Circle(412.048f, 479.702f, 20) named "ball6"
  ball6.velocity_x = -0.001f
  ball6.velocity_y = -0.18f
  ball6.color = -15230084
  arena1 += ball6
  val wall4 = Rectangle(411.013f, 531.25f, 40, 35) named "wall4"
  wall4.noVelocity = true
  arena1 += wall4
  val wall5 = Rectangle(410, 150, 40, 35) named "wall5"
  wall5.noVelocity = true
  arena1 += wall5
  val wall6 = Rectangle(145, 305, 50, 45) named "wall6"
  wall6.noVelocity = true
  wall6.color = -5266352
  arena1 += wall6
  val total = IntegerBox(40, 70, 40, 40, 0) named "total"
  total.noVelocity = true
  total.color = -55698
  arena1 += total
  val total2 = IntegerBox(50, 610, 40, 40, 0) named "total2"
  total2.noVelocity = true
  total2.color = -19655
  arena1 += total2
  val total1 = IntegerBox(400, 475, 40, 40, 0) named "total1"
  total1.noVelocity = true
  total1.color = -15230084
  arena1 += total1
  val score = IntegerBox(625, 235, 40, 40, 0) named "score"
  score.noVelocity = true
  arena1 += score
  val wall7 = Rectangle(50, 260, 50, 45) named "wall7"
  wall7.noVelocity = true
  wall7.color = -5266352
  arena1 += wall7
  val wall8 = Rectangle(125, 250, 50, 45) named "wall8"
  wall8.noVelocity = true
  wall8.color = -5266352
  arena1 += wall8
  val wall9 = Rectangle(40, 330, 50, 45) named "wall9"
  wall9.noVelocity = true
  wall9.color = -5266352
  arena1 += wall9
  val wall10 = Rectangle(105, 355, 50, 45) named "wall10"
  wall10.noVelocity = true
  wall10.color = -5266352
  arena1 += wall10
  val wall11 = Rectangle(55, 195, 50, 45) named "wall11"
  wall11.noVelocity = true
  wall11.color = -5266352
  arena1 += wall11
  val wall12 = Rectangle(130, 185, 50, 45) named "wall12"
  wall12.noVelocity = true
  wall12.color = -5266352
  arena1 += wall12
  val wall13 = Rectangle(205, 180, 50, 45) named "wall13"
  wall13.noVelocity = true
  wall13.color = -5266352
  arena1 += wall13
  val wall14 = Rectangle(200, 245, 50, 45) named "wall14"
  wall14.noVelocity = true
  wall14.color = -5266352
  arena1 += wall14
  val wall15 = Rectangle(95, 300, 50, 45) named "wall15"
  wall15.noVelocity = true
  wall15.color = -5266352
  arena1 += wall15
  val wall16 = Rectangle(175, 365, 50, 45) named "wall16"
  wall16.noVelocity = true
  wall16.color = -5266352
  arena1 += wall16
  val wall17 = Rectangle(40, 395, 50, 45) named "wall17"
  wall17.noVelocity = true
  wall17.color = -5266352
  arena1 += wall17
  val wall18 = Rectangle(110, 415, 50, 45) named "wall18"
  wall18.noVelocity = true
  wall18.color = -5266352
  arena1 += wall18
  val wall19 = Rectangle(175, 430, 50, 45) named "wall19"
  wall19.noVelocity = true
  wall19.color = -5266352
  arena1 += wall19
  val wall20 = Rectangle(240, 375, 50, 45) named "wall20"
  wall20.noVelocity = true
  wall20.color = -5266352
  arena1 += wall20
  val wall21 = Rectangle(270, 305, 50, 45) named "wall21"
  wall21.noVelocity = true
  wall21.color = -5266352
  arena1 += wall21
  val wall22 = Rectangle(270, 250, 50, 45) named "wall22"
  wall22.noVelocity = true
  wall22.color = -5266352
  arena1 += wall22
  val wall23 = Rectangle(275, 190, 50, 45) named "wall23"
  wall23.noVelocity = true
  wall23.color = -5266352
  arena1 += wall23
  val wall24 = Rectangle(240, 440, 50, 45) named "wall24"
  wall24.noVelocity = true
  wall24.color = -5266352
  arena1 += wall24
  
  WhenFingerMovesOn(player) { (xFrom, yFrom, xTo, yTo) =>
    player.x += xTo - xFrom // 2 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "x"), "$plus$eq"),List(EApply(ESelect(EIdent("xTo"), "$minus"),List(EIdent("xFrom"))))), EApply(ESelect(ESelect(EIdentShape(player), "x"), "$eq"),List(EConstantNumber(255.0))), EApply(ESelect(ESelect(EIdentShape(player), "x"), "$plus$eq"),List(EConstantNumber(59.0)))))))
  WhenFingerMovesOn(player2) { (xFrom, yFrom, xTo, yTo) =>
    player2.x += xTo - xFrom // 2 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player2), "x"), "$plus$eq"),List(EApply(ESelect(EIdent("xTo"), "$minus"),List(EIdent("xFrom"))))), EApply(ESelect(ESelect(EIdentShape(player2), "x"), "$eq"),List(EConstantNumber(255.0))), EApply(ESelect(ESelect(EIdentShape(player2), "x"), "$plus$eq"),List(EConstantNumber(59.0)))))))
  NoCollisionBetween(player1, wall1)
  WhenFingerMovesOn(player1) { (xFrom, yFrom, xTo, yTo) =>
    player1.y += yTo - yFrom // 2 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player1), "y"), "$plus$eq"),List(EApply(ESelect(EIdent("yTo"), "$minus"),List(EIdent("yFrom"))))), EApply(ESelect(ESelect(EIdentShape(player1), "y"), "$eq"),List(EConstantNumber(494.0))), EApply(ESelect(ESelect(EIdentShape(player1), "y"), "$plus$eq"),List(EConstantNumber(153.0)))))))
  NoCollisionBetween(player, wall2)
  NoCollisionBetween(player2, wall3)
  WhenCollisionBetween(player1, wall5) {
    player1.y = 239 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player1), "y"), "$eq"),List(EConstantNumber(239.0))), EApply(ESelect(ESelect(EIdentShape(player1), "y"), "$plus$eq"),List(EConstantNumber(35.0)))))))
  WhenCollisionBetween(player1, wall4) {
    player1.y = 479 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player1), "y"), "$eq"),List(EConstantNumber(479.0))), EApply(ESelect(ESelect(EIdentShape(player1), "y"), "$plus$eq"),List(EConstantNumber(-37.0)))))))
  WhenCollisionBetween(ball, wall6) {
    wall6.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall6), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall6) {
    wall6.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall6), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall6) {
    wall6.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall6), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, ennemy) {
    total.value += -1 // 5 more
    score.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(-1.0f))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$minus"),List(ESelect(EIdentShape(score), "value"))))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$minus"),List(ESelect(EIdentShape(score), "value"))))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$minus"),List(ESelect(EIdentShape(score), "value"))))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$minus$eq"),List(ESelect(EIdentShape(score), "value"))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(-1.0f))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(EConstantNumber(1.0f))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(ERandomNumber(List(1.0f, 2.0f))))))))
  WhenCollisionBetween(ball1, ennemy) {
    total2.value += -1 // 4 more
    score.value += 1 // 3 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(-1.0f))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$minus"),List(ESelect(EIdentShape(total1), "value"))))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus"),List(ESelect(EIdentShape(total1), "value"))))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$minus$eq"),List(ESelect(EIdentShape(total1), "value"))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(-1.0f))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(EConstantNumber(1.0f))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus"),List(ESelect(EIdentShape(score), "value"))))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(ESelect(EIdentShape(total1), "value"))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(ERandomNumber(List(2.0f, 4.0f))))))))
  WhenCollisionBetween(ennemy, ball6) {
    total1.value += -1 // 9 more
    score.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(-1.0f))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$div"),List(ESelect(EIdentShape(score), "value"))))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$div"),List(ESelect(EIdentShape(score), "value"))))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus"),List(ESelect(EIdentShape(total1), "value"))))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$div"),List(ESelect(EIdentShape(score), "value"))))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$div$eq"),List(ESelect(EIdentShape(score), "value"))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(ESelect(EIdentShape(total2), "value"))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$div$eq"),List(ESelect(EIdentShape(total), "value"))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$div$eq"),List(EConstantNumber(2.0f))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(0.0f))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(EConstantNumber(1.0f))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(ERandomNumber(List(3.0f, 6.0f))))))))
  WhenIntegerChanges(score) { (newValue) =>
    ennemy.x = random(280.0f, 90.0f, 180.0f) // 1 more
    ennemy.y = random(485.0f, 240.0f, 355.0f) // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ennemy), "x"), "$eq"),List(ERandomNumber(List(280.0f, 90.0f, 180.0f)))), EApply(ESelect(ESelect(EIdentShape(ennemy), "x"), "$plus$eq"),List(ERandomNumber(List(100.0f, -22.0f, 90.0f)))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ennemy), "y"), "$eq"),List(ERandomNumber(List(485.0f, 240.0f, 355.0f)))), EApply(ESelect(ESelect(EIdentShape(ennemy), "y"), "$plus$eq"),List(ERandomNumber(List(130.0f, -83.0f, 115.0f))))))))
  WhenCollisionBetween(ball, wall7) {
    wall7.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall7), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0f))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0f)))))))
  WhenCollisionBetween(ball1, wall7) {
    wall7.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall7), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0f))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0f)))))))
  WhenCollisionBetween(ball6, wall7) {
    wall7.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall7), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall8) {
    wall8.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall8), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall8) {
    wall8.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall8), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall8) {
    wall8.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall8), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall9) {
    wall9.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall9), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall9) {
    wall9.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall9), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall9) {
    wall9.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall9), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall10) {
    wall10.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall10), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall10) {
    wall10.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall10), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall10) {
    wall10.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall10), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall11) {
    wall11.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall11), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall11) {
    wall11.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall11), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall11) {
    wall11.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall11), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall12) {
    wall12.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall12), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall12) {
    wall12.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall12), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall12) {
    wall12.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall12), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall13) {
    wall13.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall13), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall13) {
    wall13.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall13), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall13) {
    wall13.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall13), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall14) {
    wall14.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall14), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall14) {
    wall14.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall14), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall14) {
    wall14.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall14), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall15) {
    wall15.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall15), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall15) {
    wall15.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall15), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall15) {
    wall15.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall15), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall16) {
    wall16.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall16), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall16) {
    wall16.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall16), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall16) {
    wall16.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall16), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall17) {
    wall17.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall17), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall17) {
    wall17.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall17), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall17) {
    wall17.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall17), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall18) {
    wall18.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall18), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall18) {
    wall18.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall18), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall18) {
    wall18.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall18), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall19) {
    wall19.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall19), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall19) {
    wall19.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall19), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall19) {
    wall19.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall19), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall20) {
    wall20.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall20), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall20) {
    wall20.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall20), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall20) {
    wall20.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall20), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall21) {
    wall21.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall21), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall21) {
    wall21.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall21), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall21) {
    wall21.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall21), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall22) {
    wall22.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall22), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall22) {
    wall22.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall22), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall22) {
    wall22.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall22), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall23) {
    wall23.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall23), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall23) {
    wall23.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall23), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall23) {
    wall23.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall23), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball, wall24) {
    wall24.visible = false
    total.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall24), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball1, wall24) {
    wall24.visible = false
    total2.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall24), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total2), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenCollisionBetween(ball6, wall24) {
    wall24.visible = false
    total1.value += 1 // 1 more
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall24), "visible"), "$eq"),List(EConstantBoolean(false))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(total1), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  WhenEver(ball6.y - ball6.radius > layoutHeight || ball6.y + ball6.radius < 0 || ball6.x - ball6.radius > layoutWidth || ball6.x + ball6.radius < 0) {
    if(ball6.y - ball6.radius > layoutHeight || ball6.y + ball6.radius < 0 || ball6.x - ball6.radius > layoutWidth || ball6.x + ball6.radius < 0) {
      ball6.x = 344 // 1 more
      ball6.y = 397 // 1 more
    }
  }.represents(EApply(ESelect(EApply(ESelect(EApply(ESelect(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball6), "y"), "$minus"),List(ESelect(EIdentShape(ball6), "radius"))), "$greater"),List(EIdent("layoutHeight"))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball6), "y"), "$plus"),List(ESelect(EIdentShape(ball6), "radius"))), "$less"),List(EConstantNumber(0.0))))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball6), "x"), "$minus"),List(ESelect(EIdentShape(ball6), "radius"))), "$greater"),List(EIdent("layoutWidth"))))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball6), "x"), "$plus"),List(ESelect(EIdentShape(ball6), "radius"))), "$less"),List(EConstantNumber(0.0))))),List(IfCode(EApply(ESelect(EApply(ESelect(EApply(ESelect(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball6), "y"), "$minus"),List(ESelect(EIdentShape(ball6), "radius"))), "$greater"),List(EIdent("layoutHeight"))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball6), "y"), "$plus"),List(ESelect(EIdentShape(ball6), "radius"))), "$less"),List(EConstantNumber(0.0))))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball6), "x"), "$minus"),List(ESelect(EIdentShape(ball6), "radius"))), "$greater"),List(EIdent("layoutWidth"))))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball6), "x"), "$plus"),List(ESelect(EIdentShape(ball6), "radius"))), "$less"),List(EConstantNumber(0.0))))),List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball6), "x"), "$eq"),List(EConstantNumber(344.0))), EApply(ESelect(ESelect(EIdentShape(ball6), "x"), "$plus$eq"),List(EConstantNumber(-162.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball6), "y"), "$eq"),List(EConstantNumber(397.0))), EApply(ESelect(ESelect(EIdentShape(ball6), "y"), "$plus$eq"),List(EConstantNumber(-73.0)))))),List())))
  WhenEver(ball1.y - ball1.radius > layoutHeight || ball1.y + ball1.radius < 0 || ball1.x - ball1.radius > layoutWidth || ball1.x + ball1.radius < 0) {
    if(ball1.y - ball1.radius > layoutHeight || ball1.y + ball1.radius < 0 || ball1.x - ball1.radius > layoutWidth || ball1.x + ball1.radius < 0) {
      ball1.x = 129 // 1 more
      ball1.y = 595 // 1 more
    }
  }.represents(EApply(ESelect(EApply(ESelect(EApply(ESelect(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "y"), "$minus"),List(ESelect(EIdentShape(ball1), "radius"))), "$greater"),List(EIdent("layoutHeight"))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "y"), "$plus"),List(ESelect(EIdentShape(ball1), "radius"))), "$less"),List(EConstantNumber(0.0))))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "x"), "$minus"),List(ESelect(EIdentShape(ball1), "radius"))), "$greater"),List(EIdent("layoutWidth"))))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "x"), "$plus"),List(ESelect(EIdentShape(ball1), "radius"))), "$less"),List(EConstantNumber(0.0))))),List(IfCode(EApply(ESelect(EApply(ESelect(EApply(ESelect(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "y"), "$minus"),List(ESelect(EIdentShape(ball1), "radius"))), "$greater"),List(EIdent("layoutHeight"))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "y"), "$plus"),List(ESelect(EIdentShape(ball1), "radius"))), "$less"),List(EConstantNumber(0.0))))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "x"), "$minus"),List(ESelect(EIdentShape(ball1), "radius"))), "$greater"),List(EIdent("layoutWidth"))))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "x"), "$plus"),List(ESelect(EIdentShape(ball1), "radius"))), "$less"),List(EConstantNumber(0.0))))),List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball1), "x"), "$eq"),List(EConstantNumber(129.0))), EApply(ESelect(ESelect(EIdentShape(ball1), "x"), "$plus$eq"),List(EConstantNumber(-376.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball1), "y"), "$eq"),List(EConstantNumber(595.0))), EApply(ESelect(ESelect(EIdentShape(ball1), "y"), "$plus$eq"),List(EConstantNumber(75.0)))))),List())))
  WhenEver(ball1.y - ball1.radius > layoutHeight || ball1.y + ball1.radius < 0 || ball1.x - ball1.radius > layoutWidth || ball1.x + ball1.radius < 0) {
    if(ball1.y - ball1.radius > layoutHeight || ball1.y + ball1.radius < 0 || ball1.x - ball1.radius > layoutWidth || ball1.x + ball1.radius < 0) {
      ball1.x = 129 // 1 more
      ball1.y = 595 // 1 more
    }
  }.represents(EApply(ESelect(EApply(ESelect(EApply(ESelect(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "y"), "$minus"),List(ESelect(EIdentShape(ball1), "radius"))), "$greater"),List(EIdent("layoutHeight"))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "y"), "$plus"),List(ESelect(EIdentShape(ball1), "radius"))), "$less"),List(EConstantNumber(0.0))))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "x"), "$minus"),List(ESelect(EIdentShape(ball1), "radius"))), "$greater"),List(EIdent("layoutWidth"))))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "x"), "$plus"),List(ESelect(EIdentShape(ball1), "radius"))), "$less"),List(EConstantNumber(0.0))))),List(IfCode(EApply(ESelect(EApply(ESelect(EApply(ESelect(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "y"), "$minus"),List(ESelect(EIdentShape(ball1), "radius"))), "$greater"),List(EIdent("layoutHeight"))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "y"), "$plus"),List(ESelect(EIdentShape(ball1), "radius"))), "$less"),List(EConstantNumber(0.0))))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "x"), "$minus"),List(ESelect(EIdentShape(ball1), "radius"))), "$greater"),List(EIdent("layoutWidth"))))), "$bar$bar"),List(EApply(ESelect(EApply(ESelect(ESelect(EIdentShape(ball1), "x"), "$plus"),List(ESelect(EIdentShape(ball1), "radius"))), "$less"),List(EConstantNumber(0.0))))),List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball1), "x"), "$eq"),List(EConstantNumber(129.0))), EApply(ESelect(ESelect(EIdentShape(ball1), "x"), "$plus$eq"),List(EConstantNumber(-376.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(ball1), "y"), "$eq"),List(EConstantNumber(595.0))), EApply(ESelect(ESelect(EIdentShape(ball1), "y"), "$plus$eq"),List(EConstantNumber(75.0)))))),List())))
}