package ch.epfl.lara.synthesis.kingpong.examples
import scala.collection.mutable.HashMap
import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.ast._

class PongGamePacman extends Game {
  /**
   * Game static values
   */
  screenWidth = 480
  screenHeight = 750

  /**
   * Game Layouts
   */
  val arena1 = Arena() named "arena1"
  val wall = Rectangle(0, 0, 25, 625) named "wall"
  wall.noVelocity = true
  wall.color = -15230084
  arena1 += wall
  val wall1 = Rectangle(455, 0, 25, 625) named "wall1"
  wall1.noVelocity = true
  wall1.color = -15230084
  arena1 += wall1
  val wall2 = Rectangle(25, 0, 430, 25) named "wall2"
  wall2.noVelocity = true
  wall2.color = -15230084
  arena1 += wall2
  val wall3 = Rectangle(25, 600, 430, 25) named "wall3"
  wall3.noVelocity = true
  wall3.color = -15230084
  arena1 += wall3
  val leftArrow = TextBox(39.764f, 661.104f, 50, 60, "\u21E6") named "leftArrow"
  leftArrow.color = -19655
  arena1 += leftArrow
  val rightArrow = TextBox(204.805f, 659.677f, 50, 60, "\u21E8") named "rightArrow"
  rightArrow.color = -19655
  arena1 += rightArrow
  val bottomArrow = TextBox(114.044f, 679.254f, 50, 60, "\u21E9") named "bottomArrow"
  bottomArrow.color = -19655
  arena1 += bottomArrow
  val topArrow = TextBox(114.201f, 610.548f, 50, 60, "\u21E7") named "topArrow"
  topArrow.color = -19655
  arena1 += topArrow
  val score = IntegerBox(390, 645, 40, 45, 0) named "score"
  score.noVelocity = true
  arena1 += score
  val scorePlayer = TextBox(290, 655, 110, 32, "Score") named "scorePlayer"
  scorePlayer.noVelocity = true
  arena1 += scorePlayer
  val scorePlayer1 = TextBox(290, 700, 110, 32, "Lives") named "scorePlayer1"
  scorePlayer1.noVelocity = true
  arena1 += scorePlayer1
  val lives = IntegerBox(390, 690, 50, 50, 4) named "lives"
  lives.noVelocity = true
  arena1 += lives
  val ennemy = Circle(130.29f, 339.341f, 20) named "ennemy"
  ennemy.velocity_x = -0.063f
  ennemy.velocity_y = -0.066f
  ennemy.color = -55698
  arena1 += ennemy
  val wall4 = Rectangle(113.947f, 24.292f, 20, 80) named "wall4"
  wall4.noVelocity = true
  wall4.color = -16777216
  arena1 += wall4
  val wall5 = Rectangle(25, 85, 90, 20) named "wall5"
  wall5.noVelocity = true
  wall5.color = -16777216
  arena1 += wall5
  val player = Circle(69.879f, 54.945f, 20) named "player"
  player.color = -19655
  arena1 += player
  val food = Circle(190, 55, 10) named "food"
  food.noVelocity = true
  arena1 += food
  val wall6 = Rectangle(140, 90, 260, 20) named "wall6"
  wall6.noVelocity = true
  wall6.color = -5266352
  arena1 += wall6
  val foodeaten = IntegerBox(575, 440, 60, 60, 0) named "foodeaten"
  foodeaten.noVelocity = true
  arena1 += foodeaten
  val wall7 = Rectangle(80, 165, 240, 20) named "wall7"
  wall7.noVelocity = true
  wall7.color = -5266352
  arena1 += wall7
  val wall8 = Rectangle(380, 110, 20, 155) named "wall8"
  wall8.noVelocity = true
  wall8.color = -5266352
  arena1 += wall8
  val wall9 = Rectangle(30, 245, 140, 20) named "wall9"
  wall9.noVelocity = true
  wall9.color = -5266352
  arena1 += wall9
  val wall10 = Rectangle(85, 325, 20, 205) named "wall10"
  wall10.noVelocity = true
  wall10.color = -5266352
  arena1 += wall10
  val wall11 = Rectangle(170, 325, 128, 21) named "wall11"
  wall11.noVelocity = true
  wall11.color = -5266352
  arena1 += wall11
  val wall12 = Rectangle(235, 245, 145, 20) named "wall12"
  wall12.noVelocity = true
  wall12.color = -5266352
  arena1 += wall12
  val wall13 = Rectangle(105, 420, 128, 21) named "wall13"
  wall13.noVelocity = true
  wall13.color = -5266352
  arena1 += wall13
  val wall14 = Rectangle(170, 510, 125, 20) named "wall14"
  wall14.noVelocity = true
  wall14.color = -5266352
  arena1 += wall14
  val wall15 = Rectangle(295, 325, 20, 205) named "wall15"
  wall15.noVelocity = true
  wall15.color = -5266352
  arena1 += wall15
  val wall16 = Rectangle(380, 330, 70, 19) named "wall16"
  wall16.noVelocity = true
  wall16.color = -5266352
  arena1 += wall16
  val wall17 = Rectangle(370, 415, 20, 120) named "wall17"
  wall17.noVelocity = true
  wall17.color = -5266352
  arena1 += wall17
  val wall18 = Rectangle(236.991f, 111.402f, 20, 130) named "wall18"
  wall18.noVelocity = true
  wall18.color = -19655
  arena1 += wall18
  val wall19 = Rectangle(38.496f, 538.925f, 30, 35) named "wall19"
  wall19.noVelocity = true
  wall19.color = -19655
  arena1 += wall19
  val wall20 = Rectangle(172.757f, 351.147f, 20, 155) named "wall20"
  wall20.noVelocity = true
  arena1 += wall20
  val wall21 = Rectangle(273.175f, 122.671f, 30, 30) named "wall21"
  wall21.noVelocity = true
  arena1 += wall21
  val food1 = Circle(265, 55, 10) named "food1"
  food1.noVelocity = true
  arena1 += food1
  val food2 = Circle(345, 50, 10) named "food2"
  food2.noVelocity = true
  arena1 += food2
  val food3 = Circle(205, 295, 10) named "food3"
  food3.noVelocity = true
  arena1 += food3
  val food4 = Circle(430, 50, 10) named "food4"
  food4.noVelocity = true
  arena1 += food4
  val food5 = Circle(430, 130, 10) named "food5"
  food5.noVelocity = true
  arena1 += food5
  val food6 = Circle(570, 125, 10) named "food6"
  food6.noVelocity = true
  arena1 += food6
  val food7 = Circle(425, 215, 10) named "food7"
  food7.noVelocity = true
  arena1 += food7
  val food8 = Circle(420, 300, 10) named "food8"
  food8.noVelocity = true
  arena1 += food8
  val food9 = Circle(285, 220, 10) named "food9"
  food9.noVelocity = true
  arena1 += food9
  val food10 = Circle(350, 295, 10) named "food10"
  food10.noVelocity = true
  arena1 += food10
  val food11 = Circle(345, 210, 10) named "food11"
  food11.noVelocity = true
  arena1 += food11
  val food12 = Circle(275, 295, 10) named "food12"
  food12.noVelocity = true
  arena1 += food12
  val food13 = Circle(135, 295, 10) named "food13"
  food13.noVelocity = true
  arena1 += food13
  val food14 = Circle(350, 140, 10) named "food14"
  food14.noVelocity = true
  arena1 += food14
  val food15 = Circle(50, 295, 10) named "food15"
  food15.noVelocity = true
  arena1 += food15
  val food16 = Circle(55, 360, 10) named "food16"
  food16.noVelocity = true
  arena1 += food16
  val food17 = Circle(140, 380, 10) named "food17"
  food17.noVelocity = true
  arena1 += food17
  val food18 = Circle(50, 425, 10) named "food18"
  food18.noVelocity = true
  arena1 += food18
  val food19 = Circle(60, 485, 10) named "food19"
  food19.noVelocity = true
  arena1 += food19
  val food20 = Circle(130, 495, 10) named "food20"
  food20.noVelocity = true
  arena1 += food20
  val food21 = Circle(125, 565, 10) named "food21"
  food21.noVelocity = true
  arena1 += food21
  val food22 = Circle(205, 570, 10) named "food22"
  food22.noVelocity = true
  arena1 += food22
  val food23 = Circle(285, 565, 10) named "food23"
  food23.noVelocity = true
  arena1 += food23
  val food24 = Circle(415, 560, 10) named "food24"
  food24.noVelocity = true
  arena1 += food24
  val food25 = Circle(340, 565, 10) named "food25"
  food25.noVelocity = true
  arena1 += food25
  val food26 = Circle(340, 498.414f, 10) named "food26"
  food26.noVelocity = true
  arena1 += food26
  val food27 = Circle(345, 365, 10) named "food27"
  food27.noVelocity = true
  arena1 += food27
  val food28 = Circle(345, 435, 10) named "food28"
  food28.noVelocity = true
  arena1 += food28
  val food29 = Circle(415, 375, 10) named "food29"
  food29.noVelocity = true
  arena1 += food29
  val food30 = Circle(420, 430, 10) named "food30"
  food30.noVelocity = true
  arena1 += food30
  val food31 = Circle(425, 485, 10) named "food31"
  food31.noVelocity = true
  arena1 += food31
  val food32 = Circle(220, 390, 10) named "food32"
  food32.noVelocity = true
  arena1 += food32
  val food33 = Circle(265, 390, 10) named "food33"
  food33.noVelocity = true
  arena1 += food33
  val food34 = Circle(260, 465, 10) named "food34"
  food34.noVelocity = true
  arena1 += food34
  val food35 = Circle(215, 470, 10) named "food35"
  food35.noVelocity = true
  arena1 += food35
  val food36 = Circle(190, 135, 10) named "food36"
  food36.noVelocity = true
  arena1 += food36
  val food37 = Circle(130, 135, 10) named "food37"
  food37.noVelocity = true
  arena1 += food37
  val food38 = Circle(55, 135, 10) named "food38"
  food38.noVelocity = true
  arena1 += food38
  val food39 = Circle(60, 205, 10) named "food39"
  food39.noVelocity = true
  arena1 += food39
  val food40 = Circle(120, 215, 10) named "food40"
  food40.noVelocity = true
  arena1 += food40
  val food41 = Circle(190, 210, 10) named "food41"
  food41.noVelocity = true
  arena1 += food41
  val ennemy1 = Circle(391.151f, 58.598f, 20) named "ennemy1"
  ennemy1.velocity_x = -0.063f
  ennemy1.velocity_y = -0.066f
  ennemy1.color = -55698
  arena1 += ennemy1
  val ennemy2 = Circle(385.36f, 577.431f, 20) named "ennemy2"
  ennemy2.velocity_x = -0.063f
  ennemy2.velocity_y = -0.066f
  ennemy2.color = -55698
  arena1 += ennemy2
  val goal = TextBox(136.4f, -228.448f, 240, 60, "You won !") named "goal"
  arena1 += goal
  val block = TextBox(97.917f, 829.769f, 240, 60, "Game over") named "block"
  arena1 += block
  
  /** Categories and sets */
  var foods = Category(food, food1, food2, food3, food4, food5, food6, food7, food8, food9, food10,
      food11, food12, food13, food14, food15, food16, food17, food18, food19, food20,
      food21, food22, food23, food24, food25, food26, food27, food28, food29, food30,
      food31, food32, food33, food34, food35, food36, food37, food38, food39, food40, food41) named "foods"
  var ennemies = Category(ennemy, ennemy1, ennemy2) named "ennemies"
  
  WhenFingerDownOn(leftArrow) {
    player.angle = -90 // more: ,player.angle += 90
    player.velocity = 0.066f
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "angle"), "$eq"),List(EConstantNumber(-90.0))), EApply(ESelect(ESelect(EIdentShape(player), "angle"), "$plus$eq"),List(EConstantNumber(90.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "velocity"), "$eq"),List(EConstantNumber(0.066848904)))))))
  WhenFingerDownOn(bottomArrow) {
    player.angle = 0 // more: ,player.angle += 90
    player.velocity = 0.068f
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "angle"), "$eq"),List(EConstantNumber(0.0))), EApply(ESelect(ESelect(EIdentShape(player), "angle"), "$plus$eq"),List(EConstantNumber(90.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "velocity"), "$eq"),List(EConstantNumber(0.06853375)))))))
  WhenFingerDownOn(topArrow) {
    player.angle = -180 // more: ,player.angle += 180
    player.velocity = 0.069f
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "angle"), "$eq"),List(EConstantNumber(-180.0))), EApply(ESelect(ESelect(EIdentShape(player), "angle"), "$plus$eq"),List(EConstantNumber(180.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "velocity"), "$eq"),List(EConstantNumber(0.069860876)))))))
  WhenFingerDownOn(rightArrow) {
    player.angle = 90 // more: ,player.angle += 90
    player.velocity = 0.06f
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "angle"), "$eq"),List(EConstantNumber(90.0))), EApply(ESelect(ESelect(EIdentShape(player), "angle"), "$plus$eq"),List(EConstantNumber(90.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "velocity"), "$eq"),List(EConstantNumber(0.060917616)))))))
  NoCollisionBetween(player, wall4)
  NoCollisionBetween(player, wall5)
  
  WhenCollisionBetween(player, wall19) {
    wall18.x = 655 // more: ,wall18.x += 419
    wall18.y = 110 // more: ,wall18.y += -1
    wall19.x = 640 // more: ,wall19.x += 602
    wall19.y = 540 // more: ,wall19.y += 2
    wall19.velocity = 0
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall18), "x"), "$eq"),List(EConstantNumber(655.0))), EApply(ESelect(ESelect(EIdentShape(wall18), "x"), "$plus$eq"),List(EConstantNumber(419.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall18), "y"), "$eq"),List(EConstantNumber(110.0))), EApply(ESelect(ESelect(EIdentShape(wall18), "y"), "$plus$eq"),List(EConstantNumber(-1.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall19), "x"), "$eq"),List(EConstantNumber(640.0))), EApply(ESelect(ESelect(EIdentShape(wall19), "x"), "$plus$eq"),List(EConstantNumber(602.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall19), "y"), "$eq"),List(EConstantNumber(540.0))), EApply(ESelect(ESelect(EIdentShape(wall19), "y"), "$plus$eq"),List(EConstantNumber(2.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall19), "velocity"), "$eq"),List(EConstantNumber(0.0)))))))
  WhenCollisionBetween(player, wall21) {
    wall20.x = 656 // more: ,wall20.x += 484
    wall21.x = 660 // more: ,wall21.x += 387
    wall21.y = 125 // more: ,wall21.y += 3
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall20), "x"), "$eq"),List(EConstantNumber(656.0))), EApply(ESelect(ESelect(EIdentShape(wall20), "x"), "$plus$eq"),List(EConstantNumber(484.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall21), "x"), "$eq"),List(EConstantNumber(660.0))), EApply(ESelect(ESelect(EIdentShape(wall21), "x"), "$plus$eq"),List(EConstantNumber(387.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(wall21), "y"), "$eq"),List(EConstantNumber(125.0))), EApply(ESelect(ESelect(EIdentShape(wall21), "y"), "$plus$eq"),List(EConstantNumber(3.0)))))))
  
  /*var rulesSet = new HashMap[(ReactiveRule, Int), Category[Shape]]
  val foodEnnemyRule1 = NoCollisionBetweenRule(EIdent(""), EIdent(""))
  rulesSet((foodEnnemyRule1, 1)) = ennemies
  rulesSet((foodEnnemyRule1, 2)) = foods
  val foodPlayerRule2 = WhenCollisionBetweenRule(EIdent(""), EIdent(""), List())
  val players = Category(player)*/
  /*val playerFoodRule = WhenCollisionBetweenRule() { (player, food)
    
  }*/
  //rulesSet((playerFoodRule, 1)) = players
  //rulesSet((playerFoodRule, 2)) = foods
  
  foods foreach { case f =>
    NoCollisionBetween(ennemy, f)
    NoCollisionBetween(ennemy1, f)
    NoCollisionBetween(ennemy2, f)
    NoCollisionEffectBetween(player, f)
    WhenCollisionBetween(player, f) {
      score.value += 1 // more: ,score.value = 1
      f.x = 660 // more: ,f.x += 358
      f.y = 265 // more: ,f.y += 185
      foodeaten.value += 1 // more: ,foodeaten.value = score.value - foodeaten.value,foodeaten.value = score.value + foodeaten.value,foodeaten.value = score.value,foodeaten.value = 1
    }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EConstantNumber(1.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(f), "x"), "$eq"),List(EConstantNumber(660.0))), EApply(ESelect(ESelect(EIdentShape(f), "x"), "$plus$eq"),List(EConstantNumber(358.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(f), "y"), "$eq"),List(EConstantNumber(265.0))), EApply(ESelect(ESelect(EIdentShape(f), "y"), "$plus$eq"),List(EConstantNumber(185.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(foodeaten), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(foodeaten), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(foodeaten), "value"), "$minus"),List(ESelect(EIdentShape(foodeaten), "value"))))), EApply(ESelect(ESelect(EIdentShape(foodeaten), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(foodeaten), "value"), "$plus"),List(ESelect(EIdentShape(foodeaten), "value"))))), EApply(ESelect(ESelect(EIdentShape(foodeaten), "value"), "$eq"),List(ESelect(EIdentShape(foodeaten), "value"))), EApply(ESelect(ESelect(EIdentShape(foodeaten), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  }
  ennemies foreach { case ennemy =>
    WhenCollisionBetween(player, ennemy) {
      player.x = 67 // more: ,player.x += -72
      player.y = 67 // more: ,player.y += -72
      player.angle = 0 // more: ,player.angle += 180
      player.velocity = 0
      score.value += -2 // more: ,score.value = -2
      lives.value += -1 // more: ,lives.value = -1
    }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "x"), "$eq"),List(EConstantNumber(67.0))), EApply(ESelect(ESelect(EIdentShape(player), "x"), "$plus$eq"),List(EConstantNumber(-72.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "y"), "$eq"),List(EConstantNumber(67.0))), EApply(ESelect(ESelect(EIdentShape(player), "y"), "$plus$eq"),List(EConstantNumber(-72.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "angle"), "$eq"),List(EConstantNumber(0.0))), EApply(ESelect(ESelect(EIdentShape(player), "angle"), "$plus$eq"),List(EConstantNumber(180.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "velocity"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(EConstantNumber(-2.0))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EConstantNumber(-2.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(lives), "value"), "$plus$eq"),List(EConstantNumber(-1.0))), EApply(ESelect(ESelect(EIdentShape(lives), "value"), "$eq"),List(EConstantNumber(-1.0)))))))
    NoCollisionBetween(ennemy, wall21)
  }
  
  WhenIntegerChanges(foodeaten) { (newValue) =>
    if(newValue == 41) {
      player.angle += 90 // more: ,player.angle = 0
      goal.y = 300 // more: ,goal.y += 528
    }
  }.represents(List(IfCode(EApply(ESelect(EIdent("newValue"), "$eq$eq"),List(EConstantNumber(41.0))),List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "angle"), "$plus$eq"),List(EConstantNumber(90.0))), EApply(ESelect(ESelect(EIdentShape(player), "angle"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(goal), "y"), "$eq"),List(EConstantNumber(300.0))), EApply(ESelect(ESelect(EIdentShape(goal), "y"), "$plus$eq"),List(EConstantNumber(528.0)))))),List())))
  WhenIntegerChanges(lives) { (newValue) =>
    if(newValue == 0) {
      player.x = 666 // more: ,player.x += 599
      player.y = 761 // more: ,player.y += 707
      block.y = 279 // more: ,block.y += -550
    }
  }.represents(List(IfCode(EApply(ESelect(EIdent("newValue"), "$eq$eq"),List(EConstantNumber(0.0))),List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "x"), "$eq"),List(EConstantNumber(666.0))), EApply(ESelect(ESelect(EIdentShape(player), "x"), "$plus$eq"),List(EConstantNumber(599.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "y"), "$eq"),List(EConstantNumber(761.0))), EApply(ESelect(ESelect(EIdentShape(player), "y"), "$plus$eq"),List(EConstantNumber(707.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(block), "y"), "$eq"),List(EConstantNumber(279.0))), EApply(ESelect(ESelect(EIdentShape(block), "y"), "$plus$eq"),List(EConstantNumber(-550.0)))))),List())))
  Camera.x = 0
  Camera.y = 0
  Camera.width = screenWidth
  Camera.height = screenHeight
}