package ch.epfl.lara.synthesis.kingpong;
import scala.collection.mutable.Set
import scala.collection.mutable.HashMap
import GameShapes._



class PongGamePacmanObj extends Game {
  /**
   * Game static values
   */
  var screenWidth = 480
  var screenHeight = 750
  
  /**
   * Game Layouts
   */
  object arena1 extends Arena {
    name = "arena1"
  }
  object wall extends Rectangle {
    x = 0; y = 0; width = 25; height = 526; name = "wall"
    noVelocity = true
    color = -15230084
    arena1 += this
  }
  object wall1 extends Rectangle {
    x = 255; y = 0; width = 25; height = 625; name = "wall1"
    noVelocity = true
    color = -15230084
    arena1 += this
  }
  object wall2 extends Rectangle {
    x = 25; y = 0; width = 430; height = 25; name = "wall1"
    noVelocity = true
    color = -15230084
    arena1 += this
  }
  object wall3 extends Rectangle {
    x = 25; y = 600; width = 430; height = 25; name = "wall3"
    noVelocity = true
    color = -15230084
    arena1 += this
  }
  object leftArrow extends TextBox {
    x = 40; y = 662; width = 50; height = 60; text = "\u21E6"; name = "leftArrow"
    color = -19655
    arena1 += this
  }
  object rightArrow extends TextBox {
    x = 205; y = 660; width = 50; height = 60; text = "\u21E8"; name = "rightArrow"
    color = -19655
    arena1 += this
  }
  object bottomArrow extends TextBox {
    x = 114; y = 680; width = 50; height = 60; text = "\u21E9"; name = "bottomArrow"
    color = -19655
    arena1 += this
  }
  object topArrow extends TextBox {
    x = 114; y = 610; width = 50; height = 60; text = "\u21E7"; name = "topArrow"
    color = -19655
    arena1 += this
  }
  object score extends IntegerBox {
    x = 390; y = 645; width = 40; height = 45; name = "score"
    noVelocity = true
    arena1 += this
  }
  object scorePlayer extends TextBox {
    x = 290; y = 655; width = 110; height = 32; name = "scorePlayer"
    text = "score"
    noVelocity = true
    arena1 += this  
  }
  object scorePlayer1 extends TextBox {
    x = 290; y = 700; width = 110; height = 32; name = "scorePlayer1"
    text = "Lives"
    noVelocity = true
    arena1 += this
  }
  object lives extends IntegerBox {
    x = 390; y = 690; width = 50; height = 50; name = "lives"
    value = 4
    noVelocity = true
    arena1 += this
  }
  object wall4 extends Rectangle {
    x =113.947f; y = 24.292f; width = 20; height = 80
    name = "wall4"
    noVelocity = true
    color = -16777216
    arena1 += this
  }
  object wall5 extends Rectangle {
    x = 25; y = 85; width = 90; height = 20; name = "wall5"
    noVelocity = true
    color = -16777216
    arena1 += this
  }
  object player extends Circle {
    x = 70; y =40; radius = 20; name = "player"
    color = -19655
    arena1 += this
  }
  object food extends Circle {
    x = 190; y = 55; radius = 10; name = "food"
    noVelocity = true
    arena1 += this
  }
  object wall6 extends Rectangle {
    x = 140; y = 90; width = 260; height = 20; name = "wall6"
    wall6.noVelocity = true
    wall6.color = -5266352
    arena1 += this
  }
  object foodeaten extends IntegerBox {
    x = 575; y = 440; width = 60; height = 60; value = 0; name = "foodeaten"
    noVelocity = true
    arena1 += this
  }
  object wall7 extends Rectangle {
    x = 80; y = 165; width = 240; height = 20; name = "wall7"
    noVelocity = true
    color = -5266352
    arena1 += this
  }
  object wall8 extends Rectangle {
    x = 380; y = 110; width = 20; height = 155; name = "wall8"
    noVelocity = true
    color = -5266352
    arena1 += this
  }
  object wall9 extends Rectangle {
    x = 30; y = 245; width = 140; height = 20; name = "wall9"
    noVelocity = true
    color = -5266352
    arena1 += this
  }
  object wall10 extends Rectangle {
    x = 85; y = 325; width = 20; height = 205; name = "wall10"
    noVelocity = true
    color = -5266352
    arena1 += this
  }
  object wall11 extends Rectangle {
    x = 170; y = 325; width = 128; height = 21; name = "wall11"
    noVelocity = true
    color = -5266352
    arena1 += this
  }
  object wall12 extends Rectangle {
    x = 235; y = 245; width = 145; height = 20; name = "wall12"
    noVelocity = true
    color = -5266352
    arena1 += this
  }
  object wall13 extends Rectangle {
    x = 105; y = 420; width = 128; height = 21; name = "wall13"
    noVelocity = true
    color = -5266352
    arena1 += this
  }
  object wall14 extends Rectangle {
    x = 170; y = 510; width = 125; height = 20; name = "wall14"
    noVelocity = true
    color = -5266352
    arena1 += this
  }
  object wall15 extends Rectangle {
    x = 295; y = 325; width = 20; height = 205; name = "wall15"
    noVelocity = true
    color = -5266352
    arena1 += this
  }
  object wall16 extends Rectangle {
    x = 380; y = 330; width = 70; height = 19; name = "wall16"
    noVelocity = true
    color = -5266352
    arena1 += this
  }
  object wall17 extends Rectangle {
    x = 370; y = 415; width = 20; height = 120; name = "wall17"
    noVelocity = true
    color = -5266352
    arena1 += this
  }
  object wall18 extends Rectangle {
    x = 237; y = 111; width = 20; height = 130; name = "wall18"
    noVelocity = true
    color = -19655
    arena1 += this
  }
  object wall19 extends Rectangle {
    x = 38; y = 539; width = 30; height = 35; name = "wall19"
    noVelocity = true
    color = -19655
    arena1 += this
  }
  object wall20 extends Rectangle {
    x = 173; y = 352; width = 20; height = 155; name = "wall20"
    noVelocity = true
    arena1 += this
  }
  object wall21 extends Rectangle {
    x = 273; y = 123; width = 30; height = 30; name = "wall21"
    noVelocity = true
    arena1 += this
  }
  object food1 extends Circle {
    x = 265; y = 55; radius = 10; name = "food1"
    noVelocity = true
    arena1 += food1
  }
  object food2 extends Circle {
    x = 345; y = 50; radius = 10; name = "food2"
    noVelocity = true
    arena1 += food2
  }
  object food3 extends Circle {
    x = 205; y = 295; radius = 10; name = "food3"
    noVelocity = true
    arena1 += food3
  }
  object food4 extends Circle {
    x = 430; y = 50; radius = 10; name = "food4"
    noVelocity = true
    arena1 += food4
  }
  object food5 extends Circle {
    x = 430; y = 130; radius = 10; name = "food5"
    noVelocity = true
    arena1 += food5
  }
  object food6 extends Circle {
    x = 570; y = 125; radius = 10; name = "food6"
    noVelocity = true
    arena1 += food6
  }
  object food7 extends Circle {
    x = 425; y = 215; radius = 10; name = "food7"
    noVelocity = true
    arena1 += food7
  }
  object food8 extends Circle {
    x = 420; y = 300; radius = 10; name = "food8"
    noVelocity = true
    arena1 += food8
  }
  object food9 extends Circle {
    x = 285; y = 220; radius = 10; name = "food9"
    noVelocity = true
    arena1 += food9
  }
  object food10 extends Circle {
    x = 350; y = 295; radius = 10; name = "food10"
    noVelocity = true
    arena1 += food10
  }
  object food11 extends Circle {
    x = 345; y = 210; radius = 10; name = "food11"
    noVelocity = true
    arena1 += food11
  }
  object food12 extends Circle {
    x = 275; y = 295; radius = 10; name = "food12"
    noVelocity = true
    arena1 += food12
  }
  object food13 extends Circle {
    x = 135; y = 295; radius = 10; name = "food13"
    noVelocity = true
    arena1 += food13
  }
  object food14 extends Circle {
    x = 350; y = 140; radius = 10; name = "food14"
    noVelocity = true
    arena1 += food14
  }
  object food15 extends Circle {
    x = 50; y = 295; radius = 10; name = "food15"
    noVelocity = true
    arena1 += food15
  }
  object food16 extends Circle {
    x = 55; y = 360; radius = 10; name = "food16"
    noVelocity = true
    arena1 += food16
  }
  object food17 extends Circle {
    x = 140; y = 380; radius = 10; name = "food17"
    noVelocity = true
    arena1 += food17
  }
  object food18 extends Circle {
    x = 50; y = 425; radius = 10; name = "food18"
    noVelocity = true
    arena1 += food18
  }
  object food19 extends Circle {
    x = 60; y = 485; radius = 10; name = "food19"
    noVelocity = true
    arena1 += food19
  }
  object food20 extends Circle {
    x = 130; y = 495; radius = 10; name = "food20"
    noVelocity = true
    arena1 += food20
  }
  object food21 extends Circle {
    x = 125; y = 565; radius = 10; name = "food21"
    noVelocity = true
    arena1 += food21
  }
  object food22 extends Circle {
    x = 205; y = 570; radius = 10; name = "food22"
    noVelocity = true
    arena1 += food22
  }
  object food23 extends Circle {
    x = 285; y = 565; radius = 10; name = "food23"
    noVelocity = true
    arena1 += food23
  }
  object food24 extends Circle {
    x = 415; y = 560; radius = 10; name = "food24"
    noVelocity = true
    arena1 += food24
  }
  object food25 extends Circle {
    x = 340; y = 565; radius = 10; name = "food25"
    noVelocity = true
    arena1 += food25
  }
  object food26 extends Circle {
    x = 340; y = 498.414f; radius = 10; name = "food26"
    noVelocity = true
    arena1 += food26
  }
  object food27 extends Circle {
    x = 345; y = 365; radius = 10; name = "food27"
    noVelocity = true
    arena1 += food27
  }
  object food28 extends Circle {
    x = 345; y = 435; radius = 10; name = "food28"
    noVelocity = true
    arena1 += food28
  }
  object food29 extends Circle {
    x = 415; y = 375; radius = 10; name = "food29"
    noVelocity = true
    arena1 += food29
  }
  object food30 extends Circle {
    x = 420; y = 430; radius = 10; name = "food30"
    noVelocity = true
    arena1 += food30
  }
  object food31 extends Circle {
    x = 425; y = 485; radius = 10; name = "food31"
    noVelocity = true
    arena1 += food31
  }
  object food32 extends Circle {
    x = 220; y = 390; radius = 10; name = "food32"
    noVelocity = true
    arena1 += food32
  }
  object food33 extends Circle {
    x = 265; y = 390; radius = 10; name = "food33"
    noVelocity = true
    arena1 += food33
  }
  object food34 extends Circle {
    x = 260; y = 465; radius = 10; name = "food34"
    noVelocity = true
    arena1 += food34
  }
  object food35 extends Circle {
    x = 215; y = 470; radius = 10; name = "food35"
    noVelocity = true
    arena1 += food35
  }
  object food36 extends Circle {
    x = 190; y = 135; radius = 10; name = "food36"
    noVelocity = true
    arena1 += food36
  }
  object food37 extends Circle {
    x = 130; y = 135; radius = 10; name = "food37"
    noVelocity = true
    arena1 += food37
  }
  object food38 extends Circle {
    x = 55; y = 135; radius = 10; name = "food38"
    noVelocity = true
    arena1 += food38
  }
  object food39 extends Circle {
    x = 60; y = 205; radius = 10; name = "food39"
    noVelocity = true
    arena1 += food39
  }
  object food40 extends Circle {
    x = 120; y = 215; radius = 10; name = "food40"
    noVelocity = true
    arena1 += food40
  }
  object food41 extends Circle {
    x = 190; y = 210; radius = 10; name = "food41"
    noVelocity = true
    arena1 += food41
  }
  object ennemy extends Circle {
    x = 130; y = 330; radius = 20; name = "ennemy"
    velocity_x = -0.063f
    velocity_y = -0.066f
    color = -55698
    arena1 += this
  }
  object ennemy1 extends Circle {
    x = 391; y = 59; radius = 20; name = "ennemy1"
    velocity_x = -0.063f
    velocity_y = -0.066f
    color = -55698
    arena1 += this
  }
  object ennemy2 extends Circle {
    x = 385; y = 577; radius = 20; name = "ennemy2"
    velocity_x = -0.063f
    velocity_y = -0.066f
    color = -55698
    arena1 += this
  }
  object goal extends TextBox {
    x = 136; y = -228; width = 240; height = 60; text = "You won !"; name = "goal"
    arena1 += this
  }
  object block extends TextBox {
    x = 98; y = 830; width = 240; height = 60; text = "Game over"; name = "block"
    arena1 += this
  }
  setCurrentArena(arena1)
  
  /** Categories and sets */
  var foods = Category(food, food1, food2, food3, food4, food5, food6, food7, food8, food9, food10,
      food11, food12, food13, food14, food15, food16, food17, food18, food19, food20,
      food21, food22, food23, food24, food25, food26, food27, food28, food29, food30,
      food31, food32, food33, food34, food35, food36, food37, food38, food39, food40, food41) named "foods"
  var ennemies = Category(ennemy, ennemy1, ennemy2) named "ennemies"
  var players = Category(player) named "players"
  var leftArrows = Category(leftArrow) named "leftArrows"
  var topArrows = Category(topArrow) named "topArrows"
  var rightArrows = Category(rightArrow) named "rightArrows"
  var bottomArrows = Category(bottomArrow) named "bottomArrows"
  var codeLeft = CompiledProgram({ leftArrow: TextBox => 
    player.angle = -90
    player.velocity = 0.066f
  })
  /*
  var codePlayerFood = CompiledProgram{ (f: Circle, p: Circle) =>
    score.value += 1 // more: ,score.value = 1
    f.x = 660 // more: ,f.x += 358
    f.y = 265 // more: ,f.y += 185
    foodeaten.value += 1 // more: ,foodeaten.value = score.value - foodeaten.value,foodeaten.value = score.value + foodeaten.value,foodeaten.value = score.value,foodeaten.value = 1
  }.represents(List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"),List(EConstantNumber(1.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(f), "x"), "$eq"),List(EConstantNumber(660.0))), EApply(ESelect(ESelect(EIdentShape(f), "x"), "$plus$eq"),List(EConstantNumber(358.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(f), "y"), "$eq"),List(EConstantNumber(265.0))), EApply(ESelect(ESelect(EIdentShape(f), "y"), "$plus$eq"),List(EConstantNumber(185.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(foodeaten), "value"), "$plus$eq"),List(EConstantNumber(1.0))), EApply(ESelect(ESelect(EIdentShape(foodeaten), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(foodeaten), "value"), "$minus"),List(ESelect(EIdentShape(foodeaten), "value"))))), EApply(ESelect(ESelect(EIdentShape(foodeaten), "value"), "$eq"),List(EApply(ESelect(ESelect(EIdentShape(foodeaten), "value"), "$plus"),List(ESelect(EIdentShape(foodeaten), "value"))))), EApply(ESelect(ESelect(EIdentShape(foodeaten), "value"), "$eq"),List(ESelect(EIdentShape(foodeaten), "value"))), EApply(ESelect(ESelect(EIdentShape(foodeaten), "value"), "$eq"),List(EConstantNumber(1.0)))))))
  
  val foodEnnemyRule1 = NoCollisionRule(ennemies, foods) named "foodEnnemyRule1"
  val foodPlayerRule2 = WhenCollisionBetweenRule(foods, players) named "foodPlayerRule2"
  val leftArrowRule = WhenFingerDownOnRule(leftArrows, codeLeft) named "leftArrowRule"
  
  // Continue here
  
  rules += foodEnnemyRule1
  rules += foodPlayerRule2
  
  */
  //var rulesSet = new HashMap[(ReactiveRule, Int), Category]
  //rulesSet((foodEnnemyRule1, 1)) = ennemies
  //rulesSet((foodEnnemyRule1, 2)) = foods
  
  WhenFingerDownOn(leftArrow) {
    player.angle = -90 // more: ,player.angle += 90
    player.velocity = 0.066f
  }
  WhenFingerDownOn(bottomArrow) {
    player.angle = 0 // more: ,player.angle += 90
    player.velocity = 0.068f
  }
  WhenFingerDownOn(topArrow) {
    player.angle = -180 // more: ,player.angle += 180
    player.velocity = 0.069f
  }
  WhenFingerDownOn(rightArrow) {
    player.angle = 90 // more: ,player.angle += 90
    player.velocity = 0.06f
  }
  NoCollisionBetween(player, wall4)
  NoCollisionBetween(player, wall5)
  
  WhenCollisionBetween(player, wall19) {
    wall18.x = 655 // more: ,wall18.x += 419
    wall18.y = 110 // more: ,wall18.y += -1
    wall19.x = 640 // more: ,wall19.x += 602
    wall19.y = 540 // more: ,wall19.y += 2
    wall19.velocity = 0
  }
  WhenCollisionBetween(player, wall21) {
    wall20.x = 656 // more: ,wall20.x += 484
    wall21.x = 660 // more: ,wall21.x += 387
    wall21.y = 125 // more: ,wall21.y += 3
  }
  
  /*val playerFoodRule = WhenCollisionBetweenRule() { (player, food)
    
  }*/
  //rulesSet((playerFoodRule, 1)) = players
  //rulesSet((playerFoodRule, 2)) = foods
  
  foods foreach { case f =>
    NoCollisionBetween(ennemy, f)
    NoCollisionBetween(ennemy1, f)
    NoCollisionBetween(ennemy2, f)
    NoCollisionEffectBetween(player, f)
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
  
  WhenIntegerChanges(foodeaten) { (oldValue, newValue) =>
    if(newValue == 41) {
      player.angle += 90 // more: ,player.angle = 0
      goal.y = 300 // more: ,goal.y += 528
    }
  }.represents(List(IfCode(EApply(ESelect(EIdent("newValue"), "$eq$eq"),List(EConstantNumber(41.0))),List(ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(player), "angle"), "$plus$eq"),List(EConstantNumber(90.0))), EApply(ESelect(ESelect(EIdentShape(player), "angle"), "$eq"),List(EConstantNumber(0.0))))), ParallelExpressions(List(EApply(ESelect(ESelect(EIdentShape(goal), "y"), "$eq"),List(EConstantNumber(300.0))), EApply(ESelect(ESelect(EIdentShape(goal), "y"), "$plus$eq"),List(EConstantNumber(528.0)))))),List())))
  WhenIntegerChanges(lives) { (oldValue, newValue) =>
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
