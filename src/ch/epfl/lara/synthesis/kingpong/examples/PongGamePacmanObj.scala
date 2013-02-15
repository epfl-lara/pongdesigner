package ch.epfl.lara.synthesis.kingpong.examples

import scala.collection.mutable.ArrayBuffer
import ch.epfl.lara.synthesis.kingpong.GameShapes._
import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.ast._

class PongGamePacmanObj extends Game {
  /**
   * TODO : Everything after should be in the game engine API
   */
  object Finger {
    def isDownOn(s: Shape): Boolean = {
      true
    }
  }
  class Layout extends Arena {
  }
  def setCurrentLayout(l: Arena) = { }
  val categories = new ArrayBuffer[InstanceSpawner]() // TODO : Move into the game API
  def WhenOverlap(f: PartialFunction[(InstanceSpawner, InstanceSpawner), Unit]) {}
  def destroy(s: Shape) = {}
  def EnableBouncing(s: InstanceSpawner, t: InstanceSpawner) = {}
  /**
   * Everything before should be in the game engine API
   */
  
  /**
   * Game static values
   */
  //layoutWidth = 480
  //layoutHeight = 750
  
  Camera.x = 0
  Camera.y = 0
  Camera.width = 640
  Camera.height = 480
  //Camera.centerObject = null
  
  /**
   * Classes and categories that run on the system
   */
  object Food extends Category[Food]
  object Wall extends Category[Wall]
  object Ennemy extends Category[Ennemy]
  object Player extends Category[Player]
  object Trigger1 extends Category[Trigger1]
  object TriggeredWall1 extends Category[TriggeredWall1]
  object Trigger2 extends Category[Trigger2]
  object TriggeredWall2 extends Category[TriggeredWall2]
  object LeftArrow extends Category[LeftArrow]
  object RightArrow extends Category[RightArrow]
  object TopArrow extends Category[TopArrow]
  object BottomArrow extends Category[BottomArrow]
  object ScoreText extends Category[ScoreText]
  object ScoreValue extends Category[ScoreValue]
  object LivesText extends Category[LivesText]
  object LivesValue extends Category[LivesValue]
  object VictoryText extends Category[LivesText]
  object VictoryValue extends Category[LivesValue]
  
  
  categories += Food
  categories += Wall
  categories += Ennemy
  categories += Player
  categories += Trigger1
  categories += TriggeredWall1
  categories += Trigger2
  categories += TriggeredWall2
  categories += LeftArrow
  categories += RightArrow
  categories += TopArrow
  categories += BottomArrow
  
  //categories = 
  
  /**
   * Class prototypes
   */
  class Food extends Circle {
    radius = 10
    noVelocity = true
    Food += this
  }
  class Wall extends Rectangle {
    Wall += this
  }
  class Trigger1 extends Rectangle {
    Trigger1 += this
  }
  class TriggeredWall1 extends Rectangle {
    TriggeredWall1 += this
  }
  class Trigger2 extends Rectangle {
    Trigger2 += this
  }
  class TriggeredWall2 extends Rectangle {
    TriggeredWall2 += this
  }
  class Ennemy extends Circle {
    radius = 20
    velocity_x = -0.063f
    velocity_y = -0.066f
    color = -55698
    Ennemy += this
  }
  class Player extends Circle {
    Player += this
  }
  class LeftArrow extends TextBox {
    x = 40; y = 662; width = 50; height = 60; text = "\u21E6"
    color = -19655
    LeftArrow += this
  }
  class RightArrow extends TextBox {
    x = 205; y = 660; width = 50; height = 60; text = "\u21E8"
    color = -19655
    layout1 += this
    RightArrow += this
  }
  class TopArrow extends TextBox {
    x = 114; y = 610; width = 50; height = 60; text = "\u21E7"
    color = -19655
    TopArrow += this
  }
  class BottomArrow extends TextBox {
    x = 114; y = 680; width = 50; height = 60; text = "\u21E9"
    color = -19655
    BottomArrow += this
  }
  class ScoreValue extends IntegerBox {
    x = 390; y = 645; width = 40; height = 45
    noVelocity = true
  }
  class ScoreText extends TextBox {
    x = 290; y = 655; width = 110; height = 32
    text = "ScoreValue_1"
    noVelocity = true
  }
  class LivesText extends TextBox {
    x = 290; y = 700; width = 110; height = 32
    text = "Lives:"
    noVelocity = true
  }
  class LivesValue extends IntegerBox {
    x = 390; y = 690; width = 50; height = 50
    value = 4
    noVelocity = true
  }
  class NumFoodEaten extends IntegerBox {
    x = 575; y = 440; width = 60; height = 60; value = 0
    noVelocity = true
  }
  class VictoryText extends TextBox {
    x = 136; y = -228; width = 240; height = 60; text = "You won !";
  }
  class GameOverText extends TextBox {
    x = 98; y = 830; width = 240; height = 60; text = "Game over"; 
  }
  
  /**
   * Game Layouts
   */
  val layout1 = new Arena {
    name = "layout1"
  }
  val Wall_0 = new Wall {
    x = 0; y = 0; width = 25; height = 526; name = "Wall_0"
    noVelocity = true
    color = -15230084
    layout1 += this
  }
  val Wall_1 = new Wall {
    x = 255; y = 0; width = 25; height = 625; name = "Wall_1*"
    noVelocity = true
    color = -15230084
    layout1 += this
  }
  val Wall_2 = new Wall {
    x = 25; y = 0; width = 430; height = 25; name = "Wall_1*"
    noVelocity = true
    color = -15230084
    layout1 += this
  }
  val Wall_3 = new Wall {
    x = 25; y = 600; width = 430; height = 25; name = "Wall_3*"
    noVelocity = true
    color = -15230084
    layout1 += this
  }
  val LeftArrow_1 = new LeftArrow {
    x = 40; y = 662; width = 50; height = 60; text = "\u21E6"; name = "LeftArrow_1"
    color = -19655
    layout1 += this
  }
  val RightArrow_1 = new RightArrow {
    x = 205; y = 660; width = 50; height = 60; text = "\u21E8"; name = "RightArrow_1"
    color = -19655
    layout1 += this
  }
  val BottomArrow_1 = new BottomArrow {
    x = 114; y = 680; width = 50; height = 60; text = "\u21E9"; name = "BottomArrow_1"
    color = -19655
    layout1 += this
  }
  val TopArrow_1 = new TopArrow {
    x = 114; y = 610; width = 50; height = 60; text = "\u21E7"; name = "TopArrow_1"
    color = -19655
    layout1 += this
  }
  val ScoreValue_1 = new ScoreValue {
    x = 390; y = 645; width = 40; height = 45; name = "ScoreValue_1"
    noVelocity = true
    layout1 += this
  }
  val ScoreText_1 = new ScoreText {
    x = 290; y = 655; width = 110; height = 32; name = "ScoreText_1"
    text = "Score:"
    noVelocity = true
    layout1 += this
  }
  val LivesText_1 = new LivesText {
    x = 290; y = 700; width = 110; height = 32; name = "LivesText_1"
    text = "Lives:"
    noVelocity = true
    layout1 += this
  }
  val LivesValue_1 = new LivesValue {
    x = 390; y = 690; width = 50; height = 50; name = "LivesValue_1"
    value = 4
    noVelocity = true
    layout1 += this
  }
  val Wall_4 = new Wall {
    x =113.947f; y = 24.292f; width = 20; height = 80
    name = "Wall_4*"
    noVelocity = true
    color = -16777216
    layout1 += this
  }
  val Wall_5 = new Wall {
    x = 25; y = 85; width = 90; height = 20; name = "Wall_5*"
    noVelocity = true
    color = -16777216
    layout1 += this
  }
  val Player_1 = new Player {
    x = 70; y =40; radius = 20; name = "player"
    color = -19655
    layout1 += this
  }
  val Wall_6 = new Wall {
    x = 140; y = 90; width = 260; height = 20; name = "Wall_6*"
    noVelocity = true
    color = -5266352
    layout1 += this
  }
  
  val NumFoodEaten_1  = new NumFoodEaten {
    x = 575; y = 440; width = 60; height = 60; value = 0; name = "NumFoodEaten_1"
    noVelocity = true
    layout1 += this
  }
  val Wall_7 = new Wall {
    x = 80; y = 165; width = 240; height = 20; name = "Wall_7*"
    noVelocity = true
    color = -5266352
    layout1 += this
  }
  val Wall_8 = new Wall {
    x = 380; y = 110; width = 20; height = 155; name = "Wall_8*"
    noVelocity = true
    color = -5266352
    layout1 += this
  }
  val Wall_9 = new Wall {
    x = 30; y = 245; width = 140; height = 20; name = "Wall_9*"
    noVelocity = true
    color = -5266352
    layout1 += this
  }
  val Wall_10 = new Wall {
    x = 85; y = 325; width = 20; height = 205; name = "Wall_10*"
    noVelocity = true
    color = -5266352
    layout1 += this
  }
  val Wall_11 = new Wall {
    x = 170; y = 325; width = 128; height = 21; name = "Wall_11*"
    noVelocity = true
    color = -5266352
    layout1 += this
  }
  val Wall_12 = new Wall {
    x = 235; y = 245; width = 145; height = 20; name = "Wall_12*"
    noVelocity = true
    color = -5266352
    layout1 += this
  }
  val Wall_13 = new Wall {
    x = 105; y = 420; width = 128; height = 21; name = "Wall_13*"
    noVelocity = true
    color = -5266352
    layout1 += this
  }
  val Wall_14 = new Wall {
    x = 170; y = 510; width = 125; height = 20; name = "Wall_14*"
    noVelocity = true
    color = -5266352
    layout1 += this
  }
  val Wall_15 = new Wall {
    x = 295; y = 325; width = 20; height = 205; name = "Wall_15*"
    noVelocity = true
    color = -5266352
    layout1 += this
  }
  val Wall_16 = new Wall {
    x = 380; y = 330; width = 70; height = 19; name = "Wall_16*"
    noVelocity = true
    color = -5266352
    layout1 += this
  }
  val Wall_17 = new Wall {
    x = 370; y = 415; width = 20; height = 120; name = "Wall_17*"
    noVelocity = true
    color = -5266352
    layout1 += this
  }
  val TriggeredWall1_1 = new TriggeredWall1 {
    x = 237; y = 111; width = 20; height = 130; name = "TriggeredWall1_1"
    noVelocity = true
    color = -19655
    layout1 += this
  }
  val Trigger1_1 = new Trigger1 {
    x = 38; y = 539; width = 30; height = 35; name = "Trigger1_1"
    noVelocity = true
    color = -19655
    layout1 += this
  }
  val TriggeredWall2_1 = new TriggeredWall2 {
    x = 173; y = 352; width = 20; height = 155; name = "TriggeredWall2_1"
    noVelocity = true
    layout1 += this
  }
  val Trigger2_1 = new Trigger2 {
    x = 273; y = 123; width = 30; height = 30; name = "Trigger2_1"
    noVelocity = true
    layout1 += this
  }
  val Food_0 = new Food {
    x = 190; y = 55; name = "Food_0"
    layout1 += this
  }
  val Food_1 = new Food{
    x = 265; y = 55; name = "Food_1"
    layout1 += this
  }
  val Food_2 = new Food {
    x = 345; y = 50; name = "Food_2"
    layout1 += this
  }
  val Food_3 = new Food {
    x = 205; y = 295; name = "Food_3"
    layout1 += this
  }
  val Food_4 = new Food {
    x = 430; y = 50; name = "Food_4"
    layout1 += this
  }
  val Food_5 = new Food {
    x = 430; y = 130; name = "Food_5"
    layout1 += this
  }
  val Food_6 = new Food {
    x = 570; y = 125; name = "Food_6"
    layout1 += this
  }
  val Food_7 = new Food {
    x = 425; y = 215; name = "Food_7"
    layout1 += this
  }
  val Food_8 = new Food {
    x = 420; y = 300; name = "Food_8"
    layout1 += this
  }
  val Food_9 = new Food {
    x = 285; y = 220; name = "Food_9"
    layout1 += this
  }
  val Food_10 = new Food {
    x = 350; y = 295; name = "Food_10"
    layout1 += this
  }
  val Food_11 = new Food {
    x = 345; y = 210; name = "Food_11"
    layout1 += this
  }
  val Food_12 = new Food {
    x = 275; y = 295; name = "Food_12"
    layout1 += this
  }
  val Food_13 = new Food {
    x = 135; y = 295; name = "Food_13"
    layout1 += this
  }
  val Food_14 = new Food {
    x = 350; y = 140; name = "Food_14"
    layout1 += this
  }
  val Food_15 = new Food {
    x = 50; y = 295; name = "Food_15"
    layout1 += this
  }
  val Food_16 = new Food {
    x = 55; y = 360; name = "Food_16"
    layout1 += this
  }
  val Food_17 = new Food {
    x = 140; y = 380; name = "Food_17"
    layout1 += this
  }
  val Food_18 = new Food {
    x = 50; y = 425; name = "Food_18"
    layout1 += this
  }
  val Food_19 = new Food {
    x = 60; y = 485; name = "Food_19"
    layout1 += this
  }
  val Food_20 = new Food {
    x = 130; y = 495; name = "Food_20"
    layout1 += this
  }
  val Food_21 = new Food {
    x = 125; y = 565; name = "Food_21"
    layout1 += this
  }
  val Food_22 = new Food {
    x = 205; y = 570; name = "Food_22"
    layout1 += this
  }
  val Food_23 = new Food {
    x = 285; y = 565; name = "Food_23"
    layout1 += this
  }
  val Food_24 = new Food {
    x = 415; y = 560; name = "Food_24"
    layout1 += this
  }
  val Food_25 = new Food {
    x = 340; y = 565; name = "Food_25"
    layout1 += this
  }
  val Food_26 = new Food {
    x = 340; y = 498.414f; name = "Food_26"
    layout1 += this
  }
  val Food_27 = new Food {
    x = 345; y = 365; name = "Food_27"
    layout1 += this
  }
  val Food_28 = new Food {
    x = 345; y = 435; name = "Food_28"
    layout1 += this
  }
  val Food_29 = new Food {
    x = 415; y = 375; name = "Food_29"
    layout1 += this
  }
  val Food_30 = new Food {
    x = 420; y = 430; name = "Food_30"
    layout1 += this
  }
  val Food_31 = new Food {
    x = 425; y = 485; name = "Food_31"
    layout1 += this
  }
  val Food_32 = new Food {
    x = 220; y = 390; name = "Food_32"
    layout1 += this
  }
  val Food_33 = new Food {
    x = 265; y = 390; name = "Food_33"
    layout1 += this
  }
  val Food_34 = new Food {
    x = 260; y = 465; name = "Food_34"
    layout1 += this
  }
  val Food_35 = new Food {
    x = 215; y = 470; name = "Food_35"
    layout1 += this
  }
  val Food_36 = new Food {
    x = 190; y = 135; name = "Food_36"
    layout1 += this
  }
  val Food_37 = new Food {
    x = 130; y = 135; name = "Food_37"
    layout1 += this
  }
  val Food_38 = new Food {
    x = 55; y = 135; name = "Food_38"
    layout1 += this
  }
  val Food_39 = new Food {
    x = 60; y = 205; name = "Food_39"
    layout1 += this
  }
  val Food_40 = new Food {
    x = 120; y = 215; name = "Food_40"
    layout1 += this
  }
  val Food_41 = new Food {
    x = 190; y = 210; name = "Food_41"
    layout1 += this
  }
  val Ennemy_1 = new Ennemy {
    x = 130; y = 330; name = "Ennemy_1"
    radius = 20; 
    velocity_x = -0.063f
    velocity_y = -0.066f
    color = -55698
    layout1 += this
  }
  val Ennemy_2 = new Ennemy {
    x = 391; y = 59; radius = 20; name = "Ennemy_2"
    velocity_x = -0.063f
    velocity_y = -0.066f
    color = -55698
    layout1 += this
  }
  val Ennemy_3 = new Ennemy {
    x = 385; y = 577; radius = 20; name = "Ennemy_3"
    velocity_x = -0.063f
    velocity_y = -0.066f
    color = -55698
    layout1 += this
  }
  val VictoryText_1 = new VictoryText {
    x = 136; y = -228; width = 240; height = 60; text = "You won !"; name = "VictoryText_1"
    layout1 += this
  }
  val GameOverText_1 = new GameOverText {
    x = 98; y = 830; width = 240; height = 60; text = "Game over"; name = "GameOverText_1"
    layout1 += this
  }
  setCurrentLayout(layout1)
  
  // Constants that can me modified.
  // Graphically, would look like a number
  var velocity_pacman = 0.066f
  
  /* Code called at every frame */
  override def everyFrame() = {
    // Graphically, would look like  ScoreText = "Score:" + str(ScoreValue)
    ScoreText_1.text = "Score:" + ScoreValue_1.value
    LivesText_1.text = "Lives:" + LivesValue_1.value
    // Graphically, would look like  Finger *down on object* LeftArrow { code }
    for(leftArrow <- LeftArrow if Finger.isDownOn(leftArrow)) {
      Player_1.angle = -90
      Player_1.velocity = velocity_pacman
    }
    for(bottomArrow <- BottomArrow if Finger.isDownOn(bottomArrow)) {
      Player_1.angle = 0
      Player_1.velocity = velocity_pacman
    }
    for(topArrow <- TopArrow if Finger.isDownOn(topArrow)) {
      Player_1.angle = -180
      Player_1.velocity = velocity_pacman
    }
    for(rightArrow <- RightArrow if Finger.isDownOn(rightArrow)) {
      Player_1.angle = 90
      Player_1.velocity = velocity_pacman
    }
    // Graphically, would look like  Player *Collision* Food { code }
    WhenOverlap{ case (p: Player, f: Food) =>
      destroy(f)
      NumFoodEaten_1.value = NumFoodEaten_1.prev_value + 1
    }
    // Graphically, would look like  Player *Collision* Trigger1 { code }
    WhenOverlap{ case (p: Player, t: Trigger1) =>
      TriggeredWall1_1.x = 655 // more: ,TriggeredWall1_1.x += 419
      TriggeredWall1_1.y = 110 // more: ,TriggeredWall1_1.y += -1
      t.x = 640 // more: ,Trigger1_1.x += 602
      t.y = 540 // more: ,Trigger1_1.y += 2
      t.velocity = 0
    }
    // Graphically, would look like  Player *Collision* Trigger2 { code }
    WhenOverlap{ case (p: Player, t: Trigger2) =>
      TriggeredWall2_1.x = 656 // more: ,TriggeredWall2_1.x += 484
      t.x = 660 // more: ,Trigger2_1.x += 387
      t.y = 125 // more: ,Trigger2_1.y += 3
    }
    // Graphically, would look like  Player *Collision* Ennemy { code }
    WhenOverlap{ case (p: Player, e: Ennemy) =>
      p.x = 67 // more: ,player.x += -72
      p.y = 67 // more: ,player.y += -72
      p.angle = 0 // more: ,player.angle += 180
      p.velocity = 0
      ScoreValue_1.value += -2 // more: ,ScoreValue_1.value = -2
      LivesValue_1.value += -1 // more: ,LivesValue_1.value = -1
    }
    // Graphically, would look like Food *on size = 0* { code }
    if(Food.size == 0 && Food.size_prev != 0) { // Inside the game : onChangeEvent
      Player_1.angle += 90 // more: ,player.angle = 0
      VictoryText_1.y = 300 // more: ,VictoryText_1.y += 528
    }
    // Graphically, would look like LivesValue *on value = 0* { code }
    if(LivesValue_1.value == 0 && LivesValue_1.prev_value != 0) {
      Player_1.x = 666 // more: ,player.x += 599
      Player_1.y = 761 // more: ,player.y += 707
      GameOverText_1.y = 279 // more: ,GameOverText_1.y += -550
    }
  }
  
  /**
   * Layers and collisions.
   * How is is done with Box2D ? we should adapt the collision system.
   */
  // Graphically, would look like Player *Bounce* Wall
  EnableBouncing(Player, Wall)
  // Graphically, would look like Ennemy *Bounce* Wall
  EnableBouncing(Ennemy, Wall)
}
