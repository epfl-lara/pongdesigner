package ch.epfl.lara.synthesis.kingpong;

class PongGameSyracuse extends Game {
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
  val score = IntegerBox(50, 215, 60, 60, 8) named "score"
  score.noVelocity = true
  score.color = -19655
  arena1 += score
  val wall4 = Rectangle(25, 180, 425, 25) named "wall4"
  wall4.noVelocity = true
  arena1 += wall4
  val ball1 = Circle(197.218f, 102.05f, 50) named "ball1"
  ball1.velocity_x = 0.197f
  ball1.velocity_y = -0.007f
  arena1 += ball1
  val score4 = IntegerBox(320, 480, 120, 105, 13) named "score4"
  score4.color = -19655
  arena1 += score4
  val ball2 = Circle(413.6f, 668.208f, 50) named "ball2"
  ball2.color = -55698
  arena1 += ball2
  val wall5 = Rectangle(354.277f, 429.863f, 95, 55) named "wall5"
  wall5.noVelocity = true
  arena1 += wall5
  val wall6 = Rectangle(354.875f, 594.868f, 95, 55) named "wall6"
  wall6.noVelocity = true
  arena1 += wall6
  val triggerthreeplusone = IntegerBox(170, 600, 60, 60, 2) named "triggerthreeplusone"
  triggerthreeplusone.noVelocity = true
  triggerthreeplusone.color = -55698
  arena1 += triggerthreeplusone
  val One = IntegerBox(115, 575, 60, 60, 1) named "One"
  arena1 += One
  val copy = IntegerBox(60, 350, 60, 60, 8) named "copy"
  arena1 += copy
  val copy1 = IntegerBox(55, 425, 60, 60, 16) named "copy1"
  arena1 += copy1
  val copy2 = IntegerBox(60, 505, 60, 60, 24) named "copy2"
  arena1 += copy2
  val threeplusone = IntegerBox(50, 645, 60, 60, 25) named "threeplusone"
  threeplusone.color = -55698
  arena1 += threeplusone
  val triggerhalf = IntegerBox(230, 320, 60, 60, 1) named "triggerhalf"
  triggerhalf.color = -15230084
  arena1 += triggerhalf
  val half = IntegerBox(275, 220, 60, 60, 4) named "half"
  half.velocity_x = 0.001f
  half.color = -15230084
  arena1 += half
  val otherhalf = IntegerBox(280, 290, 60, 60, 4) named "otherhalf"
  otherhalf.noVelocity = true
  arena1 += otherhalf
  val basetwo = IntegerBox(280, 370, 60, 60, 8) named "basetwo"
  basetwo.noVelocity = true
  arena1 += basetwo
  val isAlsoOdd = IntegerBox(400, 285, 60, 60, 0) named "isAlsoOdd"
  isAlsoOdd.noVelocity = true
  val isOdd = IntegerBox(400, 215, 60, 60, 0) named "isOdd"
  isOdd.noVelocity = true
  arena1 += isOdd
  val isEven = IntegerBox(400, 350, 60, 60, 1) named "isEven"
  isEven.noVelocity = true
  arena1 += isEven
  arena1 += isAlsoOdd
  
  WhenFingerDownOn(ball2){
    score.value = score4.value
  }.represents(List(EApply(ESelect(ESelect(EIdentShape(score), "value"), "$eq"), List(ESelect(EIdentShape(score4), "value")))))
  WhenFingerDownOn(wall5) {
    score4.value += 1
  }
  WhenFingerDownOn(wall6) {
    score4.value += -1
  }
  WhenCollisionBetween(ball1, wall1) {
    triggerthreeplusone.value += -1
    triggerhalf.value += -1
  }
  WhenIntegerChanges(score) { (oldValue, newValue) =>
    copy.value = newValue
    copy1.value = score.value + copy.value
    copy2.value = copy.value + copy1.value
    threeplusone.value = One.value + copy2.value
  }
  WhenIntegerChanges(triggerthreeplusone) { (oldValue, newValue) =>
    if(newValue == 0) {
      score.value = threeplusone.value
    }
  }
  WhenIntegerChanges(copy) { (oldValue, newValue) =>
    half.value = newValue / 2
    otherhalf.value = newValue / 2
  }
  WhenIntegerChanges(half) { (oldValue, newValue) =>
    basetwo.value = half.value + otherhalf.value
  }
  WhenIntegerChanges(triggerhalf) { (oldValue, newValue) =>
    if(newValue == 0) {
      score.value = half.value
    }
  }
  WhenIntegerChanges(basetwo) { (oldValue, newValue) =>
    isOdd.value = copy.value - basetwo.value
  }
  WhenIntegerChanges(otherhalf) { (oldValue, newValue) =>
    basetwo.value = half.value + otherhalf.value
  }
  WhenIntegerChanges(basetwo) { (oldValue, newValue) =>
    isOdd.value = copy.value - basetwo.value
    isAlsoOdd.value = copy.value - basetwo.value
  }
  WhenIntegerChanges(triggerhalf) { (oldValue, newValue) =>
    if(newValue == 0) {
      score.value = otherhalf.value
      triggerthreeplusone.value += 1
      triggerhalf.value += 1
    }
  }
  WhenIntegerChanges(isEven) { (oldValue, newValue) =>
    if(newValue == 0) {
      triggerthreeplusone.value = 1
      triggerhalf.value = 2
    }
  }
  WhenIntegerChanges(triggerthreeplusone) { (oldValue, newValue) =>
    if(newValue == 0) {
      score.value = threeplusone.value
      triggerthreeplusone.value += 2
    }
  }
  WhenIntegerChanges(isAlsoOdd) { (oldValue, newValue) =>
    if(newValue == 0) {
      triggerthreeplusone.value = 2
      triggerhalf.value = 1
    }
  }
  WhenIntegerChanges(isOdd) { (oldValue, newValue) =>
    isEven.value = One.value - isOdd.value
  }
}