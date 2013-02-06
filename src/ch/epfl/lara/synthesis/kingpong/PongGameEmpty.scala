package ch.epfl.lara.synthesis.kingpong;

/**
 * Simplest version of Pong game - where the score is to be added.
 */

class PongGameEmpty extends Game {
  import GameShapes._

  /**
   * Game static values
   */
  var screenWidth = 480
  var screenHeight = 750

  /** Game Layouts */
  var arena1 = Arena() named "arena1"
  
  setCurrentArena(arena1)
  
  //val arena1 = Arena()
  /*val ball = Circle(235.70345f, 496.10297f, 50) named "ball"
  arena1 += ball*/
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
  /*val ball1 = Circle(204.63803f, 242.68817f, 50) named "ball1"
  arena1 += ball1*/

  /**
   * Rules to be guessed.
   */

  Camera.x = 0
  Camera.y = 0
  Camera.width = screenWidth
  Camera.height = screenHeight
}