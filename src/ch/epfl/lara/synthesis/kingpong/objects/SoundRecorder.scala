package ch.epfl.lara.synthesis.kingpong.objects

import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer

import org.jbox2d.collision.shapes.PolygonShape

import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._

/**
 * An element drawn at a specific time.
 */
case class SoundRecorded(var recorder: SoundRecorder,
    number: Int,
    startTime: Int,
    endTime: Int,
    uri: String) extends GameObject(recorder.name.get + "[" + number + "]", GameObject.PLANNED_SINCE_BEGINNING) 
      with Rectangular with Positionable with Directionable with FixedRectangularContains {
  
  private val shape = new PolygonShape()
  def noVelocity_=(b: Boolean) = {}
  def game = recorder.game
  
  var duration: Int = Int.MaxValue
  val x = aliasProperty[Float] (
    name  = "x", 
    getF  = () => recorder.x.get  + width.get  * (number + 1),
    nextF = () => recorder.x.next + width.next * (number + 1),
    exprF = () => recorder.x.expr + width.expr * (number + 1) 
  )
  
  val y = aliasProperty[Float] (
    name  = "y", 
    getF  = () => recorder.y.get,
    nextF = () => recorder.y.next,
    exprF = () => recorder.y.expr
  )
  
  val width = aliasProperty[Float] (
    name  = "y", 
    getF  = () => recorder.width.get,
    nextF = () => recorder.width.next,
    exprF = () => recorder.width.expr
  )
  
  val height = aliasProperty[Float] (
    name  = "y", 
    getF  = () => recorder.height.get,
    nextF = () => recorder.height.next,
    exprF = () => recorder.height.expr
  )
  
  val angle = proxyProperty(recorder.angle)
  val color = proxyProperty(recorder.color)
  val visible = proxyProperty(recorder.visible)
  
  def getShape = {
    shape.setAsBox(width.get/2, height.get/2, Vec2(x.get, y.get), 0f)
    shape
  }
  
  def getAABB() = {
    val bottomLeft = Vec2(left.get, top.get)
    val upperRight = bottomLeft add Vec2(width.get, height.get)
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }
    
  //TODO we cannot copy a recorded sound
  protected def rawCopy(f: HistoricalProperty[_] => Expr) = ???
}

class SoundRecorder (
    val game: Game,
    init_name: Expr, 
    init_x: Expr,
    init_y: Expr,
    init_angle: Expr,
    init_width: Expr, 
    init_height: Expr,
    init_visible: Expr,
    init_color: Expr,
    init_recording: Expr,
    planned: GameObject.IsPlanned
    ) extends AbstractObject(init_name, init_x, init_y, init_angle, init_visible, init_color, planned)
      with ResizableRectangular
      with Movable
      with Visiblable
      with Colorable
      with Directionable with FixedRectangularContains {
  
  @BeanProperty lazy val recordings = ArrayBuffer.tabulate[SoundRecorded](0)(n => null)

  val width = simpleProperty[Float]("width", init_width)
  val height = simpleProperty[Float]("height", init_height)
  val recording = simpleProperty[Boolean]("recording", init_recording)

  def numRecords = recordings.length
  
  // --------------------------------------------------------------------------
  // Utility functions
  // --------------------------------------------------------------------------  
  
  def getAABB = {
    val bottomLeft = Vec2(x.get - width.get/2, y.get - height.get/2)
    val upperRight = bottomLeft add Vec2(width.get, height.get)
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }
  
  private val shape = new PolygonShape()
  
  def getShape = {
    shape.setAsBox(width.get/2, height.get/2, Vec2(x.get, y.get), 0f)
    shape
  }

  def rawCopy(f: HistoricalProperty[_] => Expr) = {
    new SoundRecorder(game, f(name), f(x), f(y), f(angle), f(width), f(height), f(visible), f(color), f(recording), game.isCopyingPlanned())
  }
}
