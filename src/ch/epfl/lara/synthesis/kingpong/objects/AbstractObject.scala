package ch.epfl.lara.synthesis.kingpong.objects

import android.util.Log
import org.jbox2d.collision.shapes.PolygonShape
import org.jbox2d.collision.shapes.CircleShape
import org.jbox2d.collision.shapes.Shape
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.common.Random
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.rules.Context
import ch.epfl.lara.synthesis.kingpong.rules.Events
import scala.collection.mutable.ArrayBuffer

abstract class AbstractObject(init_name: Expr, 
                              init_x: Expr,
                              init_y: Expr,
                              init_angle: Expr,
                              init_visible: Expr, 
                              init_color: Expr
                             ) extends GameObject(init_name) {
  
  def noVelocity_=(b: Boolean): Unit = {}
  val x = simpleProperty[Float]("x", init_x)
  val y = simpleProperty[Float]("y", init_y)
  val angle = simpleProperty[Float]("angle", init_angle)
  val visible = simpleProperty[Boolean]("visible", init_visible)
  val color = simpleProperty[Int]("color", init_color)
  
  def getShape: Shape
}


sealed abstract class Box[T : PongType](
    val game: Game,
    init_name: Expr, 
    init_x: Expr,
    init_y: Expr,
    init_angle: Expr,
    init_width: Expr, 
    init_height: Expr, 
    init_value: Expr,
    init_visible: Expr,
    init_color: Expr
   ) extends AbstractObject(init_name, init_x, init_y, init_angle, init_visible, init_color)
     with Rectangular
     with Movable
     with Rotationable
     with Visiblable
     with Colorable
     with AngularRectangularContains {
  
  private val shape = new PolygonShape()
  
  val width = simpleProperty[Float]("width", init_width)
  val height = simpleProperty[Float]("height", init_height)
  val value = simpleProperty[T]("value", init_value)
  
  def getAABB = {
    //TODO fix this
    val bottomLeft = Vec2(x.get, y.get)
    val upperRight = Vec2(x.get + width.get, y.get + height.get)
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }
  
  def getShape = {
    shape.setAsBox(width.get/2, height.get/2, Vec2(x.get, y.get), 0f)
    shape
  }
}

class IntBox(
    game: Game,
    init_name: Expr, 
    init_x: Expr,
    init_y: Expr,
    init_angle: Expr,
    init_width: Expr, 
    init_height: Expr, 
    init_value: Expr,
    init_visible: Expr,
    init_color: Expr
   ) extends Box[Int](game, init_name, init_x, init_y, init_angle, init_width, init_height, 
                      init_value, init_visible, init_color) {
  
  def makecopy(name: String): GameObject = {
    new IntBox(game, name, x.init, y.init, angle.init, width.init,  height.init, 
               value.init, visible.init, color.init)
  }
}

class StringBox(
    game: Game,
    init_name: Expr, 
    init_x: Expr,
    init_y: Expr,
    init_angle: Expr,
    init_width: Expr, 
    init_height: Expr, 
    init_value: Expr,
    init_visible: Expr,
    init_color: Expr
   ) extends Box[String](game, init_name, init_x, init_y, init_angle, init_width, init_height, 
                      init_value, init_visible, init_color) {
  
  def makecopy(name: String): GameObject = {
    new StringBox(game, name, x.init, y.init, angle.init, width.init,  height.init, 
                  value.init, visible.init, color.init)
  }
}

class BooleanBox(
    game: Game,
    init_name: Expr, 
    init_x: Expr,
    init_y: Expr,
    init_angle: Expr,
    init_width: Expr, 
    init_height: Expr, 
    init_value: Expr,
    init_visible: Expr,
    init_color: Expr
   ) extends Box[Boolean](game, init_name, init_x, init_y, init_angle, init_width, init_height, 
                         init_value, init_visible, init_color) {
  
  def makecopy(name: String): GameObject = {
    new BooleanBox(game, name, x.init, y.init, angle.init, width.init,  height.init, 
                  value.init, visible.init, color.init)
  }
}

trait InputManager extends GameObject {
  
  def collectInput(from: Context)

  override def postStep(ctx: Context): Unit = {
    super.postStep(ctx)
    // Maps the previous events to the input mechanisms
    collectInput(ctx)
  }
  
}

/**
 * Input class already defining some methods
 */
class Joystick(
    val game: Game,
    init_name: Expr, 
    init_x: Expr,
    init_y: Expr,
    init_angle: Expr,
    init_radius: Expr, 
    init_visible: Expr,
    init_color: Expr
    ) extends AbstractObject(init_name, init_x, init_y, init_angle, init_visible, init_color) 
      with Circular
      with Movable
      with InputManager
      with Visiblable {
  
  private val shape = new CircleShape()
  
  val radius = simpleProperty[Float]("radius", init_radius)
  val relative_x = simpleProperty[Float]("relative_x", 0)
  val relative_y = simpleProperty[Float]("relative_y", 0)
  val jump = simpleProperty[Boolean]("jump", false)
  val isRight = simpleProperty[Boolean]("isRight", false)
  val isLeft = simpleProperty[Boolean]("isLeft", false)
  
  def collectInput(from: Context) = {
    import Events._
    
    def updateRelativeCoords(dx: Float, dy: Float) = {
      relative_x.set(dx)
      relative_y.set(dy)
      jump.set(dy < -radius.get / 2)
      isRight.set(dx > radius.get / 2)
      isLeft.set(dx < -radius.get / 2)
    }
    
    def updateAbsoluteCoords(at: Vec2) = {
      updateRelativeCoords( at.x - x.get, at.y - y.get)
    }
    
    from.events foreach {
      case FingerMove(_, at, objs) if objs contains this =>
        if(contains(at)) updateAbsoluteCoords(at) else updateRelativeCoords(0, 0)
      case FingerDown(at, objs) if objs contains this => updateAbsoluteCoords(at)
      case FingerUp(at, objs) if objs contains this => updateRelativeCoords(0, 0)
      case _ =>
    }
  } 

  def getAABB = {
    val center = Vec2(x.get, y.get)
    val bottomLeft = center add Vec2(-radius.get, -radius.get)
    val upperRight = center add Vec2(radius.get, radius.get)
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }
  
  def getShape = {
    shape.setRadius(radius.get)
    shape.m_p.set(x.get, y.get)
    shape
  }

  def makecopy(name: String): GameObject = {
    new Joystick(game, name, x.init, y.init, angle.init, radius.init, visible.init, color.init)
  }
}

class RandomGenerator(
    val game: Game,
    init_name: Expr,
    init_x: Expr,
    init_y: Expr,
    init_angle: Expr,
    init_width: Expr, 
    init_height: Expr, 
    init_minValue: Expr, 
    init_maxValue: Expr, 
    init_visible: Expr,
    init_color: Expr
    ) extends AbstractObject(init_name, init_x, init_y, init_angle, init_visible, init_color) 
      with ResizableRectangular
      with Movable
      with Rotationable
      with Visiblable
      with Colorable
      with FixedRectangularContains { self =>
  
  private val shape = new PolygonShape()
  private val rnd = Random()
  private var currentValue: Int = -1
  
  val width = simpleProperty[Float]("width", init_width)
  val height = simpleProperty[Float]("height", init_height)
  val minValue = simpleProperty[Int]("min value", init_minValue)
  val maxValue = simpleProperty[Int]("max value", init_maxValue) // inclusive
  val value = namedProperty[Int] (
    name  = "value", 
    getF  = () => currentValue,
    nextF = () => currentValue
  )
  
  private def nextValue(): Unit = {
    currentValue = rnd.nextInt(maxValue.get + 1 - minValue.get) + minValue.get
  }
  
  override def reset(interpreter: Interpreter) = {
    super.reset(interpreter)
    nextValue()
  }
  
  override def postStep(ctx: Context) = {
    super.postStep(ctx)
    nextValue()
  }
    
  def getAABB = {
    val bottomLeft = Vec2(left.get, top.get)
    val upperRight = Vec2(left.get + width.get, top.get + height.get)
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }
  
  def getShape = {
    shape.setAsBox(width.get/2, height.get/2, Vec2(x.get, y.get), 0f)
    shape
  }

  def makecopy(name: String): GameObject = {
    new RandomGenerator(game, name, x.init, y.init, angle.init, width.init, height.init, 
        minValue.init, maxValue.init, visible.init, color.init)
  }
}

object Array2D {
  val CELL_WIDTH = 1
  val CELL_HEIGHT = 1
}

class Array2D(
    val game: Game,
    init_name: Expr,
    init_x: Expr,
    init_y: Expr,
    init_visible: Expr,
    init_color: Expr,
    init_numColumns: Expr,
    init_numRows: Expr
    ) extends AbstractObject(init_name, init_x, init_y, 0, init_visible, init_color) 
      with Rectangular
      with Movable
      with FixedRectangularContains { self =>
 
  import Array2D._
  
  private val shape = new PolygonShape()
  
  val cellsCategory = new Category {
    def game = self.game
    def name = self.name.get + " cells"
    def objects = self.cells.view.flatten
  }
    
  //TODO for the moment the array size is constant
  lazy val cells = ArrayBuffer.tabulate(numColumns.get, numRows.get) { (col, row) =>
    //TODO set a category to these cells
    Cell(this, col, row)
  }
  
  // Properties
  val numRows = simpleProperty[Int]("numRows", init_numRows)
  val numColumns = simpleProperty[Int]("numColumns", init_numColumns)
    
  val width = aliasProperty[Float] (
    name  = "width", 
    getF  = () => numColumns.get  * CELL_WIDTH,
    nextF = () => numColumns.next * CELL_WIDTH,
    exprF = () => numColumns.expr * CELL_WIDTH
  )
  
  val height = aliasProperty[Float] (
    name  = "height", 
    getF  = () => numRows.get  * CELL_HEIGHT,
    nextF = () => numRows.next * CELL_HEIGHT,
    exprF = () => numRows.expr * CELL_HEIGHT
  )
  
  def getShape = {
    shape.setAsBox(width.get/2, height.get/2, Vec2(x.get, y.get), 0f)
    shape
  }

  def getAABB() = {
    val bottomLeft = Vec2(left.get, top.get)
    val upperRight = Vec2(left.get + width.get, top.get + height.get)
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }

  protected def makecopy(name: String) = {
    new Array2D(game, name, x.init, y.init, visible.init, color.init, numColumns.init, numRows.init)
  }
}

case class Cell(
    array: Array2D,
    column: Int,
    row: Int
    ) extends GameObject(array.name.get + "[" + row + "," + column + "]") 
      with Rectangular with Positionable
      with FixedRectangularContains {
  
  import Array2D._
  
  private val shape = new PolygonShape()
  def noVelocity_=(b: Boolean) = {}
  def game = array.game
  
  val x = aliasProperty[Float] (
    name  = "x", 
    getF  = () => array.left.get  + width.get  * (column + 0.5f),
    nextF = () => array.left.next + width.next * (column + 0.5f),
    exprF = () => array.left.expr + width.expr * (column + 0.5f) 
  )
  
  val y = aliasProperty[Float] (
    name  = "y", 
    getF  = () => array.top.get  + height.get  * (row + 0.5f),
    nextF = () => array.top.next + height.next * (row + 0.5f),
    exprF = () => array.top.expr + height.expr * (row + 0.5f)
  )
  
  val width = constProperty[Float] ("width", CELL_WIDTH)
  
  val height = constProperty[Float] ("height", CELL_HEIGHT)
  
  override val bottom = aliasProperty[Float] (
    name  = "bottom", 
    getF  = () => array.top.get  + height.get  * (row + 1),
    nextF = () => array.top.next + height.next * (row + 1),
    exprF = () => array.top.expr + height.expr * (row + 1)
  )
  
  override val top = aliasProperty[Float] (
    name  = "top", 
    getF  = () => array.top.get  + height.get  * row,
    nextF = () => array.top.next + height.next * row,
    exprF = () => array.top.expr + height.expr * row
  )
  
  override val left = aliasProperty[Float] (
    name  = "left", 
    getF  = () => array.left.get  + width.get  * column,
    nextF = () => array.left.next + width.next * column,
    exprF = () => array.left.expr + width.expr * column
  )
  
  override val right = aliasProperty[Float] (
    name  = "right", 
    getF  = () => array.left.get  + width.get  * (column + 1),
    nextF = () => array.left.next + width.next * (column + 1),
    exprF = () => array.left.expr + width.expr * (column + 1)
  )
  
  val angle = proxyProperty(array.angle)
  val color = proxyProperty(array.color)
  val visible = proxyProperty(array.visible)
  
  def getShape = {
    shape.setAsBox(width.get/2, height.get/2, Vec2(x.get, y.get), 0f)
    shape
  }
  
  def getAABB() = {
    val bottomLeft = Vec2(left.get, top.get)
    val upperRight = bottomLeft add Vec2(width.get, height.get)
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }
    
  //TODO we cannot copy a cell!!!
  protected def makecopy(name: String) = ???
}
