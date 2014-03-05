package ch.epfl.lara.synthesis.kingpong.objects

import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import org.jbox2d.collision.shapes.PolygonShape
import org.jbox2d.collision.shapes.CircleShape
import org.jbox2d.collision.shapes.Shape
import ch.epfl.lara.synthesis.kingpong.rules.Context
import ch.epfl.lara.synthesis.kingpong.rules.Events

abstract class AbstractObject(init_name: Expr, 
                              init_x: Expr,
                              init_y: Expr,
                              init_angle: Expr,
                              init_visible: Expr, 
                              init_color: Expr
                             ) extends GameObject(init_name) {
  
  val x = simpleProperty[Float]("x", init_x)
  val y = simpleProperty[Float]("y", init_y)
  val angle = simpleProperty[Float]("angle", init_angle)
  val visible = simpleProperty[Boolean]("visible", init_visible)
  val color = simpleProperty[Int]("color", init_color)
  
  def getShape: Shape
}


case class Box[T : PongType](val game: Game,
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
                         with Rectangular {
  
  def className = "Box[" + implicitly[PongType[T]].getPongType.toString() + "]"
  // --------------------------------------------------------------------------
  // Properties
  // --------------------------------------------------------------------------
  
  val width = simpleProperty[Float]("width", init_width)
  val height = simpleProperty[Float]("height", init_height)
  val value = simpleProperty[T]("value", init_value)
  
  // --------------------------------------------------------------------------
  // Utility functions
  // --------------------------------------------------------------------------  
  
  def getAABB = {
    val bottomLeft = Vec2(x.get, y.get)
    val upperRight = bottomLeft add Vec2(width.get, height.get)
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }
  
  private val shape = new PolygonShape()
  
  def getShape = {
    shape.setAsBox(width.get/2, height.get/2, Vec2(x.get, y.get), 0f)
    shape
  }

  def contains(pos: Vec2) = getAABB.contains(pos)
  
  def makecopy(name: String): GameObject = {
    this.copy[T](init_name = name)
  }
}

trait InputManager {
  
  def collectInput(from: Context)

}

/**
 * Input class already defining some methods
 */
case class Joystick(val game: Game,
                        init_name: Expr, 
                        init_x: Expr,
                        init_y: Expr,
                        init_angle: Expr,
                        init_radius: Expr, 
                        init_visible: Expr,
                        init_color: Expr
                       ) extends AbstractObject(init_name, init_x, init_y, init_angle, init_visible, init_color) 
                         with Circular
                         with InputManager {
  
  def className = "Joystick"
  
  // --------------------------------------------------------------------------
  // Properties
  // --------------------------------------------------------------------------
  
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

  // --------------------------------------------------------------------------
  // Utility functions
  // --------------------------------------------------------------------------  
  
  def getAABB = {
    val center = Vec2(x.get, y.get)
    val bottomLeft = center add Vec2(-radius.get, -radius.get)
    val upperRight = center add Vec2(radius.get, radius.get)
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }
  
  private val shape = new CircleShape()

  def getShape = {
    shape.setRadius(radius.get)
    shape.m_p.set(x.get, y.get)
    shape
  }

  // MIKEAL shouldn't we use the shape instead of the AABB ?
  def contains(pos: Vec2) = getAABB.contains(pos)
  
  def makecopy(name: String): GameObject = {
    this.copy(init_name = name)
  }
}

/*
case class PathMovement(val game: Game,
                        init_name: Expr, 
                        init_x: Expr,
                        init_y: Expr,
                        init_angle: Expr,
                        init_visible: Expr,
                        init_color: Expr
                       ) extends AbstractObject(init_name, init_x, init_y, init_angle, init_visible, init_color) {
  

  def className = "Movement"

  def contains(pos: Vec2): Boolean = false
  def getAABB(): AABB = ???
  protected def makecopy(name: String): GameObject = ???
  def getShape: Shape = ???
}
*/


object Array2D {
  
  val CELL_WIDTH = 1
  val CELL_HEIGHT = 1
  
}

case class Array2D(
    game: Game,
    init_name: Expr,
    init_x: Expr,
    init_y: Expr,
    init_visible: Expr,
    init_color: Expr,
    init_numColumns: Expr,
    init_numRows: Expr
    ) extends AbstractObject(init_name, init_x, init_y, 0, init_visible, init_color) 
      with Rectangular {
 
  import Array2D._
  
  val className = "Array2D"
  
  //TODO for the moment the array size is constant
  lazy val cells = Array.tabulate(numColumns.get, numRows.get)(Cell(this, _, _))
    
  // Properties
  val numRows = simpleProperty[Int]("numRows", init_numRows)
  val numColumns = simpleProperty[Int]("numColumns", init_numColumns)
    
  val width = readOnlyProperty[Float] (
    name  = "width", 
    getF  = numColumns.get  * CELL_WIDTH,
    nextF = numColumns.next * CELL_WIDTH,
    exprF = numColumns.expr * CELL_WIDTH
  )
  
  val height = readOnlyProperty[Float] (
    name  = "height", 
    getF  = numRows.get  * CELL_HEIGHT,
    nextF = numRows.next * CELL_HEIGHT,
    exprF = numRows.expr * CELL_HEIGHT
  )
  
  private val shape = new PolygonShape()
  def getShape = {
    shape.setAsBox(width.get/2, height.get/2, Vec2(x.get, y.get), 0f)
    shape
  }

  def getAABB() = {
    val bottomLeft = Vec2(left.get, top.get)
    val upperRight = bottomLeft add Vec2(width.get, height.get)
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }
    
  def contains(pos: Vec2) = getAABB.contains(pos)
  
  protected def makecopy(name: String) = this.copy(init_name = name)
}

case class Cell(
    array: Array2D,
    column: Int,
    row: Int
    ) extends GameObject(array.name.get + "[" + row + "," + column + "]") 
      with Rectangular {
  
  import Array2D._
  
  val className = "Cell"
  def game = array.game
  
  val x = readOnlyProperty[Float] (
    name  = "x", 
    getF  = array.left.get  + (column + 0.5f) * width.get,
    nextF = array.left.next + (column + 0.5f) * width.next,
    exprF = array.left.expr + (column + 0.5f) * width.expr
  )
  
  val y = readOnlyProperty[Float] (
    name  = "y", 
    getF  = array.top.get  + (row + 0.5f) * height.get,
    nextF = array.top.next + (row + 0.5f) * height.next,
    exprF = array.top.expr + (row + 0.5f) * height.expr
  )
  
  val width = constProperty[Float] (
    name   = "width", 
    constF = CELL_WIDTH
  )
  
  val height = constProperty[Float] (
    name   = "height", 
    constF = CELL_HEIGHT
  )
  
  override val bottom = readOnlyProperty[Float] (
    name  = "bottom", 
    getF  = array.top.get  + (row + 1) * height.get,
    nextF = array.top.next + (row + 1) * height.next,
    exprF = array.top.expr + (row + 1) * height.expr
  )
  
  override val top = readOnlyProperty[Float] (
    name  = "top", 
    getF  = array.top.get  + row * height.get,
    nextF = array.top.next + row * height.next,
    exprF = array.top.expr + row * height.expr
  )
  
  override val left = readOnlyProperty[Float] (
    name  = "left", 
    getF  = array.left.get  + column * width.get,
    nextF = array.left.next + column * width.next,
    exprF = array.left.expr + column * width.expr
  )
  
  override val right = readOnlyProperty[Float] (
    name  = "right", 
    getF  = array.left.get  + (column + 1) * width.get,
    nextF = array.left.next + (column + 1) * width.next,
    exprF = array.left.expr + (column + 1) * width.expr
  )
  
  val angle = proxyProperty(array.angle)
  val color = proxyProperty(array.color)
  val visible = proxyProperty(array.visible)
  
  private val shape = new PolygonShape()
  def getShape = {
    shape.setAsBox(width.get/2, height.get/2, Vec2(x.get, y.get), 0f)
    shape
  }
  
  def getAABB() = {
    val bottomLeft = Vec2(left.get, top.get)
    val upperRight = bottomLeft add Vec2(width.get, height.get)
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }
    
  def contains(pos: Vec2) = getAABB.contains(pos)
  
  //TODO we cannot copy a cell!!!
  protected def makecopy(name: String) = ???
  
}
