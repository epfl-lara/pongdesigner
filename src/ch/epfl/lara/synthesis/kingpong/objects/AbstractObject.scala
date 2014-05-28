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
  
  def rawCopy(f: HistoricalProperty[_] => Expr) = {
    new IntBox(game, f(name), f(x), f(y), f(angle), f(width), f(height), f(value), f(visible), f(color))
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
                      init_value, init_visible, init_color) with ValueTextable {
  
  def rawCopy(f: HistoricalProperty[_] => Expr) = {
    new StringBox(game, f(name), f(x), f(y), f(angle), f(width), f(height), f(value), f(visible), f(color))
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
                         init_value, init_visible, init_color) with Booleanable {
  
  def rawCopy(f: HistoricalProperty[_] => Expr) = {
    new BooleanBox(game, f(name), f(x), f(y), f(angle), f(width), f(height), f(value), f(visible), f(color))
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

  def rawCopy(f: HistoricalProperty[_] => Expr) = {
    new Joystick(game, f(name), f(x), f(y), f(angle), f(radius), f(visible), f(color))
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

  def rawCopy(f: HistoricalProperty[_] => Expr) = {
    new RandomGenerator(game, f(name), f(x), f(y), f(angle), f(width), f(height), f(minValue), f(maxValue), f(visible), f(color))
  }
}

class Array2D(
    val game: Game,
    init_name: Expr,
    init_x: Expr,
    init_y: Expr,
    init_visible: Expr,
    init_color: Expr,
    init_cellWidth: Expr,
    init_cellHeight: Expr,
    init_numColumns: Expr,
    init_numRows: Expr
    ) extends AbstractObject(init_name, init_x, init_y, 0, init_visible, init_color) 
      with Rectangular
      with Movable
      with Colorable
      with FixedRectangularContains { self =>
 
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
  val numRows    = simpleProperty[Int]("numRows", init_numRows)
  val numColumns = simpleProperty[Int]("numColumns", init_numColumns)
  val cellWidth  = simpleProperty[Float]("cellWidth", init_cellWidth)
  val cellHeight = simpleProperty[Float]("cellHeight", init_cellHeight)

  val width = aliasProperty[Float] (
    name  = "width", 
    getF  = () => numColumns.get  * cellWidth.get,
    nextF = () => numColumns.next * cellWidth.next,
    exprF = () => numColumns.expr * cellWidth.expr
  )
  
  val height = aliasProperty[Float] (
    name  = "height", 
    getF  = () => numRows.get  * cellHeight.get,
    nextF = () => numRows.next * cellHeight.next,
    exprF = () => numRows.expr * cellHeight.expr
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

  /**
   * Find the cell that contains the given object.
   * The implementation should be as fast as possible.
   */
  def containingCell(obj: Positionable): ObjectLiteral = {
    val pos = obj.center.get
    if (!this.contains(pos)) {
      ObjectLiteral.empty
    } else {
      cells(((pos.x - left.get) / cellWidth.get).toInt)(((pos.y - top.get) / cellHeight.get).toInt).expr
    }
  }
  
  protected def rawCopy(f: HistoricalProperty[_] => Expr) = {
    new Array2D(game, f(name), f(x), f(y), f(visible), f(color), f(cellWidth), f(cellHeight), f(numColumns), f(numRows))
  }
}

case class Cell(
    array: Array2D,
    column: Int,
    row: Int
    ) extends GameObject(array.name.get + "[" + row + "," + column + "]") 
      with Rectangular
      with FixedRectangularContains {

  private val shape = new PolygonShape()
  def noVelocity_=(b: Boolean) = {}
  def game = array.game
  
  val x = namedProperty[Float] (
    name  = "x", 
    getF  = () => array.left.get  + width.get  * (column + 0.5f),
    nextF = () => array.left.next + width.next * (column + 0.5f)
    //,    exprF = () => array.left.expr + width.expr * (column + 0.5f) 
  )
  
  val y = namedProperty[Float] (
    name  = "y", 
    getF  = () => array.top.get  + height.get  * (row + 0.5f),
    nextF = () => array.top.next + height.next * (row + 0.5f)
    //,    exprF = () => array.top.expr + height.expr * (row + 0.5f)
  )
  
  val width = proxyProperty[Float]("width", array.cellWidth)
  val height = proxyProperty[Float]("height", array.cellHeight)
  
  override val bottom = namedProperty[Float] (
    name  = "bottom", 
    getF  = () => array.top.get  + height.get  * (row + 1),
    nextF = () => array.top.next + height.next * (row + 1)
    //,    exprF = () => array.top.expr + height.expr * (row + 1)
  )
  
  override val top = namedProperty[Float] (
    name  = "top", 
    getF  = () => array.top.get  + height.get  * row,
    nextF = () => array.top.next + height.next * row
    //,    exprF = () => array.top.expr + height.expr * row
  )
  
  override val left = namedProperty[Float] (
    name  = "left", 
    getF  = () => array.left.get  + width.get  * column,
    nextF = () => array.left.next + width.next * column
    //,    exprF = () => array.left.expr + width.expr * column
  )
  
  override val right = namedProperty[Float] (
    name  = "right", 
    getF  = () => array.left.get  + width.get  * (column + 1),
    nextF = () => array.left.next + width.next * (column + 1)
    //,    exprF = () => array.left.expr + width.expr * (column + 1)
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
  protected def rawCopy(f: HistoricalProperty[_] => Expr) = ???
}

/**
 * Gravity in the game.
 */
class Gravity(
    val game: Game,
    init_name: Expr, 
	  init_x: Expr,
	  init_y: Expr,
	  init_angle: Expr,
    init_radius: Expr,
	  init_visible: Expr, 
	  init_color: Expr
	 ) extends GameObject(init_name) with Movable with Rotationable with ResizableCircular with FixedRectangularContains with Visiblable with Colorable with Booleanable {
  
  def noVelocity_=(b: Boolean): Unit = {}
  val x = simpleProperty[Float]("x", init_x)
  val y = simpleProperty[Float]("y", init_y)
  val angle = simpleProperty[Float]("angle", init_angle)
  val visible = simpleProperty[Boolean]("visible", init_visible)
  val color = simpleProperty[Int]("color", init_color)
  val value = simpleProperty[Boolean]("enabled", true)

  private val shape = new PolygonShape()
  
  val radius = simpleProperty[Float]("radius", init_radius)
  
  val smallradius = namedProperty[Float] (
      name = "halfradius",
      getF = () => (radius.get/10),
      nextF = () => (radius.next/10)
  )

  override val left = namedProperty[Float](
    name = "left",
    getF = () => Math.min(x.get, xTo.get-smallradius.get),
    nextF = () => Math.min(x.next, xTo.next-smallradius.next)
  )
  
  override val right = namedProperty[Float](
    name = "right",
    getF = () => Math.max(x.get, xTo.get+smallradius.get),
    nextF = () => Math.max(x.next, xTo.next+smallradius.next)
  )
  
  override val top = namedProperty[Float](
    name = "top",
    getF = () => Math.min(y.get, yTo.get-smallradius.get),
    nextF = () => Math.min(y.next, yTo.next-smallradius.next)
  )
  
  override val bottom = namedProperty[Float](
    name = "bottom",
    getF = () => Math.max(y.get, yTo.get+smallradius.get),
    nextF = () => Math.max(y.next, yTo.next+smallradius.next)
  )
  
  val xTo = namedProperty[Float] (
    name  = "xTo", 
    getF  = () => (x.get + radius.get * Math.cos(angle.get)).toFloat,
    nextF = () => (x.next + radius.next * Math.cos(angle.next)).toFloat
    //exprF = () => (x.expr + radius.expr * MethodCall("cosDeg", List(angle.expr)))
  )
  
  val yTo = namedProperty[Float] (
    name  = "yTo", 
    getF  = () => (y.get + radius.get * Math.sin(angle.get)).toFloat,
    nextF = () => (y.next + radius.next * Math.sin(angle.next)).toFloat
    //exprF = () => (y.expr + radius.expr * MethodCall("sinDeg", List(angle.expr)))
  )
  
  private var cache: Vec2 = Vec2(0, 0)
  private var savedRadius = 0f
  private var savedAngle = 0f
  
  def vector = {
    if(radius.get != savedRadius || angle.get != savedAngle) {
      savedRadius = radius.get
      savedAngle = angle.get
      val res = Vec2((savedRadius * Math.cos(savedAngle)).toFloat, (savedRadius * Math.sin(savedAngle)).toFloat)
      cache = res
      res
    } else cache
  }
  
  private var cacheNext: Vec2 = Vec2(0, 0)
  private var savedRadiusNext = 0f
  private var savedAngleNext = 0f
  def vectorNext = {
    if(radius.next != savedRadiusNext || angle.next != savedAngleNext) {
      savedRadiusNext = radius.next
      savedAngleNext = angle.next
      val res = Vec2((savedRadiusNext * Math.cos(savedAngleNext)).toFloat, (savedRadiusNext * Math.sin(savedAngleNext)).toFloat)
      cacheNext = res
      res
    } else cacheNext
  }
  
  /*simplePhysicalProperty[Vec2] (
    name  = "vector",
    init = Tuple((init_x + init_radius * MethodCall("cosDeg", List(init_angle))), (init_y + init_radius * MethodCall("sinDeg", List(init_angle)))),
    f = () => {
      val g = Vec2(xTo.next - x.next, yTo.next - y.next)
      game.updateGravity(g)
    }
    //exprF = () => (x.expr + radius.expr * MethodCall("cosDeg", List(angle.expr)))
  )*/

  def getAABB = {
    val one_corner = Vec2(x.get, y.get)
    val other_corner = Vec2(xTo.get, yTo.get)
    new org.jbox2d.collision.AABB(one_corner, other_corner)
  }
  
  def getShape = {
    shape.setAsBox(Math.abs(x.get - xTo.get), Math.abs(y.get - yTo.get), Vec2((x.get + xTo.get) / 2, (y.get + yTo.get) / 2), 0f)
    shape
  }

  def rawCopy(f: HistoricalProperty[_] => Expr) = {
    new Gravity(game, f(name), f(x), f(y), f(angle), f(radius), f(visible), f(color))
  }
}
