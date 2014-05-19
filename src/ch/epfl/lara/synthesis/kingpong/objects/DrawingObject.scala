package ch.epfl.lara.synthesis.kingpong.objects

import org.jbox2d.collision.shapes.PolygonShape
import org.jbox2d.collision.shapes.CircleShape
import org.jbox2d.collision.shapes.Shape
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.rules.Context
import ch.epfl.lara.synthesis.kingpong.rules.Events
import scala.collection.mutable.ArrayBuffer
import ch.epfl.lara.synthesis.kingpong.expression.Trees


/**
 * An element drawn at a specific time.
 */
case class DrawingElement(time: Int, fromx: Float, fromy: Float, tox: Float, toy: Float, width: Float, color: Int) {
  var next: DrawingElement = _
  var pred: DrawingElement = _
  var prevTan: (Float, Float) = _
  var nextTan: (Float, Float) = _
}


/**
 * Provides time-dependent drawing facilities for presentations.
 */
case class DrawingObject(
    val game: Game,
    init_name: Expr, 
    init_x: Expr,
    init_y: Expr,
    init_angle: Expr,
    init_width: Expr, 
    init_height: Expr,
    init_visible: Expr,
    init_color: Expr,
    init_stroke_width: Expr,
    init_color_drawing: Expr
    ) extends AbstractObject(init_name, init_x, init_y, init_angle, init_visible, init_color)
      with ResizableRectangular
      with Movable
      with Visiblable
      with Colorable
      with Directionable
      with AngularRectangularContains {
  
  val width = simpleProperty[Float]("width", init_width)
  val height = simpleProperty[Float]("height", init_height)
  //val width_drawing= simpleProperty[Float]("width_drawing", 0.02f)
  
  val stroke_width = simpleProperty[Float]("stroke_width", init_stroke_width)
  
  val width_drawing = aliasProperty (
    name  = "bottom", 
    getF  = () => stroke_width.get/(width.get * game.pixelsByUnit),
    nextF = () => stroke_width.next/(width.next * game.pixelsByUnit),
    exprF = () => stroke_width.expr/(width.expr * MethodCall("gamePixelsPerUnit", ObjectLiteral(this)::Nil))
  )
  
  val color_drawing= simpleProperty[Int]("color_drawing", init_color_drawing)
  private val mDrawings = ArrayBuffer[DrawingElement]() // Records all drawings.
  def getDrawingElements = mDrawings
  def addDrawingElement(time: Int, fromx: Float, fromy: Float, tox: Float, toy: Float, stroke_width: Float, color: Int): Unit = {
    // Convert it into relative coordinates [0, 1] and rotation.
    val cosa = Math.cos(-Math.toRadians(angle.get)).toFloat
    val sina = Math.sin(-Math.toRadians(angle.get)).toFloat
    @inline def scaleX(a: Float): Float = (a - (x.get - width.get / 2))/width.get
    @inline def scaleY(a: Float): Float = (a - (y.get - height.get / 2))/height.get
    val unrotatedfromX = scaleX(fromx * cosa - fromy * sina)
    val unrotatedfromY = scaleY(fromy * sina + fromy * cosa)
    val unrotatedToX = scaleX(tox * cosa - toy * sina)
    val unrotatedToY = scaleY(toy * sina + toy * cosa)
    if(!mDrawings.exists( d => d.time == time && d.fromx == unrotatedfromX && d.fromy == unrotatedfromY && d.tox == unrotatedToX && d.toy == unrotatedToY && d.width == stroke_width && d.color == color)) {
      addDrawingElement(DrawingElement(time, unrotatedfromX, unrotatedfromY, unrotatedToX, unrotatedToY, stroke_width, color))
    }
  }
    
  def addDrawingElement(e: DrawingElement): Unit = {
    if(mDrawings.size > 0) { // Join drawing elements together.
      val p =  mDrawings.last
      if(p.tox == e.fromx && p.toy == e.fromy && p.width == e.width && p.color == e.color) {
        p.next = e
        e.pred = p
        val pred = p.pred
        // Smoothing: Add tangent to the previous
        if(pred != null) {
          val x1 = pred.fromx
          val y1 = pred.fromy
          val x2 = p.fromx
          val y2 = p.fromy
          val x3 = p.tox
          val y3 = p.toy
          val angle = Math.abs(Math.atan2(x3-x2, y3-y2)-Math.atan2(x2-x1, y2-y1))
          if(angle < Math.PI / 4 || angle >= (2-1/4.0)*Math.PI) {
            // Make the two lines continuous.
            val d1 = Math.sqrt(Math.pow(x2-x1, 2) + Math.pow(y2-y1, 2)).toFloat
            val d2 = Math.sqrt(Math.pow(x3-x2, 2) + Math.pow(y3-y2, 2)).toFloat
            val t = d1/(d1+d2)
            val a = t/6
            val b = (1-t)/6
            p.prevTan = (x2 - a*(x3-x1), y2 - a*(y3-y1))
            p.nextTan = (x2 + b*(x3-x1), y2 + b*(y3-y1))
          }
        }
      }
    }
    mDrawings += e
  }
    
  val defaultRule = { (g: Game) =>
    import g._
    import Trees._
    fingerMoveOver(this) { move =>
      whenever(move._2 in this) {
        MethodCall("importDrawings", List(ObjectLiteral(this), move._1, move._2, width_drawing, color_drawing))
      }
    }
  }
  
  
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

  //def contains(pos: Vec2) = getAABB.contains(pos)
  
  def makecopy(name: String): GameObject = {
    this.copy(init_name = name)
  }
}

