package ch.epfl.lara.synthesis.kingpong.objects

import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import org.jbox2d.collision.shapes.PolygonShape
import org.jbox2d.collision.shapes.CircleShape
import org.jbox2d.collision.shapes.Shape
import ch.epfl.lara.synthesis.kingpong.rules.Context
import ch.epfl.lara.synthesis.kingpong.rules.Events

trait MenuObjects extends AbstractObject {
  
}

/**
 * Pointer object containing a reference to an object
 */
case class ObjRef(override val game: Game,
                        init_name: Expr, 
                        init_x: Expr,
                        init_y: Expr 
                       ) extends GameObject(init_name) 
                         with Point {

  def className = s"InputMenu"
  // --------------------------------------------------------------------------
  // Properties
  // --------------------------------------------------------------------------
  
  val obj = simpleProperty[GameObject]("obj", ObjectLiteral(null))

  def angle: RWProperty[Float] = if(obj.get != null) obj.get.angle else null
  def color: RWProperty[Int] = if(obj.get != null) obj.get.color else null
  def visible: RWProperty[Boolean] = if(obj.get != null) obj.get.visible else null
  def x: RWProperty[Float] = if(obj.get != null) obj.get.x else null
  def y: RWProperty[Float] = if(obj.get != null) obj.get.y else null
  
  // --------------------------------------------------------------------------
  // Utility functions
  // --------------------------------------------------------------------------  
  
  def getAABB = {
    val center = Vec2(x.get, y.get)
    val bottomLeft = center
    val upperRight = center
    new org.jbox2d.collision.AABB(bottomLeft, upperRight)
  }
  
  //private val shape = new CircleShape()
  //shape.setRadius(game.typeCheckAndEvaluate[Float](init_radius))

  //def getShape = {
  //  shape.m_p.set(x.get, y.get)
  //  shape
  //}

  def contains(pos: Vec2) = getAABB.contains(pos)
  
  def makecopy(name: String): GameObject = {
    this.copy(init_name=StringIsExpr(name))
  }
}

/**
 * Input class already defining some methods
 */
case class ActiveBox(override val game: Game,
                        init_name: Expr, 
                        init_x: Expr,
                        init_y: Expr,
                        init_angle: Expr,
                        init_radius: Expr, 
                        init_visible: Expr,
                        init_color: Expr,
                        init_picture: Expr
                       ) extends AbstractObject(init_name, init_x, init_y, init_angle, init_visible, init_color) 
                         with Circular
                         with InputManager {

  def className = s"ActiveBox"
  // --------------------------------------------------------------------------
  // Properties
  // --------------------------------------------------------------------------
  
  val radius = simpleProperty[Float]("radius", init_radius)
  val picture = simpleProperty[String]("picture", init_picture)
  val obj = simpleProperty[GameObject]("obj", ObjectLiteral(null))
  
  def collectInput(from: Context): Unit = {
    import Events._ ;
     
    /*from.events foreach {
      case FingerMove(_, at, objs) if objs contains this =>
        if(contains(at)) updateAbsoluteCoords(at) else updateRelativeCoords(0, 0)
      case FingerDown(at, objs) if objs contains this => updateAbsoluteCoords(at)
      case FingerUp(at, objs) if objs contains this => updateRelativeCoords(0, 0)
      case _ =>
    }*/
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
  shape.setRadius(game.evaluate[Float](init_radius))

  def getShape = {
    shape.m_p.set(x.get, y.get)
    shape
  }

  def contains(pos: Vec2) = getAABB.contains(pos)
  
  def makecopy(name: String): GameObject = {
    this.copy(init_name=StringIsExpr(name))
  }
}