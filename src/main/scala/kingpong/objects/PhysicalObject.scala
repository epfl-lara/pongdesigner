package ch.epfl.lara.synthesis.kingpong.objects

import org.jbox2d.dynamics.Body
import org.jbox2d.dynamics.BodyType
import org.jbox2d.dynamics.World
import org.jbox2d.dynamics.BodyDef
import org.jbox2d.dynamics.FixtureDef
import org.jbox2d.collision.shapes.PolygonShape
import org.jbox2d.collision.shapes.CircleShape

import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.expression.Trees._

abstract class PhysicalObject(init_name: Expr, 
                              init_x: Expr,
                              init_y: Expr,
                              init_angle: Expr,
                              init_visible: Expr,
                              init_density: Expr,
                              init_friction: Expr,
                              init_restitution: Expr,
                              init_fixedRotation: Expr
                             ) extends GameObject(init_name) { self =>
  
  protected val game: Game
  protected val body: Body

  // exactly one fixture (and thus shape) on the body         
  require(body.getFixtureList() != null && 
          body.getFixtureList().getNext() == null)
  
  body.setUserData(self)
  
  protected val fixture = body.getFixtureList()
  
  /** The body mass. */
  def mass = body.getMass()
  
  
  // --------------------------------------------------------------------------
  // Properties
  // --------------------------------------------------------------------------

  val x = property[Float]("x", init_x) { x =>
    body.setTransform(Vec2(x, body.getPosition().y), body.getAngle())
  } { () =>
    body.getPosition().x
  }

  val y = property[Float]("y", init_y) { y =>
    body.setTransform(Vec2(body.getPosition().x, y), body.getAngle())
  } { () =>
    body.getPosition().y
  }

  val angle = property[Float]("angle", init_angle) { a =>
    body.setTransform(body.getPosition(), a)
  } { () =>
    body.getAngle()
  }

  val visible = property[Boolean]("visible", init_visible) { b =>
    body.setActive(b)
  } { () =>
    body.isActive()
  }

  val density = property[Float]("density", init_density) { d =>
    fixture.setDensity(d)
    body.resetMassData() // update the body mass
  } { () =>
    fixture.getDensity()
  }

  val friction = property[Float]("friction", init_friction) { f =>
    fixture.setFriction(f)
  } { () =>
    fixture.getFriction()
  }

  val restitution = property[Float]("restitution", init_restitution) { r =>
    fixture.setRestitution(r)
  } { () =>
    fixture.getRestitution()
  }

  val fixedRotation = property[Boolean]("fixed-rotation", init_fixedRotation) { b =>
    body.setFixedRotation(b)
  } { () =>
    body.isFixedRotation()
  }
  

  //TODO !!!!
  /*
  val tpe = Property(body.getType()) { v =>
    body.setType(v)
  }
  */
  
  // --------------------------------------------------------------------------
  // Actions on the body
  // --------------------------------------------------------------------------
  
  def applyLinearImpulse(impulse: Vec2, point: Vec2 = body.getWorldCenter()) {
    body.applyLinearImpulse(impulse, point)
  }
  
  def applyForce(force: Vec2, point: Vec2 = body.getWorldCenter()) {
    body.applyForce(force, point)
  }
  
  def applyAngularImpulse(impulse: Float) {
    body.applyAngularImpulse(impulse)
  }
  
  // --------------------------------------------------------------------------
  // Utility functions
  // --------------------------------------------------------------------------  
  
  def getAABB() = fixture.getAABB()
  
}

class Rectangle (protected val game: Game,
                 init_name: Expr,
                 init_x: Expr,
                 init_y: Expr,
                 init_angle: Expr,
                 init_width: Expr,
                 init_height: Expr,
                 init_visible: Expr,
                 init_density: Expr,
                 init_friction: Expr,
                 init_restitution: Expr,
                 init_fixedRotation: Expr,
                 init_tpe: BodyType = BodyType.DYNAMIC
                ) extends PhysicalObject(init_name, init_x, init_y, init_angle, init_visible, init_density,
                                         init_friction, init_restitution, init_fixedRotation)
                  with Rectangular {

  final val body = {
    val body_def = new BodyDef()
    body_def.position = Vec2(game.typeCheckAndEvaluate[Float](init_x), 
                             game.typeCheckAndEvaluate[Float](init_y))
    body_def.`type` = init_tpe
    
    val shape = new PolygonShape()
    shape.setAsBox(game.typeCheckAndEvaluate[Float](init_width),
                   game.typeCheckAndEvaluate[Float](init_height))
  
    val fixture_def = new FixtureDef()
    fixture_def.shape = shape
    fixture_def.density = game.typeCheckAndEvaluate[Float](init_density)
    fixture_def.friction = game.typeCheckAndEvaluate[Float](init_friction)
    fixture_def.restitution = game.typeCheckAndEvaluate[Float](init_restitution)
    
    val body = game.world.world.createBody(body_def)
    body.createFixture(fixture_def)
    body
  }

  protected val shape: PolygonShape = fixture.getShape().asInstanceOf[PolygonShape] 
  
  val width: Property[Float] = simplePhysicalProperty[Float]("width", init_width) { w =>
    shape.setAsBox(w, height.get)
    body.resetMassData() // update the body mass
  }

  val height: Property[Float] = simplePhysicalProperty[Float]("height", init_height) { h =>
    shape.setAsBox(width.get, h)
    body.resetMassData() // update the body mass
  }
  
}

class Circle(protected val game: Game,
             init_name: Expr,
             init_x: Expr,
             init_y: Expr,
             init_angle: Expr,
             init_radius: Expr,
             init_visible: Expr,
             init_density: Expr,
             init_friction: Expr,
             init_restitution: Expr,
             init_fixedRotation: Expr,
             init_tpe: BodyType = BodyType.DYNAMIC
            ) extends PhysicalObject(init_name, init_x, init_y, init_angle, init_visible, init_density,
                                     init_friction, init_restitution, init_fixedRotation) {
	
  // Create the physical JBox2D body with a circle shape.
  final val body = {
    val body_def = new BodyDef()
    body_def.position = Vec2(game.typeCheckAndEvaluate[Float](init_x), 
                             game.typeCheckAndEvaluate[Float](init_y))
    body_def.`type` = init_tpe
    
    val shape = new CircleShape()
    shape.m_radius = game.typeCheckAndEvaluate[Float](init_radius)
  
    val fixture_def = new FixtureDef()
    fixture_def.shape = shape
    fixture_def.density = game.typeCheckAndEvaluate[Float](init_density)
    fixture_def.friction = game.typeCheckAndEvaluate[Float](init_friction)
    fixture_def.restitution = game.typeCheckAndEvaluate[Float](init_restitution)
    
    val body = game.world.world.createBody(body_def)
    body.createFixture(fixture_def)
    body
  }

  property[Float]("radius", init_radius) { r =>
    fixture.getShape().m_radius = r
    body.resetMassData() // update the body mass
  } { () =>
    fixture.getShape().m_radius
  }
   
}