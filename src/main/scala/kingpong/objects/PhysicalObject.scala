package ch.epfl.lara.synthesis.kingpong.objects

import org.jbox2d.dynamics.Body
import org.jbox2d.dynamics.BodyType
import org.jbox2d.dynamics.World
import org.jbox2d.dynamics.BodyDef
import org.jbox2d.dynamics.FixtureDef
import org.jbox2d.collision.shapes.PolygonShape
import org.jbox2d.collision.shapes.CircleShape

import android.util.Log

import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.rules.Context

abstract class PhysicalObject(init_name: Expr, 
                              init_x: Expr,
                              init_y: Expr,
                              init_angle: Expr,
                              init_visible: Expr,
                              init_velocity: Expr,
                              init_angularVelocity: Expr,
                              init_density: Expr,
                              init_friction: Expr,
                              init_restitution: Expr,
                              init_fixedRotation: Expr
                             ) extends GameObject(init_name) { self =>
  
  protected def game: Game
  protected def body: Body 
  protected def fixture = body.getFixtureList()
  
  /** The body mass. */
  def mass = body.getMass()
  
  override def reset(interpreter: Interpreter)(implicit context: Context) = {
    super.reset(interpreter)
    body.setAwake(true)
  }
  
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

  val velocity = property[Vec2]("velocity", init_velocity) { v =>
    body.setLinearVelocity(v)
  } { () =>
    body.getLinearVelocity()
  }

  val angularVelocity = property[Float]("angular-velocity", init_angularVelocity) { av =>
    body.setAngularVelocity(av)
  } { () =>
    body.getAngularVelocity()
  }

  val visible = simplePhysicalProperty[Boolean]("visible", init_visible) { b =>
    body.setActive(b)
  }

  val density = simplePhysicalProperty[Float]("density", init_density) { d =>
    fixture.setDensity(d)
    body.resetMassData() // update the body mass
  }

  val friction = simplePhysicalProperty[Float]("friction", init_friction) { f =>
    fixture.setFriction(f)
  }

  val restitution = simplePhysicalProperty[Float]("restitution", init_restitution) { r =>
    fixture.setRestitution(r)
  }

  val fixedRotation = simplePhysicalProperty[Boolean]("fixed-rotation", init_fixedRotation) { b =>
    body.setFixedRotation(b)
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
  
  def getAABB() = fixture.getAABB(0)
  def contains(pos: Vec2) = fixture.testPoint(pos)
  
}

class Rectangle (protected val game: Game,
                 init_name: Expr,
                 init_x: Expr,
                 init_y: Expr,
                 init_angle: Expr,
                 init_width: Expr,
                 init_height: Expr,
                 init_visible: Expr,
                 init_velocity: Expr,
                 init_angularVelocity: Expr,
                 init_density: Expr,
                 init_friction: Expr,
                 init_restitution: Expr,
                 init_fixedRotation: Expr,
                 init_tpe: BodyType = BodyType.DYNAMIC
                ) extends PhysicalObject(init_name, init_x, init_y, init_angle, init_visible, init_velocity, init_angularVelocity,
                                         init_density, init_friction, init_restitution, init_fixedRotation)
                  with Rectangular {

  val body = {
    val body_def = new BodyDef()
    body_def.position = Vec2(game.typeCheckAndEvaluate[Float](init_x), 
                             game.typeCheckAndEvaluate[Float](init_y))
    body_def.`type` = init_tpe
    
    //if (init_tpe == BodyType.DYNAMIC) {
    //  body_def.bullet = true
    //}

    val shape = new PolygonShape()
    shape.setAsBox(game.typeCheckAndEvaluate[Float](init_width)/2,
                   game.typeCheckAndEvaluate[Float](init_height)/2)
  
    val fixture_def = new FixtureDef()
    fixture_def.shape = shape
    fixture_def.density = game.typeCheckAndEvaluate[Float](init_density)
    fixture_def.friction = game.typeCheckAndEvaluate[Float](init_friction)
    fixture_def.restitution = game.typeCheckAndEvaluate[Float](init_restitution)
    
    val body = game.world.world.createBody(body_def)
    val fixture = body.createFixture(fixture_def)
    body.setUserData(this)

    body
  }

  protected def shape: PolygonShape = fixture.getShape().asInstanceOf[PolygonShape] 
  
  val width: Property[Float] = simplePhysicalProperty[Float]("width", init_width) { w =>
    shape.setAsBox(w/2, height.get/2)
    body.resetMassData() // update the body mass
  }

  val height: Property[Float] = simplePhysicalProperty[Float]("height", init_height) { h =>
    shape.setAsBox(width.get/2, h/2)
    body.resetMassData() // update the body mass
  }
  
}

class Circle(protected val game: Game,
             init_name: Expr,
             init_x: Expr,
             init_y: Expr,
             init_radius: Expr,
             init_visible: Expr,
             init_velocity: Expr,
             init_angularVelocity: Expr,
             init_density: Expr,
             init_friction: Expr,
             init_restitution: Expr,
             init_fixedRotation: Expr,
             init_tpe: BodyType = BodyType.DYNAMIC
            ) extends PhysicalObject(init_name, init_x, init_y, 0, init_visible, init_velocity, init_angularVelocity,
                                     init_density, init_friction, init_restitution, init_fixedRotation) {
	
  // Create the physical JBox2D body with a circle shape.
  final val body = {
    val body_def = new BodyDef()
    body_def.position = Vec2(game.typeCheckAndEvaluate[Float](init_x), 
                             game.typeCheckAndEvaluate[Float](init_y))
    body_def.`type` = init_tpe
    
    //if (init_tpe == BodyType.DYNAMIC) {
    //  body_def.bullet = true
    //}
    
    val shape = new CircleShape()
    shape.m_radius = game.typeCheckAndEvaluate[Float](init_radius)
  
    val fixture_def = new FixtureDef()
    fixture_def.shape = shape
    fixture_def.density = game.typeCheckAndEvaluate[Float](init_density)
    fixture_def.friction = game.typeCheckAndEvaluate[Float](init_friction)
    fixture_def.restitution = game.typeCheckAndEvaluate[Float](init_restitution)
    
    val body = game.world.world.createBody(body_def)
    val fixture = body.createFixture(fixture_def)
    body.setUserData(this)

    body
  }

  val radius = simplePhysicalProperty[Float]("radius", init_radius) { r =>
    fixture.getShape().m_radius = r
    body.resetMassData() // update the body mass
  }
  
}
