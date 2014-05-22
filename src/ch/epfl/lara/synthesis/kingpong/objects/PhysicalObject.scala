package ch.epfl.lara.synthesis.kingpong.objects

import org.jbox2d.collision.shapes.CircleShape
import org.jbox2d.collision.shapes.PolygonShape
import org.jbox2d.dynamics.Body
import org.jbox2d.dynamics.BodyDef
import org.jbox2d.dynamics.BodyType
import org.jbox2d.dynamics.FixtureDef
import org.jbox2d.dynamics.Fixture

import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.rules.Context
import ch.epfl.lara.synthesis.kingpong.rules.Events

abstract class PhysicalObject(
    init_name: Expr, 
    init_x: Expr,
    init_y: Expr,
    init_angle: Expr,
    init_visible: Expr,
    init_velocity: Expr,
    init_angularVelocity: Expr,
    init_density: Expr,
    init_friction: Expr,
    init_restitution: Expr,
    init_linearDamping: Expr,
    init_fixedRotation: Expr,
    init_color: Expr,
    init_tpe: BodyType
    ) extends GameObject(init_name)
      with Movable with SpeedSettable with Visiblable with Colorable with Rotationable { self =>

  tpe = init_tpe

  /** The inner body of this physical object. */
  def body: Body = _body
  protected def fixture: Fixture = body.getFixtureList()

  private var _body: Body = null
  private var bodyRemovedFromWorld = true

  protected val _bodyDef = new BodyDef()
  protected def fixtureDef: Seq[FixtureDef]
  protected var last_fixture: GameObject = null

  protected def bodyDef: BodyDef = {
    _bodyDef.`type` = tpe
    _bodyDef.position = Vec2(getOrElseInit(x), getOrElseInit(y))
    _bodyDef.linearDamping = getOrElseInit(linearDamping)
    //if (init_tpe == BodyType.DYNAMIC) {
    //  body_def.bullet = true
    //}
    _bodyDef
  }

  /** Initialize the fixture definition with common physical properties. */
  protected def initFixtureDef(fixtureDef: FixtureDef): FixtureDef = {
    fixtureDef.density = getOrElseInit(density)
    fixtureDef.friction = getOrElseInit(friction)
    fixtureDef.restitution = getOrElseInit(restitution)
    fixtureDef
  }

  private def removeFromWorld() = {
    game.world.world.destroyBody(body)
    bodyRemovedFromWorld = true // We keep the old body for flushing and loading properties.
  }
  
  def noVelocity_=(b: Boolean): Unit = {
    if(noVelocity && !b) {
      removeFromWorld() 
      tpe = BodyType.DYNAMIC
      addToWorld()
    } else if(!noVelocity && b) {
      removeFromWorld() 
      tpe = BodyType.STATIC
      addToWorld()
    }
  }
  
  protected def addToWorld() = {
    val body = game.world.world.createBody(bodyDef)
    fixtureDef foreach { fixture_definition =>
      body.createFixture(fixture_definition) 
      //TODO rename `last_fixture`, or change the code, there no logic here 
      last_fixture = this
    }
    body.setUserData(this)
    _body = body
    bodyRemovedFromWorld = false
  }
  
  override def setExistenceAt(time: Int) = {
    val exists = super.setExistenceAt(time)
    if(bodyRemovedFromWorld && exists) {
      addToWorld()
    }
    if(!bodyRemovedFromWorld && !exists) {
      removeFromWorld()
    }
    exists
  }
  
  /** The body mass. */
  def mass = body.getMass()
  
  override def reset(interpreter: Interpreter) = {
    super.reset(interpreter)
    body.setAwake(true)
  }
  
  // --------------------------------------------------------------------------
  // Properties
  // --------------------------------------------------------------------------

  val x = property[Float]("x", init_x,
    x => body.setTransform(Vec2(x, body.getPosition().y), body.getAngle()),
    () => body.getPosition().x
  )

  val y = property[Float]("y", init_y,
    y => body.setTransform(Vec2(body.getPosition().x, y), body.getAngle()),
    () => body.getPosition().y
  )

  val angle = property[Float]("angle", init_angle,
    a => body.setTransform(body.getPosition(), a),
    () => body.getAngle()
  )

  val velocity = property[Vec2]("velocity", init_velocity,
    v => body.setLinearVelocity(v),
    () => body.getLinearVelocity()
  )
  
  val color = simpleProperty[Int]("color", init_color)

  val angularVelocity = property[Float]("angular-velocity", init_angularVelocity,
    av => body.setAngularVelocity(av),
    () => body.getAngularVelocity()
  )

  val visible = simplePhysicalProperty[Boolean]("visible", init_visible,
    b => body.setActive(b)
  )

  val density = simplePhysicalProperty[Float]("density", init_density, 
    d => {
      fixture.setDensity(d)
      body.resetMassData() // update the body mass
    }
  )
    
  val friction = simplePhysicalProperty[Float]("friction", init_friction, 
    f => fixture.setFriction(f)
  )

  val restitution = simplePhysicalProperty[Float]("restitution", init_restitution,
    r => fixture.setRestitution(r)
  )

  val linearDamping = simplePhysicalProperty[Float]("linear-damping", init_linearDamping,
    d => body.setLinearDamping(d)
  )

  val fixedRotation = simplePhysicalProperty[Boolean]("fixed-rotation", init_fixedRotation,
    b => body.setFixedRotation(b)
  )

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

  protected def getOrElseInit[T : PongType](property: HistoricalProperty[T]): T = property.get match {
    case null  => game.evaluate[T](property.init)
    case value => value
  }
}

class Rectangle (
    val game: Game,
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
    init_linearDamping: Expr,
    init_fixedRotation: Expr,
    init_color: Expr,
    init_sensor: Expr,
    init_tpe: BodyType = BodyType.DYNAMIC
    ) extends PhysicalObject(init_name, init_x, init_y, init_angle, init_visible, init_velocity, init_angularVelocity,
                             init_density, init_friction, init_restitution, init_linearDamping, init_fixedRotation,
                             init_color, init_tpe)
      with ResizableRectangular
      with AngularRectangularContains {

  protected def fixtureDef = {
    val shape = new PolygonShape()
    shape.setAsBox(getOrElseInit(width) / 2, getOrElseInit(height) / 2)
  
    val fixture_def = new FixtureDef()
    initFixtureDef(fixture_def)
    fixture_def.shape = shape
    fixture_def.isSensor = getOrElseInit(sensor)
    Seq(fixture_def)
  }
  
  protected def shape: PolygonShape = fixture.getShape().asInstanceOf[PolygonShape]
  
  val width: HistoricalRWProperty[Float] = simplePhysicalProperty[Float]("width", init_width, 
    w => {
      shape.setAsBox(w/2, height.get/2)
      body.resetMassData() // update the body mass
    }
  )
    
  val height: HistoricalRWProperty[Float] = simplePhysicalProperty[Float]("height", init_height,
    h => {
      shape.setAsBox(width.get/2, h/2)
      body.resetMassData() // update the body mass
    }
  )

  val sensor = simplePhysicalProperty[Boolean]("sensor", init_sensor,
    s => fixture.setSensor(s)
  )

  addToWorld()

  def rawCopy(f: HistoricalProperty[_] => Expr) = {
    new Rectangle(game, f(name), f(x), f(y), f(angle), f(width), f(height), f(visible), f(velocity), f(angularVelocity),
                  f(density), f(friction), f(restitution), f(linearDamping), f(fixedRotation), f(color), f(sensor), tpe)
  }
  
  override def contains(pos: Vec2) = super[AngularRectangularContains].contains(pos)
}

class Character (
    val game: Game,
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
    init_linearDamping: Expr,
    init_fixedRotation: Expr,
    init_color: Expr,
    init_tpe: BodyType = BodyType.DYNAMIC
    ) extends PhysicalObject(init_name, init_x, init_y, init_angle, init_visible, init_velocity, init_angularVelocity,
                             init_density, init_friction, init_restitution, init_linearDamping, init_fixedRotation,
                             init_color, init_tpe)
      with ResizableRectangular 
      with InputManager {
  
  protected def fixtureDef = {
    val shape = new PolygonShape()
    shape.setAsBox(getOrElseInit(width) / 2, getOrElseInit(height) / 2)

    val shapeSensor = new CircleShape()
    shapeSensor.m_radius = getOrElseInit(width) / 2
    shapeSensor.m_p.set(0f, getOrElseInit(height) / 2)

    val fixture_def = new FixtureDef()
    fixture_def.shape = shape
    initFixtureDef(fixture_def)
    
    val fixture_sensor = new FixtureDef()
    fixture_sensor.shape = shapeSensor
    fixture_sensor.isSensor = true
    Seq(fixture_def, fixture_sensor)
  }
  
  protected def shape: PolygonShape = {
    var f = fixture
    while(f != null) {
      f.getShape match {
        case p: PolygonShape => return p
        case _ => f = f.getNext
      }
    }
    throw new Error("Tried to access unexisting PolygonShape in Character")
  }
  
   def collectInput(from: Context) = {
    import Events._
    var g  = false
    from.events foreach {
      case BeginContact(c, objectA, objectB)  =>
        g = g || (objectA == last_fixture ||  objectB == last_fixture)
      case CurrentContact(c, objectA, objectB) => 
        g = g || (objectA == last_fixture || objectB == last_fixture)
      case _ => 
    }
    grounded.set(g)
  } 
   
  // --------------------------------------------------------------------------
  // Properties
  // --------------------------------------------------------------------------
  
  val width: HistoricalRWProperty[Float] = simplePhysicalProperty[Float]("width", init_width,
    w => {
      shape.setAsBox(w/2, height.get/2)
      body.resetMassData() // update the body mass
    }
  )
  
  val height: HistoricalRWProperty[Float] = simplePhysicalProperty[Float]("height", init_height,
    h => {
      shape.setAsBox(width.get/2, h/2)
      body.resetMassData() // update the body mass
    }
  )
    
  val grounded = simpleProperty[Boolean]("grounded", false)

  addToWorld()

  def rawCopy(f: HistoricalProperty[_] => Expr) = {
    new Character(game, f(name), f(x), f(y), f(angle), f(width), f(height), f(visible), f(velocity), f(angularVelocity),
                  f(density), f(friction),  f(restitution), f(linearDamping), f(fixedRotation), f(color), tpe)
  }
}

class Circle(
    val game: Game,
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
    init_linearDamping: Expr,
    init_fixedRotation: Expr,
    init_color: Expr,
    init_sensor: Expr,
    init_tpe: BodyType = BodyType.DYNAMIC
    ) extends PhysicalObject(init_name, init_x, init_y, 0, init_visible, init_velocity, init_angularVelocity,
                             init_density, init_friction, init_restitution, init_linearDamping, init_fixedRotation,
                             init_color, init_tpe)
      with ResizableCircular {
  
  protected def fixtureDef = {
    val shape = new CircleShape()
    shape.m_radius = getOrElseInit(radius)
  
    val fixture_def = new FixtureDef()
    fixture_def.shape = shape
    fixture_def.isSensor = getOrElseInit(sensor)
    initFixtureDef(fixture_def)
    Seq(fixture_def)
  }
  
  val radius = simplePhysicalProperty[Float]("radius", init_radius,
    r => {
      fixture.getShape().m_radius = r
      body.resetMassData() // update the body mass
    }
  )
  
  val sensor = simplePhysicalProperty[Boolean]("sensor", init_sensor, 
    s => fixture.setSensor(s)
  )

  addToWorld()

  def rawCopy(f: HistoricalProperty[_] => Expr) = {
    new Circle(game, f(name), f(x), f(y), f(radius), f(visible), f(velocity), f(angularVelocity), f(density),
               f(friction), f(restitution), f(linearDamping), f(fixedRotation), f(color), f(sensor), tpe)
  }
}
