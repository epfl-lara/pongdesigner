package ch.epfl.lara.synthesis.kingpong

import scala.collection.mutable.ArrayBuffer
import scala.collection.Traversable
import scala.collection.mutable.HashMap
import android.graphics.RectF
import ch.epfl.lara.synthesis.kingpong.ast._

/**
 * GameShapes
 * Describes shapes that can be used by a game.
 * Each modifiable variable has three states
 * 1) The initial state at t = 0
 * 2) The current state (visible)
 * 3) The changing state
 */
object GameShapes {
  import TriggerEvent._
  
  /** Gravity in the game */
  trait ForceField {
    def force_x: Float
    def force_y: Float
  }
  
  object ZeroGravity extends ForceField {
    def force_x = 0
    def force_y = 0
  }
  
  class CustomGravity extends Shape with ForceField {
    override def moveDuring(deltaTime: Long, timestamp: Long) = { }
    def force_x = velocity_x
    def force_y = velocity_y
    def selectableBy(xCursor: Float, yCursor: Float): Boolean = {
      x > xCursor - selectableAreaRadius && x < xCursor - selectableAreaRadius &&
      y > yCursor - selectableAreaRadius && y < yCursor - selectableAreaRadius
    }
    def isOutsideRectangle(xMin: Float, yMin: Float, xMax: Float, yMax: Float): Boolean = {
      x < xMin || x > xMax || y < yMin || y > yMax
    }
    def distanceSelection(xCursor: Float, yCursor: Float): Float = {
      val dx = xCursor-x
      val dy = yCursor-y
      val res = Math.sqrt(dx*dx + dy*dy).toFloat
      res
    }
    def getBoundingBox(r: RectF) = {
      r.set(x - selectableAreaRadius, y - selectableAreaRadius, x + selectableAreaRadius, y + selectableAreaRadius)
    }
    def mass = 0
  }
  
  trait BouncingFriction {
    def frictionCollision: Float
    def frictionTangent: Float
    def apply(s: GameShapes.Shape) = { s.velocity_x *= frictionCollision }
    def apply(s: GameShapes.Shape, xUnit: Float, yUnit: Float) = {
      var vCollision = s.velocity_x * xUnit + s.velocity_y * yUnit
      var vTangent = s.velocity_x * -yUnit + s.velocity_y * xUnit
      vCollision *= frictionCollision
      vTangent *= frictionTangent
      s.velocity_x = vCollision * xUnit + vTangent * -yUnit
      s.velocity_y = vCollision * yUnit + vTangent * xUnit
    }
  }
  
  object BasicFriction extends BouncingFriction {
    def frictionCollision = 0.1f
    def frictionTangent = 0.95f
  }
  
  object ElasticFriction extends BouncingFriction {
    override def frictionCollision = 1.0f
    override def frictionTangent = 1.0f
  }
  
  class CustomFriction(var frictionCollision: Float = 0.9f, var frictionTangent: Float = 0.95f) extends BouncingFriction {
  }
  
  /** Maximal distance to which an object can be selected */
  var selectableAreaRadius = 30.0f
  
  final val DEFAULT_COLOR = 0xFF00FFFF
  final val DEFAULT_NAME = "shape"
  final val DEFAULT_GRAVITY = ZeroGravity
  final val DEFAULT_FRICTION = ElasticFriction
  
  /**
   * Computes a new name based on the context and the given baseName
   */
  def getBrandNewName(baseName: String, context: HashMap[String, Expression]) = {
    var prefix = DEFAULT_NAME
    if(baseName != null && baseName != "") { 
      //prefix = "\\d*\\z".r.replaceFirstIn(baseName, "")
      var i = baseName.length - 1
      while(i >= 0 && baseName.charAt(i) >= '0' && baseName.charAt(i) <= '9') {
        i = i - 1
      }
      if(i == -1) {
        prefix = DEFAULT_NAME
      } else {
        prefix = baseName.substring(0, i+1)
      }
    }
    if(prefix == "") prefix = GameShapes.DEFAULT_NAME
    var resultingName = prefix
    var needPostFix = false
    var postFix = 1
    var foundName = false
    while(!foundName) {
      foundName = true
      if(context contains resultingName) {
        if(!needPostFix) {
          needPostFix = true
          resultingName = prefix + postFix
          foundName = false
        } else {
          postFix += 1
          resultingName = prefix + postFix
          foundName = false
        }
      }
    }
    resultingName
  }
  
  trait NamedObject {
    /** The name of the shape and methods to modify it */
    var mName: String = DEFAULT_NAME
    def named(n: String): this.type = {
      mName = n
      this
    }
    def name_=(n: String) = this named n
    def name: String = mName
    
    /** Cloning method */
    //def clone() //TODO: to implement
  }
  
  /** General shape */
  abstract class Shape extends NamedObject {
    override def toString() = mName

    /** Gameflow-independent parameters */
    protected var attachedGame: Game = null
    def attach(game: Game) = attachedGame = game
    def delete() = attachedGame.deleteShape(this)

    /** noVelocity = true means that the object will not be moved by its intrinsic velocity */
    var noVelocity = false
    
    /** Gravity */
    var gravity: ForceField = DEFAULT_GRAVITY
    
    /** Bouncing friction */
    var friction: BouncingFriction = DEFAULT_FRICTION
    
    /** Color expressions */
    var color: Int = DEFAULT_COLOR
    var prev_color: Int = color
    var start_color: Expression = Expression.NONE
    var copy_color: Int = color

    /** Physical attributes */
    def mass: Float
    
    /** Velocity expressions */
    var velocity_x = 0.0f
    var velocity_y = 0.0f
    def velocity = Math.sqrt(velocity_x * velocity_x + velocity_y * velocity_y).toFloat
    def velocity_=(v: Float) = {
      val oldVelocity = velocity
      if(oldVelocity != 0) {
        velocity_x = velocity_x / oldVelocity * v
        velocity_y = velocity_y / oldVelocity * v
      } else {
        val a = Math.toRadians(90-mAngle)
        velocity_x = v*Math.cos(a).toFloat
        velocity_y = v*Math.sin(a).toFloat
      }
    }
    var prev_velocity_x = 0f
    var prev_velocity_y = 0f
    def prev_velocity = Math.sqrt(prev_velocity_x * prev_velocity_x + prev_velocity_y * prev_velocity_y).toFloat
    var start_velocity_x: Expression = Expression.NONE
    var start_velocity_y: Expression = Expression.NONE
    var copy_velocity_x = 0f
    var copy_velocity_y = 0f
    private var mAngle = 0f
    def angle = {
      if(velocity_x != 0 || velocity_y != 0) {
        Math.toDegrees(Math.atan2(velocity_x, velocity_y)).toFloat
        
      } else mAngle
    }
    def angle_=(ag: Float) = {
      this synchronized {
        mAngle = ag
        val v = velocity
        val a = Math.toRadians(90-ag)
        velocity_x = v*Math.cos(a).toFloat
        velocity_y = v*Math.sin(a).toFloat
      }
    }
    def prev_angle = if(prev_velocity_x != 0 || prev_velocity_y != 0) Math.toDegrees(Math.atan2(prev_velocity_x, prev_velocity_y)).toFloat else 0f
    
    /** Position expressions */
    private var mX = 0f
    private var mY = 0f
    def x_=(xp: Float) = {
      mX = xp
    }
    def y_=(yp: Float) = {
      if(mName == "ball1") {
        mX = mX + 0.0f
      }
      mY = yp
    }
    def x = mX
    def y = mY
    var prev_x = 0f
    var prev_y = 0f
    var start_x: Expression = Expression.NONE
    var start_y: Expression = Expression.NONE
    var copy_x = 0f
    var copy_y = 0f
    
    /** Visibility */
    var visible = true
    var prev_visible: Boolean = true
    var start_visible: Expression = Expression.NONE
    var copy_visible = true
    
    /** Active in the current game */
    var active = true
    
    /** Moves this shape for a small amount of time */
    def moveDuring(deltaTime: Long, timestamp: Long) = {
      if(!noVelocity) {
        velocity_x += gravity.force_x * deltaTime
        velocity_y += gravity.force_y * deltaTime
        mX += velocity_x * deltaTime
        mY += velocity_y * deltaTime
      } else {
        val previousX = history_x(timestamp, x)
        val previousY = history_y(timestamp, y)
        if(mX != previousX) {
          val computedVelocityX = (mX - previousX) / deltaTime.toFloat
          if(Math.abs(computedVelocityX) < 0.5) {
            velocity_x = computedVelocityX
          }
        } else {
          velocity_x = 0
        }
        if(mY != previousY) {
          val computedVelocityY = (mY - previousY) / deltaTime.toFloat
          if(Math.abs(computedVelocityY) < 0.5) {
            velocity_y = computedVelocityY
          }
        } else {
          velocity_y = 0
        }
      }
    }
    /** Returns true if this shape is selectable at this coordinates */
    def selectableBy(x: Float, y: Float): Boolean
    def isOutsideRectangle(xMin: Float, yMin: Float, xMax: Float, yMax: Float): Boolean
    def distanceSelection(x: Float, y: Float): Float
    def getBoundingBox(r: RectF)
    def prevCenterX: Float = x
    def prevCenterY: Float = y
    def centerX: Float = x
    def centerY: Float = y
    
    /** Mode switching */
    def initialize(x: Expression, y: Expression) {
      start_x = x
      start_y = y
    }

    /** Stores the current state of the shape as its initial state */
    def storeInitialState(overrideExisting: Boolean) = {
      if(start_x.isNone || overrideExisting) {
        start_x = EConstant(x)
      }
      if(start_y.isNone || overrideExisting) start_y = EConstant(y)
      if(start_velocity_x.isNone || overrideExisting) start_velocity_x = EConstant(velocity_x)
      if(start_velocity_y.isNone || overrideExisting) start_velocity_y = EConstant(velocity_y)
      if(start_color.isNone || overrideExisting) start_color = EConstant(color)
      if(start_visible.isNone || overrideExisting) start_visible = EConstant(visible)
    }
    
    /** Store a copy of the values in the previous variables */
    def storePrevValues() = {
      prev_x = x
      prev_y = y
      prev_velocity_x = velocity_x
      prev_velocity_y = velocity_y
      prev_color = color
      prev_visible = visible
    }
    
    /** Takes a snapshot of the variables */
    def snapShot() = {
      copy_x = x
      copy_y = y
      copy_velocity_x = velocity_x
      copy_velocity_y = velocity_y
      copy_color = color
      copy_visible = visible
    }
    /** Reverts to the snapshot */
    def revertToSnapShot() = {
      x = copy_x
      y = copy_y 
      velocity_x = copy_velocity_x 
      velocity_y = copy_velocity_y 
      color = copy_color 
      visible = copy_visible
    }
    /** Verify that the values are identical to the previous values.*/
    def verifyTests():Boolean = {
      (x == prev_x || CodeGenerator.almostTheSame(x - copy_x, prev_x - copy_x)) &&
      (y == prev_y || CodeGenerator.almostTheSame(y - copy_y, prev_y - copy_y)) &&
      (velocity_x == prev_velocity_x || CodeGenerator.almostTheSame(velocity_x - copy_velocity_x, prev_velocity_x - copy_velocity_x)) &&
      (velocity_y == prev_velocity_y || CodeGenerator.almostTheSame(velocity_y - copy_velocity_y, prev_velocity_y - copy_velocity_y)) &&
      color == prev_color &&
      visible == prev_visible
    }
    /** Restores the previous state */
    def restorePrev() = {
      mX = prev_x
      mY = prev_y
      velocity_x = prev_velocity_x
      velocity_y = prev_velocity_y
      color = prev_color
    }
    /** Switch back to start values */
    def reset(context: HashMap[String, Expression]) = {
      if(!start_x.isNone) mX = start_x.evaluate(context).number_value
      if(!start_y.isNone) mY = start_y.evaluate(context).number_value
      if(!start_velocity_x.isNone) velocity_x = start_velocity_x.evaluate(context).number_value
      if(!start_velocity_y.isNone) velocity_y = start_velocity_y.evaluate(context).number_value
      if(!start_color.isNone) color = start_color.evaluate(context).number_value.toInt
      if(!start_visible.isNone) visible = start_visible.evaluate(context).boolean_value
    }
    
    /** History of each of the parameters */
    var history_x = new ParameterHistoryCollection[Float]()
    var history_y = new ParameterHistoryCollection[Float]()
    var history_velocity_x= new ParameterHistoryCollection[Float]()
    var history_velocity_y = new ParameterHistoryCollection[Float]()
    var history_color = new ParameterHistoryCollection[Int]()
    var history_visible = new ParameterHistoryCollection[Boolean]()
    var history_outofscreen = new ParameterHistoryCollection[Boolean]()
    val histories = List(history_x, history_y, history_velocity_x, history_velocity_y, history_color, history_visible, history_outofscreen)
    
    /** Returns true if the shape just moved */
    def movedAtTime(timestamp: Long): Boolean = {
      mX != history_x(timestamp, mX) || mY != history_y(timestamp, mY)
    }
    
    /** Adds the current parameter values to the current history */
    def storeState(timestamp: Long) = {
      this synchronized {
        history_x.addOrReplaceValue(timestamp, mX)
        history_y.addOrReplaceValue(timestamp, mY)
        history_velocity_x.addOrReplaceValue(timestamp, velocity_x)
        history_velocity_y.addOrReplaceValue(timestamp, velocity_y)
        history_color.addOrReplaceValue(timestamp, color)
        history_visible.addOrReplaceValue(timestamp, visible)
        history_outofscreen.addOrReplaceValue(timestamp, isOutsideRectangle(0, 0, attachedGame.screenWidth, attachedGame.screenHeight))
      }
    }

    /** Changes all the parameters to have the value at the given time. */
    def returnToTime(timestamp: Long) = {
      this synchronized {
        x = history_x(timestamp, x)
        y = history_y(timestamp, y)
        velocity_x = history_velocity_x(timestamp, velocity_x)
        velocity_y = history_velocity_y(timestamp, velocity_y)
        color = history_color(timestamp, color)
        visible = history_visible(timestamp, visible)
        storePrevValues()
      }
    }
    /** Removes values that are newer than the given timestamp */
    def keepOnlyValuesBeforeAndIncluding(timestamp: Long) = {
      histories.foreach {
        history => history.keepOnlyValuesBeforeAndIncluding(timestamp)
      }
    }

    /**
     * Raises triggers if any, and return true
     */
    def raiseTriggers(timestamp: Long, ruleToStopBefore: ReactiveRule = null): Boolean = {
      if(attachedGame != null) {
        if(history_outofscreen(timestamp, false) && !history_outofscreen(timestamp-1, false)) {
          attachedGame.triggerEvents.addEvent(attachedGame.currentTime, BEYOND_SCREEN_EVENT, this, null, Math.min(Math.max(mX, 0), attachedGame.screenWidth), Math.min(Math.max(mY, 0), attachedGame.screenHeight), 0, 0)
        }
      }
      true
    }
    
    /** Iterates a function for each of the attributes*/
    def foreachAttribute(f: String => Unit): Unit = {
      f("x")
      f("y")
      f("angle")
      f("velocity")
      f("color")
      f("visible")
    }
    
  }
  
  /**
   * Rectangle shape
   */
  object Rectangle {
    /** Creates a rectangle from position and size */
    def build(x: Float, y: Float, width: Int, height:Int): Rectangle = {
      val result = new Rectangle()
      result.width = width
      result.height = height
      result.x = x
      result.y = y
      result
    }
    /** Creates a rectangle from its four coordinates*/
    def buildFromBounds(x: Float, y: Float, x2: Float, y2:Float): Rectangle = {
      val result = new Rectangle()
      result.width = (x2-x).toInt + 1
      result.height = (y2-y).toInt + 1
      result.x = x
      result.y = y
      result
    }
  }
  /**
   * Rectangular shape definition
   */
  trait Rectangular extends Shape {
    var width: Int = 0
    var height: Int = 0
    var prev_width: Int = 0
    var prev_height: Int = 0
    var start_width: Expression = Expression.NONE
    var start_height: Expression = Expression.NONE
    var copy_width: Int = 0
    var copy_height: Int = 0
    
    def mass = width * height * 1.0f
    
    def selectableBy(xCursor: Float, yCursor: Float):Boolean = {
      xCursor > x - selectableAreaRadius &&
      xCursor < x + width + selectableAreaRadius &&
      yCursor > y - selectableAreaRadius &&
      yCursor < y + height + selectableAreaRadius
    }
    def distanceSelection(xCursor: Float, yCursor: Float):Float = {
      val dx = if(xCursor < x) {
        x - xCursor
      } else if(xCursor > x + width) {
        xCursor - x - width
      } else 0
      val dy = if(yCursor < y) {
        y - yCursor
      } else if(yCursor > y + height) {
        yCursor - y - height
      } else 0
      Math.sqrt(dx*dx + dy*dy).toFloat
    }
    def getBoundingBox(r: RectF) = {
      r.set(x, y, x+width, y+height)
    }
    /** If the shape is outside the rectangle */
    def isOutsideRectangle(xMin: Float, yMin: Float, xMax: Float, yMax: Float) = {
      x + width < xMin || y + height < yMin || x > xMax || y > yMax
    }
    
    override def centerX = x + width / 2
    override def centerY = y + height / 2
    override def prevCenterX = prev_x + prev_width / 2
    override def prevCenterY = prev_y + prev_height / 2
    var initializedFromBounds: Boolean = false
    
    var start_x2: Expression = Expression.NONE
    var start_y2: Expression = Expression.NONE
    
    def initialize(x: Expression, y: Expression, width: Expression, height: Expression):this.type = {
      initialize(x, y)
      start_width = width
      start_height = height
      start_x2 = x + width
      start_y2 = y + height
      this
    }
    
    def initializeFromBounds(x: Expression, y: Expression, x2: Expression, y2: Expression):this.type = {
      initialize(x, y)
      start_x2 = x2
      start_y2 = y2
      start_width = x2 - x
      start_height = y2 - y
      initializedFromBounds = true
      this
    }
    override def storeInitialState(overrideExisting: Boolean) = {
      super.storeInitialState(overrideExisting)
      if(start_width.isNone || overrideExisting) start_width = EConstant(width)
      if(start_height.isNone || overrideExisting) start_height = EConstant(height)
    }
    override def storePrevValues() = {
      super.storePrevValues()
      prev_width = width
      prev_height = height
    }
    override def snapShot() = {
      super.snapShot()
      copy_width = width
      copy_height = height
    }
    override def revertToSnapShot() = {
      super.revertToSnapShot()
      width = copy_width
      height = copy_height 
    }
    override def verifyTests():Boolean = {
      super.verifyTests() &&
      width == prev_width &&
      height == prev_height
    }
    override def restorePrev() = {
      super.restorePrev()
      width = prev_width
      height = prev_height
    }
    override def reset(context: HashMap[String, Expression]) = {
      super.reset(context)
      if(!start_width.isNone) width = start_width.evaluate(context).number_value.toInt
      if(!start_height.isNone) height = start_height.evaluate(context).number_value.toInt
    }
    
    var history_width = new ParameterHistoryCollection[Int]()
    var history_height = new ParameterHistoryCollection[Int]()
    
    override def storeState(timestamp: Long) = {
      this synchronized {
        super.storeState(timestamp)
        history_width.addOrReplaceValue(timestamp, width)
        history_height.addOrReplaceValue(timestamp, height)
      }
    }
    override def returnToTime(timestamp: Long) = {
      this synchronized {
        width = history_width(timestamp, width)
        height = history_height(timestamp, height)
        super.returnToTime(timestamp)
      }
    }
    override def keepOnlyValuesBeforeAndIncluding(timestamp: Long) = {
      super.keepOnlyValuesBeforeAndIncluding(timestamp)
      history_width.keepOnlyValuesBeforeAndIncluding(timestamp)
      history_height.keepOnlyValuesBeforeAndIncluding(timestamp)
    }
    
    override def foreachAttribute(f: String => Unit): Unit = {
      super.foreachAttribute(f)
      f("width")
      f("height")
    }
  }
  class Rectangle extends Rectangular {
  }
  object Camera {
    def build(x: Float, y: Float, width: Int, height:Int): Camera= {
      val result = new Camera()
      result.width = width
      result.height = height
      result.x = x
      result.y = y
      result
    }
  }
  class Camera extends Rectangular with Category[Shape] {
    mName = "Camera"
    override def selectableBy(xCursor: Float, yCursor: Float):Boolean = false
    override def distanceSelection(xCursor: Float, yCursor: Float):Float = 0
    override def getBoundingBox(r: RectF) = {
      r.set(x, y, x+width, y+height)
    }
    override def mass = 0
    
    var target: GameShapes.Shape = null
    override def x = {
      if(target != null) target.centerX - (width/2) else super.x
    }
    override def y = {
      if(target != null) target.centerY - (height/2) else super.y
    }
    
    override def remove(s: GameShapes.Shape) = {
      super.remove(s)
      if(content.size == 0) {
        target = null
      }
    }
    
    override def add(s: GameShapes.Shape) = {
      if(content.size == 0) {
        super.add(s)
        target = s
      } else {
        reset()
        super.add(s)
        target = s
      }
    }
    
    override def reset() = {
      super.reset()
      target = null
    }
  }
  
  /**
   * Circle shape
   */
  object Circle {
    /** Creates a circle from position and radius */
    def build(x: Float, y: Float, radius: Float):Circle = {
      val result = new Circle()
      result.radius = radius
      result.x = x
      result.y = y
      result
    }
  }
  class Circle extends Shape {
    var radius: Float = 0
    var prev_radius: Float = 0
    var start_radius: Expression = Expression.NONE
    var copy_radius: Float = 0
    
    def mass = 3.14f*radius*radius
    def selectableBy(xCursor: Float, yCursor: Float) = {
      val distX = xCursor-x
      val distY = yCursor-y
      val r = radius + selectableAreaRadius
      distX*distX + distY*distY < r*r
    }
    def distanceSelection(xCursor: Float, yCursor: Float):Float = {
      val dx = xCursor-x
      val dy = yCursor-y
       val r = radius + selectableAreaRadius
       val res = Math.sqrt(dx*dx + dy*dy).toFloat - radius
       if(res < 0) 0 else res
    }
    def getBoundingBox(r: RectF) = {
      r.set(x - radius, y - radius, x + radius, y + radius)
    }
    def isOutsideRectangle(xMin: Float, yMin: Float, xMax: Float, yMax: Float) = {
      x + radius < xMin || y + radius < yMin || x - radius > xMax || y - radius > yMax
    }
    def initialize(x: Expression, y: Expression, radius: Expression):Circle = {
      initialize(x, y)
      start_radius = radius
      this
    }
    override def storeInitialState(overrideExisting: Boolean) = {
      super.storeInitialState(overrideExisting)
      if(start_radius.isNone || overrideExisting) start_radius = EConstant(radius)
    }
    override def storePrevValues() {
      super.storePrevValues()
      prev_radius = radius
    }
    override def snapShot() {
      super.snapShot()
      copy_radius = radius
    }
    override def revertToSnapShot() = {
      super.revertToSnapShot()
      radius = copy_radius
    }
    override def verifyTests():Boolean = {
      super.verifyTests() &&
      radius == prev_radius
    }
    override def restorePrev() {
      super.restorePrev()
      radius = prev_radius
    }
    override def reset(context: HashMap[String, Expression]) = {
      super.reset(context)
      if(!start_radius.isNone) radius = start_radius.evaluate(context).number_value
    }
    var history_radius = new ParameterHistoryCollection[Float]()
    
    override def storeState(timestamp: Long) {
      this synchronized {
        super.storeState(timestamp)
        history_radius.addOrReplaceValue(timestamp, radius)
      }
    }
    override def returnToTime(timestamp: Long) = {
      this synchronized {
        radius = history_radius(timestamp, radius)
        super.returnToTime(timestamp)
      }
    }
    override def keepOnlyValuesBeforeAndIncluding(timestamp: Long) = {
      super.keepOnlyValuesBeforeAndIncluding(timestamp)
      history_radius.keepOnlyValuesBeforeAndIncluding(timestamp)
    }
    override def foreachAttribute(f: String => Unit): Unit = {
      super.foreachAttribute(f)
      f("radius")
    }
  }
  
  /**
   * Creator for an IntegerBox
   */
  object IntegerBox {
    /** Creates a displayed integer from position, text size and value.
     *  Width is added for selection*/
    def build(x: Float, y: Float, width: Int, height:Int, value:Int):IntegerBox = {
      val result = new IntegerBox
      result.width = width
      result.height = height
      result.value = value
      result.x = x
      result.y = y
      result
    }
  }
  /**
   * A shape defining an integer.
   */
  class IntegerBox extends Rectangular {
    def value = mValue
    def value_=(v: Int) = setValue(v)
    def prev_value = mPrevValue
    def prev_value_= (v: Int) = setPrevValue(v)
    var start_value : Expression = Expression.NONE
    var copy_value: Int = 0

    noVelocity = true
    
    private var mValue: Int = 0
    private var stringEquivalent = "0"
    def setValue(v: Int) = {
      mValue = v
      stringEquivalent = mValue.toString()
      if(stringEquivalent != mPrevValue.toString) {
        stringEquivalentModify = mPrevValue.toString + "->" + stringEquivalent 
      } else {
        stringEquivalentModify = stringEquivalent
      }
    }
    private var mPrevValue = 0
    private var stringEquivalentModify = "0"
    def setPrevValue(v: Int) = {
      mPrevValue = v
      if(stringEquivalent != mPrevValue.toString) {
        stringEquivalentModify = mPrevValue.toString + "->" + stringEquivalent 
      } else {
        stringEquivalentModify = stringEquivalent
      }
    }
    def ++ = setValue(mValue + 1)
    def -- = setValue(mValue - 1)
    def +=(valueToAdd: Int) = {
      setValue(mValue + valueToAdd)
    }
    def increment() = setValue(mValue + 1)
    
    def getString = stringEquivalent
    def getStringModify = stringEquivalentModify

    def initialize(x: Expression, y: Expression, width: Expression, height: Expression, value: Expression):IntegerBox = {
      super.initialize(x, y, width, height)
      start_value = value
      this
    }
    override def storePrevValues() = {
      super.storePrevValues()
      setPrevValue(mValue)
    }
    override def snapShot() {
      super.snapShot()
      copy_value = mValue
    }
    override def revertToSnapShot() {
      super.revertToSnapShot()
      mValue = copy_value
    }
    override def verifyTests():Boolean = {
      super.verifyTests() &&
      mValue == mPrevValue
    }
    override def restorePrev() = {
      super.restorePrev()
      setValue(mPrevValue)
    }
    override def storeInitialState(overrideExisting: Boolean = false) = {
      super.storeInitialState(overrideExisting)
      if(start_value.isNone || overrideExisting) start_value = EConstant(mValue)
    }
    override def reset(context: HashMap[String, Expression]) = {
      super.reset(context)
      if(!start_value.isNone) setValue(start_value.evaluate(context).number_value.toInt)
    }
    
    var history_value = new ParameterHistoryCollection[Int]()
    override def storeState(timestamp: Long) {
      this synchronized {
        super.storeState(timestamp)
        history_value.addOrReplaceValue(timestamp, value)
      }
    }
    
    override def returnToTime(timestamp: Long) = {
      this synchronized {
        value = history_value(timestamp, value)
        super.returnToTime(timestamp)
      }
    }
    override def keepOnlyValuesBeforeAndIncluding(timestamp: Long) = {
      super.keepOnlyValuesBeforeAndIncluding(timestamp)
      history_value.keepOnlyValuesBeforeAndIncluding(timestamp)
    }
    
    override def raiseTriggers(timestamp: Long, ruleToStopBefore: ReactiveRule = null): Boolean = {
      if(attachedGame != null) {
        val oldValue = history_value(timestamp-1, value)
        val newValue = history_value(timestamp, value)
        if(oldValue != newValue) attachedGame.onIntegerChangeEvent(this, newValue, ruleToStopBefore) else true
      } else {
        true
      }
    }
    override def foreachAttribute(f: String => Unit): Unit = {
      super.foreachAttribute(f)
      f("value")
    }
  }
  
  /**
   * Text shapes
   */
  object TextBox {
    /** Creates a displayed text from position, size, value and visibility */
    def build(x: Float, y: Float, width: Int, height:Int, text:String):TextBox = {
      val result = new TextBox
      result.width = width
      result.height = height
      result.text = text
      result.x = x
      result.y = y
      result
    }
  }
  /**
   * A rectangular shape displaying text.
   */
  class TextBox extends Rectangular {
    var mText:String = null
    var mPrevText:String = null
    var start_text:Expression = Expression.NONE
    var copy_text: String = null
    
    private def stringEquivalent = mText
    private var stringEquivalentModify: String = null
    
    def text = mText
    def text_=(t: String) = setText(t)
    def prev_text = mPrevText
    def prev_text_= (v: String) = setPrevText(v)
    
    def setText(t: String) = {
      mText = t
      if(mText !=  mPrevText) {
        stringEquivalentModify = mPrevText + "->" + stringEquivalent
      } else {
        stringEquivalentModify = stringEquivalent
      }
    }
    def setPrevText(t: String) = {
      mPrevText = t
      if(mText !=  mPrevText) {
        stringEquivalentModify = mPrevText + "->" + stringEquivalent
      } else {
        stringEquivalentModify = stringEquivalent
      }
    }
    
    def getString = stringEquivalent
    def getStringModify = stringEquivalentModify
    
    def initialize(x: Expression, y: Expression, width: Expression, height: Expression, text: Expression):TextBox = {
      super.initialize(x, y, width, height)
      start_text = text
      this
    }
    override def storePrevValues() = {
      super.storePrevValues()
      setPrevText(mText)
    }
    override def snapShot() {
      super.snapShot()
      copy_text = mText
    }
    override def revertToSnapShot() {
      super.revertToSnapShot()
      mText = copy_text
    }
    override def verifyTests():Boolean = {
      super.verifyTests() &&
      mText == mPrevText
    }
    override def restorePrev() = {
      super.restorePrev()
      setText(mPrevText)
      visible = prev_visible
    }
    override def storeInitialState(overrideExisting: Boolean = false) = {
      super.storeInitialState(overrideExisting)
      if(start_text.isNone || overrideExisting) start_text = EConstant(mText)
    }
    override def reset(context: HashMap[String, Expression]) = {
      super.reset(context)
      if(!start_text.isNone) text = start_text.evaluate(context).string_value
    }
    
    var history_text = new ParameterHistoryCollection[String]()
    override def storeState(timestamp: Long) {
      this synchronized {
        super.storeState(timestamp)
        history_text.addOrReplaceValue(timestamp, mText)
      }
    }
    override def returnToTime(timestamp: Long) = {
      this synchronized {
        text = history_text(timestamp, text)
        super.returnToTime(timestamp)
      }
    }
    override def keepOnlyValuesBeforeAndIncluding(timestamp: Long) = {
      super.keepOnlyValuesBeforeAndIncluding(timestamp)
      history_text.keepOnlyValuesBeforeAndIncluding(timestamp)
    }
    
    override def foreachAttribute(f: String => Unit): Unit = {
      super.foreachAttribute(f)
      f("text")
    }
  }
  
  /**
   * An Arena contains the shapes forming an area
   */
  object Arena {
    def build() = new Arena()
  }
  
  /**
   * An Arena contains the shapes forming an area
   */
  class Arena extends Traversable[Shape] {
    /** Gameflow-independent parameters */
    protected var attachedGame: Game = null
    def attach(game: Game) = attachedGame = game
    
    var mName: String = null
    def name_=(n: String): String = null
    def name: String = mName
    private var mArrayShape = new ArrayBuffer[Shape]
    def +=(s: Shape) = {
      if(attachedGame != null) {
        attachedGame.recordShapeAdditionToArena(this, s)
      }
      mArrayShape += s
    }
    def -=(s: Shape) = {
      mArrayShape -= s
    }
    def foreach[U](f: Shape => U):Unit = {
      var i = 0
      var size = mArrayShape.size
      while(i <= size - 1) {
        f(mArrayShape(i))
        i += 1
      }
    }
    def foreachVisible[U](f: Shape => U):Unit = {
      var i = 0
      var size = mArrayShape.size
      while(i <= size - 1) {
        val s1 = mArrayShape(i)
        if(s1.visible) f(s1)
        i += 1
      }
    }
    def foreachVisiblePair(f: (Shape, Shape) => Unit) {
      var i = 0
      var j = 0
      var size = mArrayShape.size
      while(i <= size - 2) {
        j = i + 1
        val s1 = mArrayShape(i)
        if(s1.visible) {
          while(j <= size - 1) {
            val s2 = mArrayShape(j)
            if(s2.visible) f(s1, s2)
            j += 1
          }
        }
        i += 1
      }
    }
    def foreachPair(f: (Shape, Shape) => Unit) {
      var i = 0
      var j = 0
      var size = mArrayShape.size
      while(i <= size - 2) {
        j = i + 1
        val s1 = mArrayShape(i)
        while(j <= size - 1) {
          val s2 = mArrayShape(j)
          f(s1, s2)
          j += 1
        }
        i += 1
      }
    }
    def named(name: String): Arena = {
      mName = name
      this
    }
    
  }

  case class ShapePair(var shape1: GameShapes.Shape, var shape2: GameShapes.Shape) {
    def update(s1: GameShapes.Shape, s2: GameShapes.Shape) {
      if(s1.## > s2.##) {
        shape1 = s2
        shape2 = s1
      } else {
        shape1 = s1
        shape2 = s2        
      }
    }
    if(shape1.## > shape2.##) {
      val temp = shape1
      shape1 = shape2
      shape2 = temp
    }
  }
  
  /** Handles the collision between two shapes */
  def handleCollision(game: Game, s1: Shape, s2: Shape):Unit = {
    /**
     * Returns the speed of the objects after collision if any, along with the coordinates of the collision
     **/
    def sendCollisionEventIfAny(c1: Shape, x1: Float, y1: Float, r1: Float, m1: Float, c2: Shape, x2: Float, y2: Float, r2: Float, m2: Float): Boolean = {
      var result = false
      val dx = x2 - x1
      val dy = y2 - y1
      val distsquare = dx*dx + dy*dy
      val r1pr2 = r1+r2
      if(distsquare <= r1pr2 * r1pr2) { // There is a static collision.
        val length = Math.sqrt(distsquare).toFloat
        val nx = dx / length
        val ny=  dy / length
        /** Elasic shock if noVelocity */
        val vx1 = if(c1.noVelocity) c1.velocity_x - c2.velocity_x else c1.velocity_x
        val vy1 = if(c1.noVelocity) c1.velocity_y - c2.velocity_y else c1.velocity_y
        val vx2 = if(c2.noVelocity) c2.velocity_x - c1.velocity_x else c2.velocity_x
        val vy2 = if(c2.noVelocity) c2.velocity_y - c1.velocity_y else c2.velocity_y
        // Collision condition.
        val vx = vx2 - vx1
        val vy = vy2 - vy1
        if((vx1*nx + vy1*ny >= vx2*nx + vy2*ny)) { // The collision is increasing
          // Go back in time to see when this collision occured, and make it occured at this time.
          val delta = (r1pr2 * r1pr2) * (vx * vx + vy * vy) - (vx * dy - vy * dx) * (vx * dy - vy * dx)
          val a =  (vx * vx + vy * vy)
          val b =  (vx * dx + vy * dy)
          var t1 = if(a != 0 && delta > 0) {
            (- b - Math.sqrt(delta).toFloat)/a
            // The collision occured at time currentTime + t1
          } else {
            0f
          }

          val dm = (m1 - m2) / (m1 + m2)
          val rm2 = 2*m2 / (m1+m2)
          val rm1 = 2*m1 / (m1+m2)
          val nxy = nx * ny
          val nx2 = nx * nx
          val ny2 = ny * ny
  
          val nvx1 = if(c1.noVelocity) c1.velocity_x else dm*nx2*vx1 + dm*nxy*vy1 + nx2*rm2*vx2 + nxy*rm2*vy2 - nxy*vy1 + ny2*vx1
          val nvy1 = if(c1.noVelocity) c1.velocity_y else dm*nxy*vx1 + dm*ny2*vy1 + nx2*vy1 + nxy*rm2*vx2 - nxy*vx1 + ny2*rm2*vy2
          val nvx2 = if(c2.noVelocity) c2.velocity_x else -dm*nx2*vx2 - dm*nxy*vy2 + nx2*rm1*vx1 + nxy*rm1*vy1 - nxy*vy2 + ny2*vx2
          val nvy2 = if(c2.noVelocity) c2.velocity_y else -dm*nxy*vx2 - dm*ny2*vy2 + nx2*vy2 + nxy*rm1*vx1 - nxy*vx2 + ny2*rm1*vy1
          if(t1 > 0) {
            t1 = -t1
          }
          // Sets the new speeds to correct the collision, as well as the time at which the collision really occured.
          game.onCollisionEvent(c1, c2,
              nvx1, nvy1, nvx2, nvy2,
              t1,
              x1 + nx * r1, y1 + ny * r1,
              nx, ny) // Position of the collision, and unit vector of it.
          result = true
        }
      }
      result
    }
    (s1, s2) match {
      case (r1: Rectangle, r2: Rectangle) if !r1.noVelocity || !r2.noVelocity =>
        val rx1 = r1.x
        var ry1 = r1.y
        var rw1 = r1.width
        var rh1 = r1.height
        val rx2 = r2.x
        var ry2 = r2.y
        var rw2 = r2.width
        var rh2 = r2.height
        val x2inside1 = rx1 < rx2 && rx2 < rx1 + rw1
        val x1inside2 = rx2 < rx1 && rx1 < rx2 + rw2
        val y2inside1 = ry1 < ry2 && ry2 < ry1 + rh1
        val y1inside2 = ry2 < ry1 && ry1 < ry2 + rh2
        if(x2inside1 || x1inside2 ) {
          if(y2inside1 || y1inside2 ) {
            if(game.isNoCollisionBetween(s1, s2)) return
            // There is a collision
            if(x2inside1 && y2inside1) {
              val dx = Math.min(rw2, rx1 + rw1 - rx2)
              val dy = Math.min(rh2, ry1 + rh1 - ry2)
              if(dy < dx || dx == rw2) {
                sendCollisionEventIfAny(r1, rx2, ry1+rh1/2, rh1/2, if(r1.noVelocity) r2.mass else r1.mass,
                                        r2, rx2, ry2+rh2/2, rh2/2, if(r2.noVelocity) r1.mass else r2.mass)
              } else {
                sendCollisionEventIfAny(r1, rx1+rw1/2, ry2, rw1/2, if(r1.noVelocity) r2.mass else r1.mass,
                                        r2,  rx2+rw2/2, ry2, rw2/2, if(r2.noVelocity) r1.mass else r2.mass)
              }
            } else if(x2inside1 && y1inside2) {
              val dx = Math.min(rw2, rx1 + rw1 - rx2)
              val dy = Math.min(rh2, ry2 + rh2 - ry1)
              if(dx < dy || dy == rh2) {
                sendCollisionEventIfAny(r1, rx1+rw1/2, ry1, rw1/2, if(r1.noVelocity) r2.mass else r1.mass,
                                        r2, rx2+rw2/2, ry1, rw2/2, if(r2.noVelocity) r1.mass else r2.mass)
              } else {
                sendCollisionEventIfAny(r1, rx2, ry1 + rh1/2, rh1/2, if(r1.noVelocity) r2.mass else r1.mass,
                                        r2, rx2, ry2 + rh2/2, rh2/2, if(r2.noVelocity) r1.mass else r2.mass)
              }
            } else if(x1inside2 && y2inside1) {
              val dx = Math.min(rw2, rx2 + rw2 - rx1)
              val dy = Math.min(rh2, ry1 + rh1 - ry2)
              if(dx < dy || dy == rh2) {
                sendCollisionEventIfAny(r1, rx1+rw1/2, ry2, rw1/2, if(r1.noVelocity) r2.mass else r1.mass,
                                        r2, rx2+rw2/2, ry2, rw2/2, if(r2.noVelocity) r1.mass else r2.mass)
              } else {
                sendCollisionEventIfAny(r1, rx1, ry1 + rh1/2, rh1/2, if(r1.noVelocity) r2.mass else r1.mass,
                                        r2, rx1, ry2 + rh2/2, rh2/2, if(r2.noVelocity) r1.mass else r2.mass)
              }
            } else if(x1inside2 && y1inside2) {
              val dx = Math.min(rw2, rx2 + rw2 - rx1)
              val dy = Math.min(rh2, ry2 + rh2 - ry1)
              if(dy < dx || dx == rw2) {
                sendCollisionEventIfAny(r1, rx1, ry1+rh1/2, rh1/2,  if(r1.noVelocity) r2.mass else r1.mass,
                                        r2,  rx1, ry2+rh2/2, rh2/2, if(r2.noVelocity) r1.mass else r2.mass)
              } else {
                sendCollisionEventIfAny(r1, rx1+rw1/2, ry1, rw1/2,  if(r1.noVelocity) r2.mass else r1.mass,
                                        r2,  rx2+rw2/2, ry1, rw2/2, if(r2.noVelocity) r1.mass else r2.mass)
              }
            }
          }
        }
        
      case (r:Rectangle, c:Circle) =>
        val rx = r.x
        val ry = r.y
        val rw = r.width
        val rh = r.height
        val cx = c.x
        val cy = c.y
        val rad = c.radius
        //if(r.noVelocity && !c.noVelocity) {
          //val vx = c.velocity_x
          //val vy = c.velocity_y
          // Horizontal collision
        val circleInsideYrectangle = ry <= cy && cy <= ry + rh
        val circleInsideXrectangle = rx <= cx && cx <= rx + rw
        val circleWithinXBounds = rx + rw >= cx - rad && rx <= cx + rad
        val circleWithinYBounds = ry + rh >= cy - rad && ry <= cy + rad
        val horizontalCollision = circleInsideYrectangle && circleWithinXBounds
        val verticalCollision = circleInsideXrectangle && circleWithinYBounds
        if(horizontalCollision || verticalCollision) {
          if(horizontalCollision && (!verticalCollision || (Math.min(cx - rx, rx + rw - cx) < Math.min(cy - ry, ry + rw - cy)))) {
              if(!game.isNoCollisionBetween(s1, s2)) {
                // Replace the rectangle with a fictive ball at the position of the impact
                sendCollisionEventIfAny(c, cx, cy, rad, if(c.noVelocity) r.mass else c.mass,
                                        r,  rx + rw/2, cy, rw/2, if(r.noVelocity) c.mass else r.mass)
              // Collision either from right or from left if the speed is towards the collision
            }
          } else {  // Vertical collision
              if(!game.isNoCollisionBetween(s1, s2)) {
                // Replace the rectangle with a fictive ball at the position of the impact
                sendCollisionEventIfAny(c, cx, cy, rad, if(c.noVelocity) r.mass else c.mass,
                                        r, cx, ry + rh/2, rh/2, if(r.noVelocity) c.mass else r.mass)
              }
          }
        } else
          // corner collision
          { // Ball in the corners
            // Heuristic to be sure that the ball is close enough so that there can be a collision.
            if(cx <= rx + rw + rad && cx >= rx - rad && cy <= ry + rh + rad && cy >= ry - rad) {
              if(game.isNoCollisionBetween(s1, s2)) {return}
              // Computation of the point that is closest to the center of the ball.
              var cornerx = 0f
              var cornery = 0f
              if(rx + rw < cx) {
                if(ry + rh < cy) {
                  cornerx = rx + rw
                  cornery = ry + rh
                } else {
                  cornerx = rx + rw
                  cornery = ry
                }
              } else {
                if(ry + rh < cy) {
                  cornerx = rx
                  cornery = ry + rh
                } else {
                  cornerx = rx
                  cornery = ry
                }
              }
              sendCollisionEventIfAny(c, cx, cy, rad, if(c.noVelocity) r.mass else c.mass,
                                      r,  cornerx, cornery, 0f, if(r.noVelocity) c.mass else r.mass)
            }
          } 
        //}
        
      case (s1:Circle, s2:Rectangle) =>
        handleCollision(game, s2, s1)
      
      case (c1:Circle, c2:Circle) if !c1.noVelocity || !c2.noVelocity =>
        if(c1.x + c1.radius < c2.x - c2.radius || c1.x - c1.radius > c2.x + c2.radius ||
           c1.y + c1.radius < c2.y - c2.radius || c1.y - c1.radius > c2.y + c2.radius) return
        if(game.isNoCollisionBetween(s1, s2)) return
        sendCollisionEventIfAny(c1, c1.x, c1.y, c1.radius, if(c1.noVelocity) c2.mass else c1.mass,
                                c2, c2.x, c2.y, c2.radius, if(c2.noVelocity) c1.mass else c2.mass)
      case _ =>
    } 
  }
}