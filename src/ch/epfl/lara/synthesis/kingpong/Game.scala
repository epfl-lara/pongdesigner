package ch.epfl.lara.synthesis.kingpong;

import scala.util.Marshal
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.collection.mutable.LinkedList
import scala.collection.mutable.Stack
import scala.collection.Traversable
import android.view.MotionEvent
import android.graphics.Canvas
import android.view.SurfaceHolder
import android.os.Handler
import android.os.Vibrator
import android.view.SurfaceView
import android.content.Context
import android.graphics.Bitmap
import android.graphics.Rect
import android.graphics.RectF
import android.graphics.drawable.NinePatchDrawable
import android.graphics.Paint
import android.util.Log
import android.graphics.Matrix
import android.util.AttributeSet
import android.graphics.drawable.BitmapDrawable
import android.graphics.PorterDuffColorFilter
import android.graphics.PorterDuff
import android.graphics.PorterDuff.Mode
import android.graphics.drawable.Drawable
import android.os.Environment
import android.widget.Toast
import android.content.res.Resources
import java.io.IOException
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.util.Random


object Game {
  var randomgenerator = new Random()
}

/**
 * The game interface and methods
 */
abstract class Game /*extends scala.Serializable*/ {
  import TriggerEvent._
  import Game._
  //import GameShapes._
    
  /** Content of the game */
  var init_arenas = new ArrayBuffer[GameShapes.Arena]() // Arenas at the beginning
  var init_shapes = new ArrayBuffer[GameShapes.Shape]() // Shapes at the beginning
  var init_actions = new ArrayBuffer[Expression]() // In which order shapes are defined
  var init_rules = new ArrayBuffer[ReactiveRule]() // Rules at the beginning
  var context = new HashMap[String, Expression]()  // The context which binds a name to an expression.
  
  /** Recorded events */
  var inputEvents = new TriggerEventCollection
  var waitingEvents = new TriggerEventCollection
  var triggerEvents = new TriggerEventCollection
  var history_timestamp = new ParameterHistoryCollection[Long]()
  
  /** Current time of the game in ms */
  var currentTime = 0L
  setCurrentTime(0L)
  var maxTime = 0L
  var beginning: Boolean = true
  
  /** Arena coordinates */
  def screenWidth: Int
  def screenHeight: Int
  
  /** Arena bounding box */
  private var mMaxX = this.screenWidth.toFloat
  private var mMaxY = this.screenHeight.toFloat
  private var mMinX = 0f
  private var mMinY = 0f
  
  /** If the game is not running, all actions are currently played by some external simulation. */
  private var replayMode = false
  
  /** First-time true variable. */
  var initializePhase = true
  val self = this
  var lookupPair = GameShapes.ShapePair(null, null)
  /**
   * Rules stored in a compiled form
   *  - whenEverRules : If an event occurs after time change
   *  - whenFingerDownRules : When a finger is pressed
   *  - whenFingerUpRules : When a finger is released
   *  - whenFingerMovesOnRules : When a finger moves over a shape
   *  - whenIntegerChangesRules : when an integer expression changes, with the (oldValue, newValue) as an argument
   */
  //var whenEverRules = ArrayBuffer[(()=>Boolean, ()=>Unit)]()
  //var whenFingerDownRules = ArrayBuffer[(GameShapes.Shape, ()=>Unit)]()
  //var whenFingerUpRules = ArrayBuffer[(GameShapes.Shape, ()=>Unit)]()
  //var whenFingerMovesOnRules = ArrayBuffer[(GameShapes.Shape, (Float, Float, Float, Float)=>Unit)]()
  //var whenIntegerChangesRules = HashMap[GameShapes.IntegerBox, List[(Int, Int)=>Unit]]()
  //var whenCollisionRules = HashMap[GameShapes.ShapePair, List[()=>Unit]]()
  //var noCollisionBetweenRules = HashMap[GameShapes.ShapePair, Boolean]()

  /** All possibles rules */
  var added_whenEverRules = new ArrayBuffer[WhenEverRule]()
  var added_whenFingerDownOnRules = HashMap[GameShapes.Shape, WhenFingerDownOnRule]()
  var added_whenFingerUpOnRules = HashMap[GameShapes.Shape, WhenFingerUpOnRule]()
  var added_whenFingerMovesOnRules = HashMap[GameShapes.Shape, WhenFingerMovesOnRule]()
  var added_whenIntegerChangesRules = HashMap[GameShapes.IntegerBox, WhenIntegerChangesRule]()
  var added_whenCollisionRules = HashMap[GameShapes.ShapePair, WhenCollisionBetweenRule]()
  var added_noCollisionBetweenRules = HashMap[GameShapes.ShapePair, Boolean]()
  var added_noCollisionEffectBetweenRules = HashMap[GameShapes.ShapePair, Boolean]()
  
  object Camera extends GameShapes.Camera
  
  /** Arena setting and getting*/
  private var currentArena: GameShapes.Arena = null
  def setCurrentArena(a: GameShapes.Arena) = {
    currentArena = a
    init_shapes foreach (_.active = false)
    a foreach (_.active = true)
  }
  def getArena: GameShapes.Arena = {
    if(currentArena == null && !init_arenas.isEmpty) {
      setCurrentArena(init_arenas(0))
    }
    currentArena
  }
  
  /** The game engine that is used to display this game*/
  private var gameEngine: GameEngineView = null
  def setGameEngine(ge: GameEngineView) = {
    gameEngine = ge
    computeRegularBounds()
    initializeGame()
  }
  
  /** Initialization of the game */
  def initializeGame() = {
    context("screenWidth") = EConstant(screenWidth)
    context("screenHeight") = EConstant(screenHeight)
    init_shapes foreach {
      shape => shape.storeInitialState(false)
    }
  }
  
  /** Takes a snapshot of all game parameters */
  def snapShot() = {
    getArena foreach (_.snapShot())
  }
  /** Reverts the game state to the snapshot */
  def revertToSnapShot() = {
    getArena foreach (_.revertToSnapShot())
  }
  /** Verify that all the tests passed (not used at this point of development) */
  def verifyTests(): Boolean = {
    var verified = true
    getArena foreach{ a => verified = verified && (a.verifyTests())}
    verified
  }
  
  /** Coordinates of the game */
  def maxX = mMaxX
  def maxY = mMaxY
  def minX = mMinX
  def minY = mMinY
  
  /** Enters the game into an edit mode */
  def enterEditMode() = {
    mMinX = 0f
    mMinY = 0f
    mMaxX = screenWidth.toFloat
    mMaxY = screenHeight.toFloat
    var result = new RectF(0, 0, 0, 0)
    getArena foreach { shape =>
      shape.getBoundingBox(result)
      val x1 = result.left
      val y1 = result.top
      val x2 = result.right
      val y2 = result.bottom
      if(x1 < mMinX) mMinX = x1
      if(y1 < mMinY) mMinY = y1
      if(x2 > mMaxX) mMaxX = x2
      if(y2 > mMaxY) mMaxY = y2
      shape.storePrevValues()
    }
  }

  /** Exits the game edit mode */
  def exitEditMode() = {
    computeRegularBounds()
    triggerEvents.keepOnlyValuesBeforeAndIncluding(currentTime)
    inputEvents.keepOnlyValuesBeforeAndIncluding(currentTime)
    history_timestamp.keepOnlyValuesBeforeAndIncluding(currentTime)
    //Log.d("Game.scala", "beginning=" + beginning)
    getArena foreach { shape =>
      //Log.d("Game.scala", "beginning=" + beginning)
      shape.storeInitialState(beginning)
      if(beginning) {
        shape match {
          case r:GameShapes.Rectangular => // TODO Apply optimizations to the code (like comparing to screenWidth, etc.)
            
          case _ =>
        }
      }
      shape.storeState(currentTime)
      shape.keepOnlyValuesBeforeAndIncluding(currentTime)
    }
    //Log.d("Game.scala", "beginning=" + beginning)
    beginning = false
  }
  
  /** Stores the initial state */
  def storeInitialState(beginning: Boolean) = {
    init_shapes foreach { shape => shape.storeInitialState(beginning) }
  }

  /** Compute regular bounds */
  def computeRegularBounds() = {
    /*mMinX = 0f
    mMinY = 0f
    mMaxX = screenWidth.toFloat
    mMaxY = screenHeight.toFloat*/
    mMinX = Camera.x
    mMinY = Camera.y
    mMaxX = Camera.x+Camera.width.toFloat
    mMaxY = Camera.y+Camera.height.toFloat
  }

  /** Pseudo-macro to add a WhenEver rule into the game */
  def WhenEver(f1: =>Boolean)(f2: =>Unit):ReactiveRule = {
    //whenEverRules += (()=>f1, ()=>f2)
    val new_rule = WhenEverRule(CompiledBoolean(() => f1), List(CompiledBlock(() => f2)))
    added_whenEverRules += new_rule
    init_rules += new_rule
    new_rule
  }
  
  /** Pseudo-macro to add a WhenFingerDownOn rule into the game */
  def WhenFingerDownOn(f1: GameShapes.Shape)(f2: =>Unit):ReactiveRule = {
    added_whenFingerDownOnRules.getOrElse(f1, null) match {
      case w@WhenFingerDownOnRule(EIdentShape(f1), code) =>
        w.code = w.code ++ List(CompiledBlock(() => f2))
        w
      case _ =>
        val new_rule = WhenFingerDownOnRule(EIdentShape(f1), List(CompiledBlock(() => f2)))
        added_whenFingerDownOnRules(f1) = new_rule
        init_rules += new_rule
        new_rule
    }
  }
  
  /** Pseudo-macro to add a WhenFingerUpOn rule into the game */
  def WhenFingerUpOn(f1: GameShapes.Shape)(f2: =>Unit) = {
    added_whenFingerUpOnRules.getOrElse(f1, null) match {
      case w@WhenFingerUpOnRule(EIdentShape(f1), code) =>
        w.code = w.code ++ List(CompiledBlock(() => f2))
        w
      case _ =>
        val new_rule = WhenFingerUpOnRule(EIdentShape(f1), List(CompiledBlock(() => f2)))
        added_whenFingerUpOnRules(f1) = new_rule
        init_rules += new_rule
        new_rule
    }
  }
  
  /** Pseudo-macro to add a WhenFingerMovesOn rule into the game */
  def WhenFingerMovesOn(f1: GameShapes.Shape)(f2: (Float, Float, Float, Float) => Unit) = {
    added_whenFingerMovesOnRules.getOrElse(f1, null) match {
      case w@WhenFingerMovesOnRule(_, List(a, b, c, d), code) => 
        w.code = w.code ++ List(CompiledBlock(() =>
          f2(a.evaluate(context).number_value,
             b.evaluate(context).number_value,
             c.evaluate(context).number_value,
             d.evaluate(context).number_value)))
        w
      case _ =>
        val a = EIdent("xFrom")
        val b = EIdent("yFrom")
        val c = EIdent("xTo")
        val d = EIdent("yTo")
        val new_rule = WhenFingerMovesOnRule(EIdentShape(f1), List(a, b, c, d),
            List(CompiledBlock(() =>
            f2(a.evaluate(context).number_value,
               b.evaluate(context).number_value,
               c.evaluate(context).number_value,
               d.evaluate(context).number_value)))
        )
        added_whenFingerMovesOnRules(f1) = new_rule
        init_rules += new_rule
        new_rule
    }
  }
  
  /** Pseudo-macro to add a WhenIntegerChanges rule into the game */
  def WhenIntegerChanges(f1: GameShapes.IntegerBox)(f2: (Int, Int)=> Unit):ReactiveRule = {
    added_whenIntegerChangesRules.getOrElse(f1, null) match {
      case w@WhenIntegerChangesRule(_, List(a, b), code) => 
        w.code = w.code ++ List(CompiledBlock(() =>
          f2(a.evaluate(context).number_value.toInt,
             b.evaluate(context).number_value.toInt)))
        w
      case _ =>
        val a = EIdent("oldValue")
        val b = EIdent("newValue")
        val new_rule = WhenIntegerChangesRule(EIdentShape(f1), List(a, b),
            List(CompiledBlock(() =>
            f2(a.evaluate(context).number_value.toInt,
               b.evaluate(context).number_value.toInt)))
        )
        added_whenIntegerChangesRules(f1) = new_rule
        init_rules += new_rule
        new_rule
    }
  }
  
  /** Pseudo-macro to add a WhenCollisionBetween rule into the game */
  def WhenCollisionBetween(f1: GameShapes.Shape, f2: GameShapes.Shape)(f3: =>Unit) = {
    lookupPair.update(f1, f2)
    added_whenCollisionRules.getOrElse(lookupPair, null) match {
      case w@WhenCollisionBetweenRule(EIdentShape(s1), EIdentShape(s2), code) =>
        w.code = w.code ++ List(CompiledBlock(() => f3))
        w
      case _ => 
        val new_rule = WhenCollisionBetweenRule(EIdentShape(f1), EIdentShape(f2), List(CompiledBlock(() => f3)))
        added_whenCollisionRules(GameShapes.ShapePair(f1, f2)) = new_rule
        init_rules += new_rule
        new_rule
    }
  }
  
  /** Pseudo-macro to add a WhenCollisionBetween rule into the game */
  def WhenCollisionBetween[U <: GameShapes.Shape, T <: GameShapes.Shape](f3: (U, T) => Unit) = {
  }
  
  /** Pseudo-macro to add a NoCollisionBetween rule into the game */
  def NoCollisionBetween(f1: GameShapes.Shape, f2: GameShapes.Shape) = {
    added_noCollisionBetweenRules(GameShapes.ShapePair(f1, f2)) = true
  }
  
  /** Pseudo-macro to add a NoCollisionEffectBetween rule into the game */
  def NoCollisionEffectBetween(f1: GameShapes.Shape, f2: GameShapes.Shape) = {
    added_noCollisionEffectBetweenRules(GameShapes.ShapePair(f1, f2)) = true
  }
  
  def setCurrentTime(time: Long) = {
    currentTime = time
    //Game.randomgenerator.setSeed(currentTime)
  }
  
  /** Moves the simulation time to the new time */
  def advanceTimeTo(newTime: Long, ruleToStopBefore: ReactiveRule = null): Boolean = {
    initializePhase = false
    val deltaTime = newTime - currentTime
    setCurrentTime(newTime)
    if(maxTime < newTime) maxTime = newTime
    if(!replayMode) {
      history_timestamp.addOrReplaceValue(newTime, newTime)
      inputEvents.addEvent(newTime, NEW_TIMESTAMP_EVENT, null, null, 0, 0, 0, 0)
    }
    val shapes = getArena
    
    shapes foreach { s =>
      s.moveDuring(deltaTime, currentTime)
      //if(s.noVelocity) s.resetVelocityIfOutdated(currentTime)
    }
    // Here we update the velocity
    shapes foreachVisiblePair { (s1, s2) =>
      GameShapes.handleCollision(this, s1, s2)
    }
    var continue = true
    added_whenEverRules foreach {
      case r@WhenEverRule(condExpr, codeList) =>
        if(r == ruleToStopBefore) continue = false
        if(Expression.evaluateExpression(context, condExpr).boolean_value && continue) {
          Expression.execute(codeList, context)
        }
    }
    inputEvents.removeTooOldValues(currentTime - lengthStoredEvent)
    history_timestamp.removeTooOldValues(currentTime - lengthStoredEvent-1000)
    triggerEvents.removeTooOldValues(currentTime - lengthStoredEvent)
    continue = storeState(newTime, continue, ruleToStopBefore)
    //if(replayMode) {
    shapes foreach { s => s.storePrevValues() }
    // We keep a copy of all the values at the current point.
    // All rules
    //}
    GameShapes.AccelerometerGravity.foreach {
      shape =>
        shape.velocity *= 0.98f
    }
    continue
  }
  
  /** Stores the state of the entire game */
  def storeState(newTime: Long, c: Boolean = true, ruleToStopBefore: ReactiveRule = null): Boolean = {
    var continue = c
    getArena foreach { s =>
      s.storeState(newTime)
      if(continue) continue = s.raiseTriggers(currentTime, ruleToStopBefore)
    }
    continue
  }

  /** Resets the simulation */
  def reset() = {
    setCurrentTime(0L)
    beginning = true
    maxTime = 0L
    init_shapes foreach {
      shape => shape.reset(context)
               shape.storePrevValues()
    }
  }
  
  /** Copy the current values to the previous values for all shapes. */
  def storePrevValues(except: GameShapes.Shape = null) = {
    init_shapes foreach {
      shape => if(shape != except) shape.storePrevValues()
    }
  }
  
  /** Restores the previous values for all shapes. */
  def restorePrev(except: GameShapes.Shape = null) = {
    init_shapes foreach {
      shape => if(shape != except) shape.restorePrev()
    }
  }
  
  /**
   * Informs the collision detector if the collision should not happen.
   **/
  def isNoCollisionBetween(s1: GameShapes.Shape, s2: GameShapes.Shape) : Boolean = {
    var result = true
    if(s1.noVelocity && s2.noVelocity) {
      result = !s1.movedAtTime(currentTime) && !s2.movedAtTime(currentTime)
    } else if(added_noCollisionBetweenRules.size != 0) {
      lookupPair.update(s1, s2)
      result =  added_noCollisionBetweenRules.getOrElse(lookupPair, false)
    } else {
      result = false
    }
    result
  }
  
  /**
   * Informs the collision detector if there should be no collision effect between two shapes.
   */
  def isNoCollisionEffectBetween(s1: GameShapes.Shape, s2: GameShapes.Shape): Boolean = {
    lookupPair.update(s1, s2)
    added_noCollisionEffectBetweenRules.getOrElse(lookupPair, false)
  }
  
  /** Invokes the action if the shape is selectable by the coordinates */
  def processEventsIfShapeSelectableBy(s: GameShapes.Shape, x: Float, y: Float, action: () => Unit): Boolean = {
    if(s.active && s.visible && s.selectableBy(x, y)) {
      action()
      true
    } else false
  }
 
  /** Invokes the action if the shape is selectable by the coordinates */
  def processMoveAction(s: GameShapes.Shape, xFrom: Float, yFrom: Float, xTo: Float, yTo: Float, action: (Float, Float, Float, Float) => Unit) = {
    if(s.active && s.visible && s.selectableBy (xFrom, yFrom)) {
      action(xFrom, yFrom, xTo, yTo)
      true
    } else false
  }
  
  /** Finger events */
  def onFingerDownEvent(x: Float, y: Float, ruleToStopBefore: ReactiveRule = null):Boolean = {
    if(!replayMode) inputEvents.addEvent(currentTime, TOUCHDOWN_EVENT, null, null, x, y, 0, 0)
    var continue = true
    val result = added_whenFingerDownOnRules.foldLeft(false){ case (found, (shape, rule)) =>
      processEventsIfShapeSelectableBy(shape, x, y, {() => if(rule == ruleToStopBefore) continue = false; if(continue) Expression.execute(rule.code, context)}) || found
    }
    if(replayMode) continue else result
  }
  def onFingerUpEvent(x: Float, y: Float, ruleToStopBefore: ReactiveRule = null):Boolean = {
    if(!replayMode) inputEvents.addEvent(currentTime, TOUCHUP_EVENT, null, null, x, y, 0, 0)
    var continue = true
    val result = added_whenFingerUpOnRules.foldLeft(false){ case (found, (shape, rule)) =>
      processEventsIfShapeSelectableBy(shape, x, y, {() =>  if(rule == ruleToStopBefore) continue = false; if(continue) Expression.execute(rule.code, context)}) || found
    }
    if(replayMode) continue else result
  }
  
  /** Updates the context of the coordinates from a finger event */
  def updateContextMoveCoordinates(xFrom: Float, yFrom: Float, xTo: Float, yTo: Float) = {
    context.getOrElseUpdate("xFrom", EConstant(0)).asInstanceOf[ModifiableValue].setValue(xFrom)
    context.getOrElseUpdate("yFrom", EConstant(0)).asInstanceOf[ModifiableValue].setValue(yFrom)
    context.getOrElseUpdate("xTo", EConstant(0)).asInstanceOf[ModifiableValue].setValue(xTo)
    context.getOrElseUpdate("yTo", EConstant(0)).asInstanceOf[ModifiableValue].setValue(yTo)   
  }
  
  /** Finger move events  */
  def onFingerMoveEvent(xFrom: Float, yFrom: Float, xTo: Float, yTo: Float, ruleToStopBefore: ReactiveRule = null): Boolean = {
    if(!replayMode) inputEvents.addEvent(currentTime, TOUCHMOVE_EVENT, null, null, xFrom, yFrom, xTo, yTo)
    updateContextMoveCoordinates(xFrom, yFrom, xTo, yTo)
    var continue = true
    val result = added_whenFingerMovesOnRules.foldLeft(false){ case (found, (shape, rule)) => processMoveAction(shape, xFrom, yFrom, xTo, yTo,
        {(xFrom, yFrom, xTo, yTo) => if(rule == ruleToStopBefore) continue = false; if(continue) Expression.execute(rule.code, context)}) || found}
    if(replayMode) continue else result
  }

  /**
   * Returns true if the ruleToStopBefore has not been reached
   */
  def onCollisionEvent(s1: GameShapes.Shape, s2: GameShapes.Shape,
      new_velocityXs1: Float, new_velocityYs1: Float, new_velocityXs2: Float, new_velocityYs2:Float,
      time_collision_relative: Float,
      x: Float, y: Float, ruleToStopBefore: ReactiveRule = null): Boolean = {
    triggerEvents.addEvent(currentTime, COLLISION_EVENT, s1, s2, x, y, s1.x, s1.y)
    if(!isNoCollisionEffectBetween(s1, s2)) {
      if(s1.velocity_x != new_velocityXs1 || s1.velocity_y != new_velocityYs1) {
        if(time_collision_relative != 0) {
          s1.x = s1.x + time_collision_relative * s1.velocity_x
          s1.y = s1.y + time_collision_relative * s1.velocity_y
        }
        s1.velocity_x = new_velocityXs1
        s1.velocity_y = new_velocityYs1
        if(s1.gravity == GameShapes.AccelerometerGravity || s1.gravity == GameShapes.Gravity2D) {
          s1.friction(s1) 
        }
        if(time_collision_relative != 0) {
          s1.x = s1.x - time_collision_relative * s1.velocity_x
          s1.y = s1.y - time_collision_relative * s1.velocity_y
        }
      }
      if(s2.velocity_x != new_velocityXs2 || s2.velocity_y != new_velocityYs2) {
        if(time_collision_relative != 0) {
          s2.x = s2.x + time_collision_relative * s2.velocity_x
          s2.y = s2.y + time_collision_relative * s2.velocity_y
        }
        s2.velocity_x = new_velocityXs2
        s2.velocity_y = new_velocityYs2
        if(s2.gravity == GameShapes.AccelerometerGravity || s1.gravity == GameShapes.Gravity2D) {
          s2.friction(s2) 
        }
        if(time_collision_relative != 0) {
          s2.x = s2.x - time_collision_relative * s2.velocity_x
          s2.y = s2.y - time_collision_relative * s2.velocity_y
        }
      }
    } else {
      s1.velocity_x *= 1
    }
    // Apply collision rules
    lookupPair.update(s1, s2)
    added_whenCollisionRules.get(lookupPair) match {
      case Some(w) => if(w == ruleToStopBefore) false else { Expression.execute(w.code, context); true}
      case _ => true
    }
  }
  
  /** Update values stored in context concerning value changes */
  def updateContextValueChanged(oldValue: Int, newValue: Int) = {
    context.getOrElseUpdate("oldValue", EConstant(0)).asInstanceOf[ModifiableValue].setValue(oldValue)
    context.getOrElseUpdate("newValue", EConstant(0)).asInstanceOf[ModifiableValue].setValue(newValue)
  }
  
  /**
   * Returns true if the ruleToStopBefore has not been encountered
   */
  def onIntegerChangeEvent(s1: GameShapes.IntegerBox, from: Int, to: Int, ruleToStopBefore: ReactiveRule = null): Boolean = {
    triggerEvents.addEvent(currentTime, INTEGER_CHANGE_EVENT, s1, null, s1.x, s1.y, from, to)
    added_whenIntegerChangesRules.getOrElse(s1, null) match {
      case null =>
        true
      case rule =>
        if(rule == ruleToStopBefore) {
          false
        } else {
          //Log.d("IntegerChange", "Event activated : changed from " + from + " to " + to)
          updateContextValueChanged(from, to)
          Expression.execute(rule.code, context)
          true
        }
    }
  }
  
  /** A custom class to be able to create a file */
  class StringAdder(var output: StringBuffer) {
    def addline (ls: String*): StringAdder = {endline; ls foreach { s => this.apply(s) }; this}
    def apply(s: String): StringAdder = {output append s; this}
    def +(s: String): StringAdder = this.apply(s)
    def add(s: String): StringAdder = this.apply(s)
    def addonly(s: String): StringAdder = this.apply(s)
    def endline: StringAdder = {output append "\n"; this}
  }
  
  /** Returns a valid scala string representation of itself */
  def outputItself(): StringBuffer = {
    val output = new StringAdder(new StringBuffer())
    output add     "package ch.epfl.lara.synthesis.kingpong;"
    output addline ""
    output addline "class PongGame extends Game {"
    output addline "  /**"
    output addline "   * Game static values"
    output addline "   */"
    output addline("  var screenWidth = " + screenWidth)
    output addline("  var screenHeight = " + screenHeight)
    output addline ""
    output addline "  /**"
    output addline "   * Game Layouts"
    output addline "   */"
    init_actions foreach {
      case ValDefCode(s, e) =>
      context.getOrElse(s.name, s) match {
        case EIdentArena(arena) => 
    output addline "  val " + arena.mName + " = Arena()" + " named \"" + arena.mName + "\""
        case EIdentShape(shape) =>
          shape match {
          case t: GameShapes.Rectangle =>
            if(t.initializedFromBounds) {
    output addline "  val " + t.mName + " = Rectangle.fromBounds(" + t.start_x.toScalaString("", context) + ", " + t.start_y.toScalaString("", context) + ", " + t.start_x2.toScalaString("", context) + ", " + t.start_y2.toScalaString("", context) + ")" + " named \"" + t.mName + "\""
            } else {
    output addline "  val " + t.mName + " = Rectangle(" + t.start_x.toScalaString("", context) + ", " + t.start_y.toScalaString("", context) + ", " + t.start_width.toScalaString("", context) + ", " + t.start_height.toScalaString("", context) + ")" + " named \"" + t.mName + "\""
            }
          case t: GameShapes.Circle =>
    output addline "  val " + t.mName + " = Circle(" + t.start_x.toScalaString("", context) + ", " + t.start_y.toScalaString("", context) + ", " + t.start_radius.toScalaString("", context) + ")" + " named \"" + t.mName + "\""
          case t: GameShapes.IntegerBox =>
    output addline "  val " + t.mName + " = IntegerBox(" + t.start_x.toScalaString("", context) + ", " + t.start_y.toScalaString("", context) + ", " + t.start_width.toScalaString("", context) + ", " + t.start_height.toScalaString("", context) + ", " + t.start_value.toScalaString("", context) +")" + " named \"" + t.mName + "\""
          case t: GameShapes.TextBox =>
    output addline "  val " + t.mName + " = TextBox(" + t.start_x.toScalaString("", context) + ", " + t.start_y.toScalaString("", context) + ", " + t.start_width.toScalaString("", context) + ", " + t.start_height.toScalaString("", context) + ", " + t.start_text.toScalaString("", context) +")" + " named \"" + t.mName + "\""
          case t => t.toString()
        }
        // Now we return all speed initialization, color, etc, etc.
        if(shape.noVelocity == false) {
          if(null != shape.start_velocity_x && shape.start_velocity_x.number_value != 0) {
      output addline "  " + shape.mName + ".velocity_x = " + shape.start_velocity_x.toScalaString("", context)
          }
          if(null != shape.start_velocity_y && shape.start_velocity_y.number_value != 0) {
      output addline "  " + shape.mName + ".velocity_y = " + shape.start_velocity_y.toScalaString("", context)
          }
        } else {
      output addline "  " + shape.mName + ".noVelocity = true"
        }
        if(null != shape.start_color && shape.start_color.number_value != GameShapes.DEFAULT_COLOR) {
    output addline "  " + shape.mName + ".color = " + shape.start_color.toScalaString("", context)
        }
        if(null != shape.start_visible && shape.start_visible.boolean_value != true) {
    output addline "  " + shape.mName + ".visible = " + shape.start_visible.toScalaString("", context)
        }
        case EIdent(name) =>
    output addline "  val " + name + " = " + e.toScalaString("", context)
        case _ =>
    output addline "  val " + s.name + " = " + e.toScalaString("", context) + "// Careful: not recognized"

    }
      case t =>
    output addline t.toScalaString("  ", context)
    }
    output addline "  "
    init_rules foreach { rule =>
      output addline rule.toCompleteScalaString("  ", context)
    }
    output addline "}"
    
    var fos:FileWriter = null
  
    try{
     val root: File = Environment.getExternalStorageDirectory();
  
     if( root.canWrite() ){
       fos = new FileWriter( root + "/" + "PongGameSelfAwareReprogrammed.scala" );
       fos.write(output.output.toString)
       fos.close()
     }else{
      //result = "file cant write";
     }
    }catch{
     case e:IOException =>
      e.printStackTrace()
      //result = "file not written";
    }
    
    output.output
  }
  
  import Expression._
  
  /** Pseudo-macro to build an Arena */
  def Arena(): GameShapes.Arena = {
    val arena = GameShapes.Arena.build()
    arena.attach(this)
    init_arenas += arena
    init_actions += ValDefCode(EIdentArena(arena), EApply(ESelect(ETop(), "Arena"), List()))
    arena
  }
  
  /** Pseudo-macro to build a rectangle from bounds */
  object Rectangle {
    def fromBounds(x: Float, y: Float, x2: Float, y2:Float):GameShapes.Rectangle = {
      val shape = GameShapes.Rectangle.buildFromBounds(x, y, x2, y2)
      init_shapes += shape
      val ident_shape = EIdentShape(shape)
      init_actions += ValDefCode(EIdentShape(shape), EApply(ESelect(ETop(), "Rectangle"), List()))
      shape.attach(self)
      shape
    }
  }
  
  /** Pseudo-macro to build a rectangle */
  def Rectangle(x: Float, y: Float, width: Int, height: Int): GameShapes.Rectangle = {
    val shape = GameShapes.Rectangle.build(x, y, width, height)
    init_shapes += shape
    val ident_shape = EIdentShape(shape)
    init_actions += ValDefCode(ident_shape, EApply(ESelect(ETop(), "Rectangle"), List()))
    shape.attach(this)
    shape
  }
  
  /** Pseudo-macro to build a circle */
  def Circle(x: Float, y: Float, radius: Float): GameShapes.Circle = {
    val shape = GameShapes.Circle.build(x, y, radius)
    init_shapes += shape
    val ident_shape = EIdentShape(shape)
    init_actions += ValDefCode(ident_shape, EApply(ESelect(ETop(), "Circle"), List()))
    shape.attach(this)
    shape
  }
  
  /** Pseudo-macro to build an IntegerBox */
  def IntegerBox(x: Float, y: Float, width: Int, height:Int, value:Int): GameShapes.IntegerBox = {
    val shape = GameShapes.IntegerBox.build(x, y, width, height, value)
    init_shapes += shape
    val ident_shape = EIdentShape(shape)
    init_actions += ValDefCode(ident_shape, EApply(ESelect(ETop(), "IntegerBox"), List()))
    shape.attach(this)
    shape
  }
  
  /** Pseudo-macro to build a TextBox */
  def TextBox(x: Float, y: Float, width: Int, height:Int, text:String): GameShapes.TextBox = {
    val shape = GameShapes.TextBox.build(x, y, width, height, text)
    init_shapes += shape
    val ident_shape = EIdentShape(shape)
    init_actions += ValDefCode(ident_shape, EApply(ESelect(ETop(), "TextBox"), List()))
    shape.attach(this)
    shape
  }
  def randomInterval(x: Float, y: Float): Float = {
    Game.randomgenerator.nextFloat()*(y-x) + x
  }
  def randomInterval(x: Int, y: Int): Int = {
    randomInterval(x.toFloat, y.toFloat).toInt
  }
  def random[T](v: List[T]): T = {
    v(Game.randomgenerator.nextInt(v.size))
  }
  def random[T](vargs: T*): T = {
    random(vargs.toList)
  }
  
  /**
   * Function used by the slider<
   * If we go back in time, we just get the shapes from the history.
   * If we go to the future, we apply all the timestamps and we replay the scene.
   */
  def returnToTime(timestamp: Long, ruleToStopBefore: ReactiveRule = null): Unit = {
    if(timestamp < currentTime) {
      val delta = if(ruleToStopBefore != null) 1 else 0
      val time_previous = history_timestamp(timestamp - delta, currentTime)
      setCurrentTime(time_previous)
      for(shape <- init_shapes) {
        shape.returnToTime(time_previous)
      }
      triggerEvents.keepOnlyValuesBeforeAndIncluding(timestamp)
      Log.d("Game.scala", "Going back to time " + currentTime)
      if(delta == 1) returnToTime(timestamp, ruleToStopBefore) // Going forward to reexecute the rules
    } else if(timestamp > currentTime) {
      replayMode = true
      var continue = true
      // Events at currentTime are not executed
      inputEvents.foreachBetween(currentTime, timestamp){ i => if(continue) {
        i match {
          case i if i.value.code == TOUCHDOWN_EVENT => 
            continue = onFingerDownEvent(i.value.x1, i.value.y1, ruleToStopBefore)
          case i if i.value.code == TOUCHMOVE_EVENT => 
            continue = onFingerMoveEvent(i.value.x1, i.value.y1, i.value.x2, i.value.y2, ruleToStopBefore)
          case i if i.value.code == TOUCHUP_EVENT => 
            continue = onFingerUpEvent(i.value.x1, i.value.y1, ruleToStopBefore)
          case i if i.value.code == NEW_TIMESTAMP_EVENT => 
            continue = advanceTimeTo(i.timestamp, ruleToStopBefore)
            Log.d("Game.scala", "Going forward to time " + i.timestamp)
          case _ =>
        }
      }}
      replayMode = false
      //setCurrentTime(history_timestamp(timestamp))
    }
  }
  
  /** Records the shape addition to the arena as custom code */
  def recordShapeAdditionToArena(a: GameShapes.Arena, s: GameShapes.Shape) = {
    init_actions += EApply(ESelect(EIdentArena(a), "$plus$eq"), List(EIdentShape(s)))
    if(s.mName == null || s.mName =="") {
      val new_name = GameShapes.getBrandNewName(null, context)
      s.mName = new_name
    }
    if(a.mName == null || a.mName =="") {
      val new_name = GameShapes.getBrandNewName(null, context)
      a.mName = new_name
      context(a.mName) = EIdentArena(a)
    }
    context(s.mName) = EIdentShape(s)
  }
  
  /** Removes an element from an array buffer */
  def removeFromArrayBuffer[T](a: ArrayBuffer[T], f: T => Boolean) = {
    var i = 0
    while(i < a.length) {
      val elem = a(i)
      if(f(elem)) {
        a.remove(i)
      } else {
        i += 1
      }
    }
  }
  
  /** Accepts a rule and inserts it into the game. */
  def insertRule(waiting_rule: ReactiveRule, causeEventTime: Long): Unit = {
    if(waiting_rule != null) {
      waiting_rule match {
        case w@WhenCollisionBetweenRule(EIdentShape(shape1), EIdentShape(shape2), code) =>
          val lookupPair = GameShapes.ShapePair(shape1, shape2)
          if(!added_whenCollisionRules.contains(lookupPair)) {
            init_rules += waiting_rule
          }
          added_whenCollisionRules(GameShapes.ShapePair(shape1, shape2)) = w
        case w@WhenFingerMovesOnRule(EIdentShape(shape1), listcoords, code) =>
          if(!added_whenFingerMovesOnRules.contains(shape1)) {
            init_rules += waiting_rule
          }
          added_whenFingerMovesOnRules(shape1) = w
        case w@WhenEverRule(condition, code) => 
          added_whenEverRules += w
          // TODO : index by condition
          added_whenEverRules foreach {
            condition =>
              init_rules += waiting_rule
          }
        case w@WhenFingerDownOnRule(EIdentShape(shape), code) =>
          if(!added_whenFingerDownOnRules.contains(shape)) {
            init_rules += waiting_rule
          }
          added_whenFingerDownOnRules(shape) = w
        case w@WhenFingerUpOnRule(EIdentShape(shape), code) =>
          if(!added_whenFingerUpOnRules.contains(shape)) {
            init_rules += waiting_rule
          }
          added_whenFingerUpOnRules(shape) = w
        case w@WhenIntegerChangesRule(EIdentShape(shape1:GameShapes.IntegerBox), listcoords, code) =>
          if(!added_whenIntegerChangesRules.contains(shape1)) {
            init_rules += waiting_rule
          }
          added_whenIntegerChangesRules(shape1) = w
        case w@NoCollisionBetweenRule(EIdentShape(shape1), EIdentShape(shape2)) =>
          val lookupPair = GameShapes.ShapePair(shape1, shape2)
          if(!added_noCollisionBetweenRules.contains(lookupPair)) {
            init_rules += waiting_rule
          }
          added_noCollisionBetweenRules(GameShapes.ShapePair(shape1, shape2)) = true
        case w@NoCollisionEffectBetweenRule(EIdentShape(shape1), EIdentShape(shape2)) =>
          val lookupPair = GameShapes.ShapePair(shape1, shape2)
          if(!added_noCollisionEffectBetweenRules.contains(lookupPair)) {
            init_rules += waiting_rule
          }
          added_noCollisionEffectBetweenRules(GameShapes.ShapePair(shape1, shape2)) = true
        case _ =>
      }
      if(causeEventTime <= currentTime) {
        val saveCurrentTime = currentTime
        returnToTime(causeEventTime-1)
        returnToTime(saveCurrentTime)
      }
    }
  }
  
  /** Deletes the selected shape
   *  Deletes code portions modifying the shape
   * */
  def deleteShape(shapeToDelete: GameShapes.Shape) =  {
    import GameShapes._
    if(shapeToDelete != null) {
      getArena -= shapeToDelete
      init_shapes -= shapeToDelete
      context.remove(shapeToDelete.mName)
      removeFromArrayBuffer(init_actions, { action:Expression => null == action || action.contains(shapeToDelete)})
      removeFromArrayBuffer(init_rules, {
        rule:ReactiveRule => if(rule != null && rule.contains(shapeToDelete)) {
          rule match {
            case w@WhenCollisionBetweenRule(EIdentShape(shape1), EIdentShape(shape2), code) =>
              val lookupPair = GameShapes.ShapePair(shape1, shape2)
              added_whenCollisionRules.remove(lookupPair)
            case w@WhenFingerMovesOnRule(s, listcoords, code) =>
              added_whenFingerMovesOnRules foreach {
                case r@(shape1, w) => if(shapeToDelete == shape1) {
                  added_whenFingerMovesOnRules.remove(shape1)
                } else if(w.contains(shapeToDelete)) {
                  w.removeShapeFromCode(shapeToDelete)
                }}
            case w@WhenEverRule(condition, code) => 
              added_whenEverRules foreach { case w => if(w contains shapeToDelete) added_whenEverRules -= w }
            case w@WhenFingerDownOnRule(s, code) =>
              added_whenFingerDownOnRules foreach { case r@(shape1, w) => if(w.contains(shapeToDelete) || shapeToDelete == shape1) added_whenFingerDownOnRules.remove(shape1) }
            case w@WhenFingerUpOnRule(s, code) =>
              added_whenFingerUpOnRules foreach { case r@(shape1, w) => if(w.contains(shapeToDelete) || shapeToDelete == shape1) added_whenFingerUpOnRules.remove(shape1) }
            case w@WhenIntegerChangesRule(EIdentShape(d: IntegerBox), listcoords, code) =>
              val rule = added_whenIntegerChangesRules(d)
              added_whenIntegerChangesRules foreach { case r@(shape1, w) => if(w.contains(shapeToDelete) || shapeToDelete == shape1) added_whenIntegerChangesRules -= d }
            case w@NoCollisionBetweenRule(EIdentShape(shape1), EIdentShape(shape2)) =>
              val lookupPair = GameShapes.ShapePair(shape1, shape2)
              added_noCollisionBetweenRules.remove(lookupPair)
            case _ => Log.d("Game.scala", "Unrecognized rule where to delete " + shapeToDelete + " : " + rule)
          }
          true
        } else {
          false
        }
      })
    }
  }
  
  def set2DAcceleration(x: Float, y: Float): Unit = {
    GameShapes.AccelerometerGravity.force_x = -x/1000
    GameShapes.AccelerometerGravity.force_y = y/1000
  }
}

