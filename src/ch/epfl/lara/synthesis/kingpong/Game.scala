package ch.epfl.lara.synthesis.kingpong

import scala.collection.mutable.ArrayBuffer
import org.jbox2d.collision.shapes.CircleShape
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.rules.Context
import ch.epfl.lara.synthesis.kingpong.rules.Events._
import ch.epfl.lara.synthesis.kingpong.common.History
import scala.collection.mutable.DoubleLinkedList
import scala.collection.mutable.Undoable
import scala.collection.mutable.Undoable
import common._

trait Game extends RulesManager with Context { self => 
  protected implicit val selfImplicit = self
  protected implicit val startIsPlanned = GameObject.PLANNED_SINCE_BEGINNING
  
  val world: PhysicalWorld
  var worldgravityangle = 0.0f
  var worldgravityradius = 0.0f
  var worldgravityenabled = true
  
  // If the time is to be restored at the next step
  var scheduledRestoreTime: Option[Int] = None
  
  var pixelsByUnit = 0f

  private[kingpong] val eventsHistory = new EventsHistory(this)
  
  private var _objects = DoubleLinkedList.empty[GameObject]
  
  var name: String = "untitled"
  
  var gravity: Option[Gravity] = None
    
  /** All objects in this game. */
  def objects: Traversable[GameObject] = _objects

  /** All objects currently alive in this game. */
  def aliveObjects: Traversable[GameObject] = {
    val res = for(o <- _objects.toTraversable if o.existsAt(time)) yield o
    res //TODO: convert this to a val with view when it will work on mikael's machine.
  }
  
  /** Events from the current last full step. */
  def events = eventsHistory.lastEvents

  /** Time of the current step. It corresponds to the last fully 
   *  completed step.
   */
  def time = eventsHistory.time
  
  /*** Maximum time achieved in this game. */
  def maxTime = eventsHistory.maxTime
  
  /*** Minimum time stored in the history for this game. */
  def minTime = eventsHistory.minTime

  private[kingpong] def restore(t: Int): Unit = {
    if (t >= 0 && t <= maxTime) {
      objects.foreach(_.restore(t))
      eventsHistory.restore(t)
      world.clear()
    }
  }
  
  private[kingpong] def setRestoreTo(t: Int): Unit = {
    if (t >= 0 && t <= maxTime) {
      scheduledRestoreTime = Some(t)
    }
  }
  
  /** Add event over property modifications*/
  private val crtAssignmentEvents = ArrayBuffer.empty[(Positionable, AssignableProperty[_], Expr)]
  def addAssignmentHistory(p: Positionable, a: AssignableProperty[_], path: Expr): Unit = {
    crtAssignmentEvents += ((p, a, path))
  }
  
  /** Add event over property modifications */
  def addAssignmentEvents(): Unit = {
    crtAssignmentEvents foreach { case (pos, ap, e) =>
      eventsHistory.addEvent(AssignmentEvent(Vec2(pos.x.next, pos.y.next), ap, e))
    }
    crtAssignmentEvents.clear()
  }

  /** Clear the history from the given time (inclusive). 
   *  If `from` is less or equal to 0, the game is reset.
   */
  private[kingpong] def clear(from: Int): Unit = {
    objects foreach { obj =>
      if (from <= 0) {
        obj.reset(interpreter)
        obj.setExistenceAt(0)
      }
      obj.clear(from)
    }
    gc() // TODO: Call this method also to remove old objects.
    world.clear()
    eventsHistory.clear(from)
  }

  /** Perform a step. */
  private[kingpong] def update(): Unit = {
    scheduledRestoreTime match {
      case Some(t) => 
        restore(t)
        clear(from = t + 1)
        scheduledRestoreTime = None
      case None =>
    }

    eventsHistory.step()                               /// Opens saving for new coordinates
    rules foreach interpreter.evaluate                 /// Evaluate all rules using the previous events
    addAssignmentEvents()
    aliveObjects foreach {_.preStep(this)}
    updateWorldGravity()                               /// Update the gravity
    world.step()                                       /// One step forward in the world
    world.contactEvents foreach eventsHistory.addEvent /// Save contacts
    aliveObjects foreach {_.postStep(this)}
    
    //_objects.filter(o => o.creation_time.get <= time && time <= o.deletion_time.get )
    // TODO : Garbage collect objects that have been deleted for too much time and which cannot be created at the beginning
  }
  
  def updateWorldGravity() {
    if(gravity.nonEmpty &&
        (gravity.get.angle.get != worldgravityangle || gravity.get.radius.get != worldgravityradius || gravity.get.value.get != worldgravityenabled)) {
      worldgravityangle = gravity.get.angle.get
      worldgravityradius = gravity.get.radius.get
      worldgravityenabled = gravity.get.value.get
      if(worldgravityenabled) {
        world.setGravity(gravity.get.vector)
      } else {
        world.setGravity(Vec2(0, 0))
      }
      
    }
  }

  def add(o: GameObject, undoable: Boolean = true) = {
    o.creationTime.setInit(time)
    o.reset(interpreter)
    o.flush()
    _objects = _objects append (new DoubleLinkedList(o, DoubleLinkedList.empty[GameObject]))
    o match {
      case o:Gravity =>
        gravity = Some(o)
      case _ =>
    }
    if(undoable && isCopyingPlanned() != GameObject.RULE_BASED_PLANNING) {
      UndoRedo.recordObjectAdd(this, o);
    }
  }
  
  def remove(o: GameObject, undoable: Boolean = true) = {
    o match {
      case o: Gravity => gravity = None
      case _ =>
    }
    _objects = _objects.filter(g => g != o)
    if(undoable  && isCopyingPlanned() != GameObject.RULE_BASED_PLANNING) {
      UndoRedo.recordObjectRemove(this, o);
    }
  }
  
  def rename(o: GameObject, newName: String) = {
    UndoRedo.recordObjectRename(this, o, o.name.get, newName);
    o.name set newName
  }

  /** Register this rule in this game engine. */
  def register(rule: Expr) {
    addRule(rule)
  }

  def evaluate[T : PongType](e: Expr): T = {
    interpreter.evaluate(e).as[T]
  }

  /** Garbage collection of objects not planned since the beginning*/
  private[kingpong] def gc() = {
    //TODO performance... Use a double linked list for deleting objects.
    var firstObject = _objects;
    var current = _objects;
    while(current != null && current.nonEmpty) {
      val obj = current.head
      if(obj.plannedFromBeginning != GameObject.PLANNED_SINCE_BEGINNING &&
        (obj.creationTime.get > time || obj.deletionTime.get < maxTime - History.MAX_HISTORY_SIZE))  {
        obj.setExistenceAt(time);
        val tmp = current
        current = current.next
        if(tmp.prev eq null) _objects = current;
        tmp.remove()
      } else {
        current = current.next
      }
    }
  }
  
  var FINGER_SIZE = 20f
  private val circle = new CircleShape()
  val id = new org.jbox2d.common.Transform()
  id.setIdentity()
  
  /**
   * Returns the set of objects touching the finger at pos, only for the real.
   */
  def physicalObjectFingerAt(pos: Vec2): Set[GameObject] = {
    circle.m_p.set(pos.x, pos.y)
    circle.m_radius = FINGER_SIZE

    def collidesCircle(obj: GameObject): Boolean = {
      obj match {
        case p: PhysicalObject =>
          world.world.getPool().getCollision().testOverlap(p.body.getFixtureList().getShape(), 0, circle, 0, p.body.getTransform(), id)
          //world.world.getPool().getCollision().
          //obj.name.get == "Paddle1"
        case e: AbstractObject =>
          world.world.getPool().getCollision().testOverlap(e.getShape, 0, circle, 0, id, id)
        case _ =>
          false
      }
    }
    
    (aliveObjects filter collidesCircle).toSet
  }
  
  /**
   * Flush the object state and save them to init state if they are just created.
   */
  def validateNextToCurrent() {
    objects.foreach { obj =>
      obj.validate()
      obj.save(time)
      if (obj.setExistenceAt(time)) {
        obj.flush()
      }
      if(obj.creationTime.get == time) { // We push the object to the beginning.
        obj.historicalProperties foreach { p =>
		      p.setInit(p.getExpr)
		    }
      }
    }
  }
  
  /**
   * Returns the set of objects containing at this position.
   */
  def abstractObjectFingerAt(pos: Vec2): Traversable[GameObject] = {
    (aliveObjects filter (_.contains(pos)))
  }

  private[kingpong] def onAccelerometerChanged(vector: Vec2): Unit = {
    eventsHistory.addEvent(AccelerometerChanged(vector))
  }

  private[kingpong] def onFingerDown(pos: Vec2): Unit = {
    eventsHistory.addEvent(FingerDown(pos, null))
  }

  private[kingpong] def onFingerUp(pos: Vec2): Unit = {
    eventsHistory.addEvent(FingerUp(pos, null))
  }

  private[kingpong] def onOneFingerMove(from: Vec2, to: Vec2): Unit = {
    eventsHistory.addEvent(FingerMove(from, to, null))
  }
  
  private[kingpong] val interpreter = new Interpreter {
    /** Initialize the global context used during evaluation. */
    def initGC() = Game.this
    
    /** Initialize the recursive context used during evaluation. */
    //def initRC() = ImmutableRecContext.empty
    def initRC() = MutableRecContext.empty
  }
  
  def foreachEvent(f: (Event, Int) => Unit) = {
    eventsHistory.foreachEvent(f)
  }
  
  final val DEFAULT_NAME: String = "SHAPE"
  
  /**
   * Computes a new name based on the context and the given baseName
   */
  def getNewName(baseName: String): String = {
    var prefix = DEFAULT_NAME
    if(baseName != null && baseName != "") { 
      prefix = baseName.reverse.dropWhile(_.isDigit).reverse
    }
    if(prefix == "") {
      prefix = DEFAULT_NAME
    }
    if(!objects.exists(_.name.get == prefix)) return prefix
    var postFix = 1
    while(objects.exists(_.name.get == prefix + postFix)) {
      postFix += 1
    }
    prefix + postFix
  }
  
  protected var mIsCopyingPlanned: GameObject.IsPlanned = GameObject.PLANNED_SINCE_BEGINNING
  def setCopyingPlanned(b: GameObject.IsPlanned) = mIsCopyingPlanned = b
  def isCopyingPlanned(): GameObject.IsPlanned = mIsCopyingPlanned
}
