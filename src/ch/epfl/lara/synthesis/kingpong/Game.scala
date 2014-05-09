package ch.epfl.lara.synthesis.kingpong

import scala.collection.mutable.ArrayBuffer
import org.jbox2d.collision.WorldManifold
import org.jbox2d.collision.shapes.CircleShape
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.rules.Context
import ch.epfl.lara.synthesis.kingpong.rules.Events._

trait Game extends RulesManager with Context { self => 
  protected implicit val seflImplicit = self
  private implicit val worldManifold = new WorldManifold()
  
  val world: PhysicalWorld
  
  // If the time is to be restored at the next step
  var scheduledRestoreTime: Option[Int] = None
  
  var pixelsByUnit = 0f

  private[kingpong] val eventsHistory = new EventsHistory(this)
  
  private val _objects = ArrayBuffer.empty[GameObject]
  
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
    gc()
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
    // save contacts
    world.beginContacts foreach { contact => 
      eventsHistory addEvent (BeginContact(contact.point, contact.objectA, contact.objectB))
    }
    world.currentContacts foreach { contact => 
      eventsHistory addEvent (CurrentContact(contact.point, contact.objectA, contact.objectB))
    }
    world.endContacts foreach { contact => 
      eventsHistory addEvent (EndContact(contact.point, contact.objectA, contact.objectB))
    }
    
    eventsHistory.step()                                /// Opens saving for new coordinates
    
    rules foreach {interpreter.evaluate}               /// Evaluate all rules using the previous events
    addAssignmentEvents()
    aliveObjects foreach {_.preStep(this)}
    world.step()                                       /// One step forward in the world
    aliveObjects foreach {_.postStep(this)}
    
    //_objects.filter(o => o.creation_time.get <= time && time <= o.deletion_time.get )
    // TODO : Garbage collect objects that have been deleted for too much time.
  }
  
  /**
   * setInstantProperties sets all properties to instant mode.
   * If true, this means that when calling setNext() on properties, it will also set up current()
   * if false, it is deactivated.
   */
  def setInstantProperties(activate: Boolean) = {
    for (o <- objects; prop <- o.historicalProperties) {
      prop.setInstant(activate)
    }
  }

  def add(o: GameObject) = {
    o.reset(interpreter)
    o.flush()
    _objects += o
  }
  
  def remove(o: GameObject) = {
    _objects -= o
  }
  
  def rename(o: GameObject, newName: String) = {
    o.name set newName
  }

  /** Register this rule in this game engine. */
  def register(rule: Expr) {
    addRule(rule)
  }

  def evaluate[T : PongType](e: Expr): T = {
    interpreter.evaluate(e).as[T]
  }

  private def gc() = {
    //TODO performance...
    val toDelete = _objects.filter { obj =>
      obj.creationTime.get > time
    }
    // Remove objects from world and categories
    toDelete foreach (_.setExistenceAt(time)) 
    _objects --= toDelete
    // TODO: remove those who are too old to be resurrected and also rules applying to them.
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
   * Returns the set of objects containing at this position.
   */
  def abstractObjectFingerAt(pos: Vec2): Set[GameObject] = {
    (aliveObjects filter (_.contains(pos))).toSet
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
  
  private val interpreter = new Interpreter {
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
    var postFix = 1
    
    //TODO find postfix
    prefix //+ 42
    //Stream.from(1).map(prefix+_).find(get(_) == None).getOrElse(DEFAULT_NAME)
  }
  
}
