package ch.epfl.lara.synthesis.kingpong

import scala.Dynamic
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{HashMap => MMap}
import scala.collection.mutable.{Set => MSet, Seq => MSeq}
import scala.language.dynamics

import org.jbox2d.collision.shapes.CircleShape
import org.jbox2d.dynamics.BodyType

import android.util.Log

import ch.epfl.lara.synthesis.kingpong.common.ColorConstants
import ch.epfl.lara.synthesis.kingpong.common.History
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.RingBuffer
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression.TreeOps._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.rules.Context
import ch.epfl.lara.synthesis.kingpong.rules.Events._
import ch.epfl.lara.synthesis.kingpong.rules.Events.FingerDown

trait RuleManager {
  // Use an ArrayBuffer for performance reasons when using `foreach`.
  // An ArrayBuffer will use a simple while loop.
  private val _rules = ArrayBuffer.empty[Expr]
  private val _rulesByObject = MMap.empty[GameObject, MSet[Expr]]
  
  /** All the rules in this game. */
  def rules: Traversable[Expr] = _rules
  def setRuleByIndex(newRule: Expr, index: Int): Unit = {
    val oldRule = _rules(index)
    _rules(index) = newRule
    for((obj, rules) <- _rulesByObject) {
      if(rules contains oldRule) {
        rules -= oldRule
        rules += newRule
      }
    }
  }
  def getRuleByIndex(index: Int): Expr = _rules(index)
  def findRuleIndex(ruleMatcher: Expr => Boolean): Int = _rules.indexWhere(ruleMatcher)
  
  def getRulesbyObject(o: GameObject): Traversable[Expr] = _rulesByObject.getOrElse(o, List())
  def addRule(r: Expr): Unit = {
    def addToCategory(category: Category) {
      for(o<-category.objects) _rulesByObject.getOrElseUpdate(o, MSet()) += r
    }
    TreeOps.preTraversal(_ match {
      case ObjectLiteral(o) => _rulesByObject.getOrElseUpdate(o, MSet()) += r
      case Foreach(category, id, body) => addToCategory(category)
      case Forall(category, id, body) => addToCategory(category)
      case Find(category, id, body) => addToCategory(category)
      case c =>
    })(r)
    _rules += r
  }
  def getRulesbyObject(o: Iterable[GameObject]): Iterable[Expr] = (o flatMap getRulesbyObject)
}

trait Game extends RuleManager { self => 
  implicit val seflImplicit = self
  val world: PhysicalWorld
  var scheduledRestoreTime: Option[Int] = None

  private val _objects = ArrayBuffer.empty[GameObject]
  
  /** All objects in this game. */
  def objects: Traversable[GameObject] = _objects

  /** All objects currently alive in this game. */
  def aliveObjects: Traversable[GameObject] = {
    val res = for(o <- _objects.toTraversable if o.existsAt(time)) yield o
    res // todo: convert this to a val with view when it will work on mikael's machine.
  }
  
  //TODO what should be the right way to get back events, 
  // particularly for a time interval
  /** Events from the current last full step. */
  def events = EventHistory.events

  /** Time of the current step. It corresponds to the last fully 
   *  completed step.
   */
  def time = EventHistory.time
  
  /*** Maximum time achieved in this game. */
  def maxTime = EventHistory.maxTime
  
  /*** Minimum time stored in the history for this game. */
  def minTime = EventHistory.minTime

  private[kingpong] def restore(t: Int): Unit = {
    if (t >= 0 && t <= maxTime) {
      objects.foreach(_.restore(t))
      EventHistory.restore(t)
      world.clear()
    }
  }
  
  private[kingpong] def setRestoreTo(t: Int): Unit = {
    if (t >= 0 && t <= maxTime) {
      scheduledRestoreTime = Some(t)
    }
  }

  /** Clear the history from the given time (inclusive). 
   *  If `from` is less or equal to 0, the game is reset.
   */
  private[kingpong] def clear(from: Int): Unit = {
    if (from <= 0) {
      objects.foreach(_.reset(interpreter))
      gc()
    }
    objects.foreach(_.clear(from))
    world.clear()
    EventHistory.clear(from)
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
    world.beginContacts foreach { 
      EventHistory addEvent BeginContact(_)
    }
    world.currentContacts foreach {
      EventHistory addEvent CurrentContact(_)
    }
    world.endContacts foreach {
      EventHistory addEvent EndContact(_)
    }
    
    EventHistory.step()                                /// Opens saving for new coordinates
    
    rules foreach {interpreter.evaluate}               /// Evaluate all rules using the previous events
    aliveObjects foreach {_.preStep(EventHistory)}
    world.step()                                       /// One step forward in the world
    aliveObjects foreach {_.postStep(EventHistory)}
    
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

  def circle(category: CategoryObject)(name: Expr,
             x: Expr,
             y: Expr,
             radius: Expr = category.radius,
             visible: Expr = category.visible,
             velocity: Expr = category.velocity,
             angularVelocity: Expr = category.angularVelocity,
             density: Expr = category.density,
             friction: Expr = category.friction,
             restitution: Expr = category.restitution,
             fixedRotation: Expr = category.fixedRotation,
             color: Expr = category.color,
             sensor: Expr = category.sensor,
             tpe: BodyType = category.tpe): Circle = {
    val c = new Circle(this, name, x, y, radius, visible, velocity, angularVelocity, 
                       density, friction, restitution, fixedRotation, color, sensor, tpe)
    c.setCategory(category)
    this add c
    c
  }

  def rectangle(category: CategoryObject)(name: Expr,
                x: Expr,
                y: Expr,
                angle: Expr = category.angle,
                width: Expr = category.width,
                height: Expr = category.height,
                visible: Expr = category.visible,
                velocity: Expr = category.velocity,
                angularVelocity: Expr = category.angularVelocity,
                density: Expr = category.density,
                friction: Expr = category.friction,
                restitution: Expr = category.restitution,
                fixedRotation: Expr = category.fixedRotation,
                color: Expr = category.color,
                sensor: Expr = category.sensor,
                tpe: BodyType = category.tpe): Rectangle = {
    val r = new Rectangle(this, name, x, y, angle, width, height, visible, velocity, angularVelocity, 
                         density, friction, restitution, fixedRotation, color, sensor, tpe)
    r.setCategory(category)
    this add r
    r
  }
  
  def randomGenerator(category: CategoryObject)(name: Expr,
                      x: Expr,
                      y: Expr,
                      angle: Expr = category.angle,
                      width: Expr = category.width,
                      height: Expr = category.height,
                      minValue: Expr = category.randomMinValue,
                      maxValue: Expr = category.randomMaxValue,
                      visible: Expr = category.visible,
                      color: Expr = category.color): RandomGenerator = {
    val r = new RandomGenerator(this, name, x, y, angle, width, height, minValue, maxValue, visible, color)
    r.setCategory(category)
    this add r
    r
  }
  
  def drawingObject(category: CategoryObject)(name: Expr,
                x: Expr,
                y: Expr,
                angle: Expr = category.angle,
                width: Expr = category.width,
                height: Expr = category.height,
                visible: Expr = category.visible,
                color: Expr = category.color): DrawingObject = {
    val r = new DrawingObject(this, name, x, y, angle, width, height, visible, color)
    r.setCategory(category)
    this add r
    r
  }
  
  def array(category: CategoryObject)(
             name: Expr,
             x: Expr,
             y: Expr,
             columns: Expr,
             rows: Expr,
             angle: Expr = category.angle,
             visible: Expr = category.visible,
             color: Expr = category.color): Array2D = {
    val array = new Array2D(this, name, x, y, visible, color, columns, rows)
    array.setCategory(category)
    this add array
    array.cells.foreach(_ foreach { cell =>
      this add cell
    })
    array
  }

  def intbox(category: CategoryObject)(name: Expr,
             x: Expr,
             y: Expr,
             value: Expr = category.value,
             angle: Expr = category.angle,
             width: Expr = category.width,
             height: Expr = category.height,
             visible: Expr = category.visible,
             color: Expr = category.color): Box[Int] = {
    val box = new Box[Int](this, name, x, y, angle, width, height, value, visible, color)
    box.setCategory(category)
    this add box
    box
  }
  
  def booleanbox(category: CategoryObject)(name: Expr,
             x: Expr,
             y: Expr,
             value: Expr = category.value,
             angle: Expr = category.angle,
             width: Expr = category.width,
             height: Expr = category.height,
             visible: Expr = category.visible,
             color: Expr = category.color): Box[Boolean] = {
    val box = new Box[Boolean](this, name, x, y, angle, width, height, value, visible, color)
    box.setCategory(category)
    this add box
    box
  }
  
  def joystick(category: CategoryObject)(name: Expr,
             x: Expr,
             y: Expr,
             angle: Expr = category.angle,
             radius: Expr = category.radius,
             visible: Expr = category.visible,
             color: Expr = category.color): Joystick = {
    val joystick = new Joystick(this, name, x, y, angle, radius, visible, color)
    joystick.setCategory(category)
    this add joystick
    joystick
  }
  
  def character(category: CategoryObject)(name: Expr,
                x: Expr,
                y: Expr,
                angle: Expr = category.angle,
                width: Expr = category.width,
                height: Expr = category.height,
                visible: Expr = category.visible,
                velocity: Expr = category.velocity,
                angularVelocity: Expr = category.angularVelocity,
                density: Expr = category.density,
                friction: Expr = category.friction,
                restitution: Expr = category.restitution,
                fixedRotation: Expr = category.fixedRotation,
                color: Expr = category.color,
                tpe: BodyType = category.tpe): Character = {
    val r = new Character(this, name, x, y, angle, width, height, visible, velocity, angularVelocity, 
                          density, friction, restitution, fixedRotation, color, tpe)
    r.setCategory(category)
    this add r
    r
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
    EventHistory.addEvent(AccelerometerChanged(vector))
  }

  private[kingpong] def onFingerDown(pos: Vec2): Unit = {
    EventHistory.addEvent(FingerDown(pos, null))
  }

  private[kingpong] def onFingerUp(pos: Vec2): Unit = {
    EventHistory.addEvent(FingerUp(pos, null))
  }

  private[kingpong] def onOneFingerMove(from: Vec2, to: Vec2): Unit = {
    EventHistory.addEvent(FingerMove(from, to, null))
  }
  
  // Advanced way to add an event unsynchronized from input.
  def addEvent(e: Event, time: Int): Event = { EventHistory.addEvent(e, time); e}

  private val interpreter = new Interpreter {
    /** Initialize the global context used during evaluation. */
    def initGC() = EventHistory
    
    /** Initialize the recursive context used during evaluation. */
    //def initRC() = ImmutableRecContext.empty
    def initRC() = MutableRecContext.empty
  }
  
  /** Handle the history for events like fingers input and
   *  contacts between physical objects.
   *  This object also act as a context for the Interpreter. Indeed, 
   *  the evaluation of rules needs to be aware of events from the 
   *  last fully completed time step.
   */
  protected object EventHistory extends Context {

    private var recording_time: Int = 0
    private var min_time: Int = time
    private var max_time: Int = time

    // Oldest first (head), more recent at the end (last). Both in O(1).
    private val history: RingBuffer[(Int, Seq[Event])] = new RingBuffer(History.MAX_HISTORY_SIZE)
    private val crtEvents = ArrayBuffer.empty[Event]

    /** Time for the last fully completed step. */
    def time = recording_time - 1
    
    /** Minimum time stored in the history. */
    def minTime = min_time
    
    /** Maxixum time stored in the history. */
    def maxTime = max_time

    def restore(t: Int): Unit = {
      if (time >= 0 && time <= max_time) {
        recording_time = t + 1
        crtEvents.clear()
        
        // TODO : Add or remove objects
      }
    }
    
    def getEvents(i: Int): Seq[Event] = history flatMap { case (time, eventSeq) if time == i => eventSeq case _ => Seq() }
    
    def foreachEvent(f: (Event, Int) => Unit) = {
      for((k, s) <- history.iterator; e <- s) {
        f(e, k)
      }
    }

    /* Advance the time and store the current events in the history. */
    def step(): Unit = {
      //Log.d("kingpong", s"Before context step, recording_time = $recording_time, min_time = $min_time, max_time = $max_time.")
      val evts = crtEvents.synchronized {
        val res = crtEvents.toList
        crtEvents.clear()
        res
      }
      
      recording_time += 1
      if (max_time < time) {
         max_time = time
      }
      min_time = Math.max(0, Math.max(min_time, max_time - History.MAX_HISTORY_SIZE + 1))

      if (evts.nonEmpty) {
        // Here we find the objects under the finger events.
        val processedEvents = evts.map {
          case FingerMove(from, to, null) =>
            FingerMove(from, to, physicalObjectFingerAt(from))
          case FingerDown(pos, null) =>
            FingerDown(pos, physicalObjectFingerAt(pos))
          case FingerUp(pos, null) =>
            FingerUp(pos, physicalObjectFingerAt(pos))
          case e => e
        }
        
        history += (time, processedEvents)
      }
      //Log.d("kingpong", s"After context step, recording_time = $recording_time, min_time = $min_time, max_time = $max_time.")
    }
    
    /** Return the events from the last fully completed time step. 
     *  Should not be called by another thread than the one who 
     *  calls `step()`.
     */
    def events: Seq[Event] = {
      if(history.isEmpty || history.last._1 != time ) {
        Seq.empty
      } else {
        history.last._2
      }
    }

    /** Add an event in the ongoing time step. 
     *  Can be called by another thread, from user inputs.
     */
    def addEvent(e: Event): Unit = crtEvents.synchronized {
      e match {
        case AccelerometerChanged(_) =>
          // Remove the old AccelerometerChanged, if any
          val i = crtEvents.indexWhere(_.isInstanceOf[AccelerometerChanged])
          if (i >= 0) 
            crtEvents(i) = e
          else
            crtEvents += e
        case _ => 
          crtEvents += e
      }      
    }
    
    /** Add an event in the ongoing time step. 
     *  Can be called by another thread, from user inputs.
     *  Caution: This function should not be used very often
     */
    def addEvent(e: Event, time: Int): Unit = history.synchronized {
      history.replace({ case (t, events) => time == t }, { case (t, events) => (t, events ++ List(e)) })
    }

    /** Completely clear the history and the ongoing time step. */
    def clear(from: Int): Unit = crtEvents.synchronized {
      //Log.d("kingpong", s"Before context clearing, recording_time = $recording_time, min_time = $min_time, max_time = $max_time.")
      if (from <= 0) {
        recording_time = 0
        history.clear()
      } else  {
        recording_time = from
        val idx = history.lastIndexWhere(_._1 < from)
        history.removeFrom(idx + 1)
      }
      
      crtEvents.clear()
      max_time = time
      min_time = Math.min(min_time, time)
      //Log.d("kingpong", s"After context clearing, recording_time = $recording_time, min_time = $min_time, max_time = $max_time.")
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
    
    def add(c: GameObject) = self add c
  }
  
  def foreachEvent(f: (Event, Int) => Unit) = {
    EventHistory.foreachEvent(f)
  }
  
  def getNewName(baseName: String): String = EventHistory.getNewName(baseName)
  
  object ::> {def unapply[A] (l: Seq[A]): Option[(Seq[A],A)] = if(l.nonEmpty) Some( (l.init, l.last) ) else None }
  object <:: {def unapply[A] (l: Seq[A]): Option[(A, Seq[A])] = if(l.nonEmpty) Some( (l.head, l.tail) ) else None }

  /**
   * Retrieves a corresponding finger down event from a given finger event.
   * Events are "linked"
   */
  @tailrec final def getFingerDownEvent(e: Event, time: Int)(events: Seq[Event] = EventHistory.getEvents(time).reverse): Option[FingerDown] = e match {
    case e@FingerDown(_,_) => Some(e)
    case e@FingerUp(v,_) =>
      events match {
        case (e@FingerMove(a, b, _))<::q if b == v =>  getFingerDownEvent(e, time)(q)
        case (e@FingerDown(a, _))<::q if a == v => Some(e)
        case _ <::q => getFingerDownEvent(e, time)(q)
        case Nil if time > 0 => getFingerDownEvent(e, time-1)()
        case Nil => None
      }
    case e@FingerMove(a,b,_) =>
      events match {
        case (e@FingerMove(c, d, _))<::q if d == a =>  getFingerDownEvent(e, time)(q)
        case (e@FingerDown(c, _))<::q if c == a => Some(e)
        case _ <::q => getFingerDownEvent(e, time)(q)
        case Nil if time > 0 => getFingerDownEvent(e, time-1)()
        case Nil => None
      }
    case _ => None
  }
  
  /**
   * Retrieves a corresponding finger Up event from a given finger event.
   * Events are "linked"
   */
  @tailrec final def getFingerUpEvent(e: Event, time: Int)(events: Seq[Event] = EventHistory.getEvents(time)): Option[FingerUp] = e match {
    case e@FingerUp(_,_) => Some(e)
    case e@FingerDown(v,_) =>
      events match {
        case (e@FingerMove(a, b, _)) <::q if a == v =>  getFingerUpEvent(e, time)(q)
        case (e@FingerUp(a, _)) <::q if a == v => Some(e)
        case _ <::q => getFingerUpEvent(e, time)(q)
        case Nil if time > 0 => getFingerUpEvent(e, time-1)()
        case Nil => None
      }
    case e@FingerMove(a,b,_) =>
      events match {
        case (e@FingerMove(c, d, _)) <::q if b == c =>  getFingerUpEvent(e, time)(q)
        case (e@FingerUp(c, _)) <::q if c == a => Some(e)
        case _ <::q => getFingerUpEvent(e, time)(q)
        case Nil if time < maxTime => getFingerUpEvent(e, time+1)()
        case Nil => None
      }
    case _ => None
  }
  
  
  
  /**
   * Retrieves a corresponding finger Move event from a given finger event.
   * Collect all objects below the line.
   * Events are "linked"
   */
  @tailrec final def getFingerMoveEvent(e: Event, time: Int)(events: Seq[Event] = EventHistory.getEvents(time), start: Option[Vec2]=None, end: Option[Vec2]=None, objects: Set[GameObject]=Set()): Option[FingerMove] = e match {
    case e@FingerUp(v,o) =>
      start match {
        case Some(a) => Some(FingerMove(a, v, objects++o))
        case None => // We are looking for the previous finger down event
          events match {
            case q ::> (e@FingerMove(a, b, o2)) if b == v => getFingerMoveEvent(e, time)(q, None, Some(v), objects++o++o2)
            case q ::> (e@FingerDown(a, o2)) if a == v => Some(FingerMove(a,v,objects++o++o2))
            case q ::> _ => getFingerMoveEvent(e, time)(q,None,Some(v),objects++o)
            case Nil if time > 0 => getFingerMoveEvent(e, time-1)(start=None,end=end,objects=objects)
            case Nil => None
          }
      }
    case e@FingerDown(v,o) =>
      end match {
        case Some(a) => Some(FingerMove(v, a, objects++o))
        case None => // We are looking for the next finger up event
          events match {
            case (e@FingerMove(a, b, o2))<::q if a == v => getFingerMoveEvent(e, time)(q, Some(v), None, objects++o++o2)
            case (e@FingerUp(a, o2))<::q if a == v => Some(FingerMove(v,a,objects++o++o2))
            case _ <::q => getFingerMoveEvent(e, time)(q,Some(v),None,objects++o)
            case Nil if time < maxTime => getFingerMoveEvent(e, time+1)(start=start,end=None,objects=objects)
            case Nil => None
          }
      }
    case e@FingerMove(m,n,o) =>
      start match {
        case Some(v) => // We look for the end.
          events match {
            case (e@FingerMove(a, b, o2))<::q if a == n => getFingerMoveEvent(e, time)(q, start, None, objects++o++o2)
            case (e@FingerUp(a, o2))<::q if a == n => Some(FingerMove(v,a,objects++o++o2))
            case _ <::q => getFingerMoveEvent(e, time)(q,start,None,objects++o)
            case Nil if time < maxTime => getFingerMoveEvent(e, time+1)(start=start,end=None,objects=objects)
            case Nil => None
          }
        case None => // We first look for the start.
          end match {
            case Some(v) =>
              events match {
                case q ::> (e@FingerMove(a, b, o2)) if b == m => getFingerMoveEvent(e, time)(q, None, Some(v), objects++o++o2)
                case q ::> (e@FingerDown(a, o2)) if a == m => Some(FingerMove(a,v,objects++o++o2))
                case q ::> _ => getFingerMoveEvent(e, time)(q,None,Some(v),objects++o)
                case Nil if time > 0 => getFingerMoveEvent(e, time-1)(start=None,end=end,objects=objects)
                case Nil => None
              }
            case None => // By default, we look for the start first, then for the end.
              events match {
                case q ::> (e@FingerMove(a, b, o2)) if b == m => getFingerMoveEvent(e, time)(q, None, None, Set())
                case q ::> (e@FingerDown(a, o2)) if a == m => getFingerMoveEvent(e, time)(start=Some(a),end=None, objects=Set())
                case q ::> _ => getFingerMoveEvent(e, time)(q,None,None,Set())
                case Nil if time > 0 => getFingerMoveEvent(e, time-1)(start=None,end=None,objects=Set())
                case Nil => None
              }
          }
      }
    case _ => None
  }
}
