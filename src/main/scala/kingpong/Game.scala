package ch.epfl.lara.synthesis.kingpong

import scala.collection.mutable.{Set => MSet}
import scala.math.Numeric$DoubleIsFractional$

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.History
import ch.epfl.lara.synthesis.kingpong.common.RingBuffer
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.rules.Rules._
import ch.epfl.lara.synthesis.kingpong.rules.Events._
import ch.epfl.lara.synthesis.kingpong.rules.Context

//TODO remove when useless
import org.jbox2d.dynamics.BodyType

import android.util.Log

trait Game extends TypeChecker with Interpreter { self => 

  val world: PhysicalWorld

  private val _objects = MSet.empty[GameObject]
  private val _rules = MSet.empty[RuleIterator]

  /** All objects in this game. */
  def objects: Iterable[GameObject] = _objects

  /** All the rules iterators in this game. */
  def rules: Iterable[RuleIterator] = _rules
  
  //TODO what should be the right way to get back events, 
  // particularly for a time interval
  /** Events from the curent last full step. */
  def events = EventHistory.events

  /** Time of the current step. It corresponds to the last fully 
   *  completed step.
   */
  def time = EventHistory.time
  
  /*** Maximum time achieved in this game. */
  def maxTime = EventHistory.maxTime

  private[kingpong] def restore(t: Long): Unit = {
    if (t >= 0 && t <= maxTime) {
      objects.foreach(_.restore(t))
      rules.foreach(_.restore(t))
      EventHistory.restore(t)
      world.clear()
    }
  }

  private[kingpong] def reset(): Unit = {
    objects.foreach(_.reset(this)(EventHistory))
    objects.foreach(_.clear())
    rules.foreach(_.reset())
    rules.foreach(_.clear())
    world.clear()
    EventHistory.clear()
  }

  /** Perform a step. */
  private[kingpong] def update(): Unit = {

    // save contacts
    world.beginContacts foreach { EventHistory addEvent BeginContact(_) }
    world.currentContacts foreach { EventHistory addEvent CurrentContact(_) }
    world.endContacts foreach { EventHistory addEvent EndContact(_) }

    EventHistory.step()
    rules foreach {_.evaluate(this)(EventHistory)}
    objects foreach {_.validate()}
    objects foreach {_.flush()}
    world.step()
    objects foreach {_.load()}
    objects foreach {_.save(time)}
    rules foreach {_.save(time)}

    //if (time == 200) {
    // restore(1)
    //}

  }


  def add(o: GameObject) = _objects add o

  /** Register this rule iterator in this game engine. */
  def register(iterator: RuleIterator) {
    typeCheck(iterator)
    _rules add iterator
  }

  /** Register this rule in this game engine. */
  def register(rule: Rule) {
    val iterator = new NoCategory(rule)
    typeCheck(iterator)
    _rules add iterator
  }

  def typeCheckAndEvaluate[T : PongType](e: Expr): T = {
    typeCheck(e, implicitly[PongType[T]].getPongType)
    eval(e)(EventHistory).as[T]
  }

  def circle(name: Expr,
             x: Expr,
             y: Expr,
             radius: Expr = 0.5, //TODO centralized default values
             visible: Expr = true,
             velocity: Expr = Vec2(0, 0),
             angularVelocity: Expr = 0,
             density: Expr = 1,
             friction: Expr = 0.2,
             restitution: Expr = 0.5,
             fixedRotation: Expr = true,
             tpe: BodyType = BodyType.DYNAMIC): Circle = {
    val c = new Circle(this, name, x, y, radius, visible, velocity, angularVelocity, 
                       density, friction, restitution, fixedRotation, tpe)
    c.reset(this)(EventHistory)
    c.flush()
    this add c
    c
  }

  def rectangle(name: Expr,
                x: Expr,
                y: Expr,
                angle: Expr = 0,
                width: Expr = 1,
                height: Expr = 1,
                visible: Expr = true,
                velocity: Expr = Vec2(0, 0),
                angularVelocity: Expr = 0,
                density: Expr = 1,
                friction: Expr = 0.2,
                restitution: Expr = 0.5,
                fixedRotation: Expr = true,
                tpe: BodyType = BodyType.DYNAMIC): Rectangle = {
    val r = new Rectangle(this, name, x, y, angle, width, height, visible, velocity, angularVelocity, 
                          density, friction, restitution, fixedRotation, tpe)
    r.reset(this)(EventHistory)
    r.flush()
    this add r
    r
  }

  def intbox(name: Expr,
             x: Expr,
             y: Expr,
             value: Expr = 0,
             angle: Expr = 0,
             width: Expr = 1,
             height: Expr = 1,
             visible: Expr = true): Box[Int] = {
    val box = new Box[Int](name, x, y, angle, width, height, value, visible)
    box.reset(this)(EventHistory)
    this add box
    box
  }

  def foreach(category: Category)(rule: GameObject => Rule): RuleIterator = {
    new Foreach1(category, rule)
  }

  def foreach(category1: Category, category2: Category)
             (rule: (GameObject, GameObject) => Rule): RuleIterator = {
    new Foreach2(category1, category2, rule.tupled)
  }

  def whenever(cond: Expr)(actions: Seq[Stat]): Rule = {
    new Whenever(cond, toSingleStat(actions))
  }

  def once(cond: Expr)(actions: Seq[Stat]): Rule = {
    new Once(cond, toSingleStat(actions))
  }

  def on(cond: Expr)(actions: Seq[Stat]): Rule = {
    new On(cond, toSingleStat(actions))
  }

  private[kingpong] def objectAt(pos: Vec2): Option[GameObject] = {
    objects.filter(_.contains(pos)).headOption
  }

  private[kingpong] def onAccelerometerChanged(vector: Vec2): Unit = {
    EventHistory.addEvent(AccelerometerChanged(vector))
  }

  private[kingpong] def onFingerDown(pos: Vec2): Unit = {
    EventHistory.addEvent(FingerDown(pos, objectAt(pos)))
  }

  private[kingpong] def onFingerUp(pos: Vec2): Unit = {
    EventHistory.addEvent(FingerUp(pos, objectAt(pos)))
  }

  private[kingpong] def onOneFingerMove(from: Vec2, to: Vec2): Unit = {
    EventHistory.addEvent(FingerMove(from, to, objectAt(from)))
  }

  private def toSingleStat(stats: Seq[Stat]): Stat = stats match {
    case Seq()  => NOP
    case Seq(s) => s
    case _      => Block(stats)
  }

  /** Handle the history for events like fingers input and
   *  contacts between physical objects.
   *  This object also act as a context for the Interpreter. Indeed, 
   *  the evaluation of rules needs to be aware of events from the 
   *  last fully completed time step.
   */
  private object EventHistory extends Context {

    private var recording_time: Long = 0
    private var max_time: Long = 0

    // Oldest first (head), more recent at the end (last). Both in O(1).
    private var history: RingBuffer[(Long, Seq[Event])] = new RingBuffer(History.MAX_HISTORY_SIZE)

    private val crtEvents = MSet.empty[Event]

    /** Time for the last fully completed step. */
    def time = recording_time - 1
    def maxTime = max_time

    def restore(t: Long): Unit = {
      if (time >= 0 && time <= max_time) {
        recording_time = t + 1
        crtEvents.clear()
      }
    }

    /* Advance the time and store the current events in the history. */
    def step(): Unit = crtEvents.synchronized {
      recording_time += 1
      
      if (max_time < time)
        max_time = time

      if (crtEvents.nonEmpty) {
        history += (time, crtEvents.toSeq)
        crtEvents.clear()
      }
    }

    /** Return the events from the last fully completed time step. 
     *  Should not be called by another thread than the one who 
     *  call `step()`.
     */
    def events: Seq[Event] = {
      if (history.isEmpty || history.last._1 != time ) {
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
          crtEvents.retain{
            case _ : AccelerometerChanged => false
            case _ => true
          }
          crtEvents add e
        case _ => 
          crtEvents add e
      }      
    }

    /** Completely clear the history and the ongoing time step. */
    def clear(): Unit = crtEvents.synchronized {
      recording_time = 0
      max_time = 0
      history.clear()
      crtEvents.clear()
    }

  }

}

class EmptyGame() extends Game {
  val world = new PhysicalWorld(Vec2(0, 1.5f))

  val cat = new Category("Moving objects")

  rectangle("Rectangle 1", 2, 0, width = 1, height = 1, fixedRotation = false).withCategory(cat)
  rectangle("Rectangle 2", 3.4, 0, width = 1, height = 2, fixedRotation = false).withCategory(cat)

  circle("Circle 1", 3, 2, radius = 1, fixedRotation = false).withCategory(cat)
  val c2 = circle("Circle 2", 2.5, 4, radius = 0.5, fixedRotation = false).withCategory(cat)
  
  val score = intbox("Score", 1, 1, value = 0)

  val cat2 = new Category("Static objects")

  val base = rectangle("Base", 0, 8, width = 20, height = 0.5, tpe = BodyType.STATIC).withCategory(cat2)

  val r1 = foreach(cat) { o =>
    whenever(base("y") < o("y")) { Seq(
      o("y") := 0, 
      o("velocity") := Vec2(0, 0)
    )}
  }

  val r2 = foreach(cat, cat2) { (o1, o2) =>
    on(Collision(o1, o2)) { Seq(
      score("value") += 1
    )}  
  }

  val r3 = whenever(FingerDownOver(c2)) { Seq(
    c2("radius") += 0.1
  )}


  register(r1)
  register(r2)
  register(r3)

}
