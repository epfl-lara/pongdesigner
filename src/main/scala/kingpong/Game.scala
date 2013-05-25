package ch.epfl.lara.synthesis.kingpong

import scala.collection.mutable.{Set => MSet}

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
  private val _rules = MSet.empty[Rule]

  /** All objects in this game. */
  def objects: Iterable[GameObject] = _objects

  /** All the rules in this game. */
  def rules: Iterable[Rule] = _rules
  
  //TODO what should be the right way to get back events, 
  // particularly for a time interval
  /** Events from the curent last full step. */
  def events = EventHistory.events

  def reset(): Unit = {
    _objects.foreach(_.reset(this)(EventHistory))
    world.clear()
    EventHistory.clear()
  }

  def add(o: GameObject) = _objects add o
  def add(rule: Rule) {
    typeCheck(rule)
    _rules add rule
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

  def whenever(cond: Expr)(actions: Seq[Stat]): Unit = {
    this add new Whenever(cond, toSingleStat(actions))
  }

  def once(cond: Expr)(actions: Seq[Stat]): Unit = {
    this add new Once(cond, toSingleStat(actions))
  }

  def on(cond: Expr)(actions: Seq[Stat]): Unit = {
    this add new On(cond, toSingleStat(actions))
  }

  private[kingpong] def objectAt(pos: Vec2): Option[GameObject] = {
    objects.filter(_.contains(pos)).headOption
  }

  /** Perform a step. */
  private[kingpong] def update(): Unit = {
    val time = EventHistory.step()
    rules foreach {_.evaluate(this)(EventHistory)}
    objects foreach {_.flush()}
    world.step()
    objects foreach {_.load()}
    objects foreach {_.save(time)}

    if (time == 200) {
      reset()
    }

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

    private var time: Long = 0
    private var history: RingBuffer[(Long, Seq[Event])] = new RingBuffer(History.MAX_HISTORY_SIZE)
    private val crtEvents = MSet.empty[Event]

    /* Advance the time and store the current events in the history. */
    def step(): Long = crtEvents.synchronized {
      val oldTime = time
      time += 1
      
      if (crtEvents.nonEmpty) {
        history += (oldTime, crtEvents.toSeq)
        crtEvents.clear()
      }
      
      oldTime
    }

    /** Return the events from the last fully completed time step. 
     *  Should not be called by another thread than the one who 
     *  call `step()`.
     */
    def events: Seq[Event] = {
      if (history.isEmpty || history.head._1 != time) {
        Seq.empty
      } else {
        history.head._2
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
      time = 0
      history.clear()
      crtEvents.clear()
    }

  }

}

class EmptyGame() extends Game {
  val world = new PhysicalWorld(Vec2(0, 1f))

  rectangle("Rectangle 1", 2, 0, width = 1, height = 1, fixedRotation = false)
  rectangle("Rectangle 2", 3.4, 0, width = 1, height = 2, fixedRotation = false)

  circle("Circle 1", 3, 2, radius = 1, fixedRotation = false)
  circle("Circle 2", 2.5, 4, radius = 0.5, fixedRotation = false)
  
  rectangle("Base", 0, 8, width = 20, height = 0.5, tpe = BodyType.STATIC)

}
