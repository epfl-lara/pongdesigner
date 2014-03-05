package ch.epfl.lara.synthesis.kingpong

import scala.Dynamic
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{HashMap => MMap}
import scala.collection.mutable.{Set => MSet}
import scala.language.dynamics

import org.jbox2d.collision.shapes.CircleShape
import org.jbox2d.dynamics.BodyType

import ch.epfl.lara.synthesis.kingpong.common.ColorConstants
import ch.epfl.lara.synthesis.kingpong.common.History
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.RingBuffer
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.rules.Context
import ch.epfl.lara.synthesis.kingpong.rules.Events._
import ch.epfl.lara.synthesis.kingpong.rules.Rules._

trait RuleManager {
  private val _rules = ListBuffer[Stat]()
  private val _rulesByObject = MMap.empty[GameObject, MSet[Stat]]
  
  /** All the rules iterators in this game. */
  def rules: Iterable[Stat] = _rules
  def getRules(): Iterable[Stat] = _rules
  def getRulesbyObject(o: GameObject):  Iterable[Stat] = _rulesByObject.getOrElse(o, List())
  def addRule(r: Stat) = {
    r traverse {
      case p: PropertyRef => _rulesByObject.getOrElseUpdate(p.property.parent, MSet()) += r
        TraverseMode.ContinueSiblings
      case c: GameObjectRef if c.obj != null  => _rulesByObject.getOrElseUpdate(c.obj, MSet()) += r
        TraverseMode.ContinueSiblings
      case _ =>
        TraverseMode.ContinueWithChildren
    }
    _rules += r
  }
  def getRulesbyObject(o: Iterable[GameObject]): Iterable[Stat] = (o flatMap getRulesbyObject)
}

trait Game extends TypeChecker with Interpreter with ColorConstants with RuleManager { self => 
  implicit val seflImplicit = self
  val world: PhysicalWorld

  private val _objects = MSet.empty[GameObject]
  

  /** All objects in this game. */
  def objects: MSet[GameObject] = _objects
  
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

  private[kingpong] def restore(t: Long): Unit = {
    if (t >= 0 && t <= maxTime) {
      objects.foreach(_.restore(t))
      //rules.foreach(_.restore(t))
      EventHistory.restore(t)
      world.clear()
    }
  }

  private[kingpong] def reset(): Unit = {
    objects.foreach(_.reset(this)(EventHistory))
    objects.foreach(_.clear())
    //rules.foreach(_.reset())
    //rules.foreach(_.clear())
    world.clear()
    EventHistory.clear()
  }

  /** Perform a step. */
  private[kingpong] def update(): Unit = {

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
    
    rules foreach {_.evaluate(this)(EventHistory)}     /// Evaluate all rules using the previous events
    objects foreach {_.validate()}                     /// Store new computed values
    objects.foreach {o => o.setExistenceAt(time.toInt) }
    objects foreach {_.flush()}                        /// push values to physical world
    world.step()                                       /// One step forward in the world
    objects foreach {_.load()}                         /// Load values from world
    objects foreach { case i: InputManager =>          /// Maps the previous events to the input mechanisms.
      i.collectInput(EventHistory) case _ => }
    objects foreach {_.save(time)}                     /// Save the values to history
    //rules foreach {_.save(time)}                       /// Save "on" and "once" values.
    
    //_objects.filter(o => o.creation_time.get <= time && time <= o.deletion_time.get )
    // TODO : Garbage collect objects that have been deleted for too much time.
  }
  
  /**
   * setInstantProperties sets all properties to instant mode.
   * If true, this means that when calling setNext() on properties, it will also set up current()
   * if false, it is deactivated.
   */
  def setInstantProperties(activate: Boolean) = {
    for (o <- objects; prop <- o.writableProperties) {
      prop.setInstant(activate)
    }
  }

  def add(o: GameObject) = {
    _objects add o
    EventHistory.set(o.name.get, GameObjectV(o))
  }

  /** Register this rule iterator in this game engine. */
  def register(iterator: RuleIterator) {
    typeCheck(iterator)(EventHistory)
    addRule(iterator)
  }

  /** Register this rule in this game engine. */
  def register(rule: Stat) {
    //val iterator = new NoCategory(rule)
    typeCheck(rule)(EventHistory)
    addRule(rule)
  }

  def typeCheckAndEvaluate[T : PongType](e: Expr): T = {
    typeCheck(e, implicitly[PongType[T]].getPongType)(EventHistory)
    eval(e)(EventHistory).as[T]
  }
  def evaluate(e: Stat): Unit = {
    eval(e)(EventHistory)
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
    if(category != null) c.setCategory(category)
    c.reset(this)(EventHistory)
    c.flush()
    this add c
    c
  }
  
  def activeBox(category: CategoryObject)(name: Expr,
             x: Expr,
             y: Expr,
             angle: Expr = category.angle,
             radius: Expr = category.radius,
             visible: Expr = category.visible,
             color: Expr = category.color,
             picture: Expr = StringLiteral("")): ActiveBox = {
    val b = new ActiveBox(this, name, x, y, angle, radius, visible, color, picture)
    if(category != null) b.setCategory(category)
    b.reset(this)(EventHistory)
    this add b
    b
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
    if(category != null) r.setCategory(category)
    r.reset(this)(EventHistory)
    r.flush()
    this add r
    r
  }
  
  def array(category: CategoryObject)(
             name: Expr,
             x: Expr,
             y: Expr,
             angle: Expr = category.angle,
             width: Expr = category.width,
             height: Expr = category.height,
             visible: Expr = category.visible,
             color: Expr = category.color): Array2D = {
    val array = Array2D(this, name, x, y, visible, color, 2, 3)
    if(category != null) array.setCategory(category)
    array.reset(this)(EventHistory)
    this add array
    array.cells.foreach(_ foreach { cell =>
      cell.reset(this)(EventHistory)
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
    if(category != null) box.setCategory(category)
    box.reset(this)(EventHistory)
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
    if(category != null) box.setCategory(category)
    box.reset(this)(EventHistory)
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
    if(category != null) joystick.setCategory(category)
    joystick.reset(this)(EventHistory)
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
    if(category != null) r.setCategory(category)
    r.reset(this)(EventHistory)
    r.flush()
    this add r
    r
  }

  def foreach(category: Category)(nameBinding: String)(rule: Stat): RuleIterator = {
    Foreach1(category, nameBinding, rule)
  }

  def foreach(category1: Category, category2: Category)(name1: String, name2: String)
             (rule: Stat): RuleIterator = {
    Foreach1(category1, name1, Foreach1(category2, name2, rule))
  }

  def whenever(cond: Expr)(actions: Stat*): Stat = {
    If(cond, toSingleStat(actions.toSeq), None)
  }
  
  def gc() = {
    _objects.retain(obj => if(!(obj.doesNotYetExist(time))) {
      true
    } else {
      obj.setExistenceAt(time)
      false
    }) // TODO: remove those who are too old to be resurrected and also rules applying to them.
  }
  
  var FINGER_SIZE = 20f
  private val circle = new CircleShape()
  val id = new org.jbox2d.common.Transform()
  id.setIdentity()
  
  def objectFingerAt(pos: Vec2): MSet[GameObject] = {
    circle.m_p.set(pos.x, pos.y)
    circle.m_radius = FINGER_SIZE

    def collidesCircle(obj: GameObject): Boolean = {
      obj match {
        case p:PhysicalObject =>
          world.world.getPool().getCollision().testOverlap(p.body.getFixtureList().getShape(), 0, circle, 0, p.body.getTransform(), id)
          //world.world.getPool().getCollision().
          //obj.name.get == "Paddle1"
        case e:AbstractObject =>
          world.world.getPool().getCollision().testOverlap(e.getShape, 0, circle, 0, id, id)
        case _ =>
          false
      }
    }
    
    val objects_containing_pos = objects filter collidesCircle
    objects_containing_pos
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
  protected object EventHistory extends Context {

    private var recording_time: Long = 0
    private var max_time: Long = 0

    // Oldest first (head), more recent at the end (last). Both in O(1).
    private var history: RingBuffer[(Long, Seq[Event])] = new RingBuffer(History.MAX_HISTORY_SIZE)

    private var crtEvents = ListBuffer[Event]()

    /** Time for the last fully completed step. */
    def time = recording_time - 1
    def maxTime = max_time

    def restore(t: Long): Unit = {
      if (time >= 0 && time <= max_time) {
        recording_time = t + 1
        crtEvents.clear()
        // TODO : Add or remove objects
        
        //crtEvents = Nil
      }
    }

    /* Advance the time and store the current events in the history. */
    def step(): Unit = {
      val c = crtEvents
      recording_time += 1
      
      if (max_time < time)
        max_time = time

      if (crtEvents.nonEmpty) {
        // Here we find the objects under the finger events.
        val history_events = c.toSeq.map {
          case FingerMove(from, to, null) =>
            FingerMove(from, to, objectFingerAt(from))
          case FingerDown(pos, null) =>
            FingerDown(pos, objectFingerAt(pos))
          case FingerUp(pos, null) =>
            FingerUp(pos, objectFingerAt(pos))
          case e => e
        }
        
        history += (time, history_events)
        crtEvents.clear()
      }
    }
    
    private var currentAssignments = MMap[Assign, List[PropertyRef]]()
    def addAssignment(a: Assign, p: PropertyRef) = currentAssignments(a) = p::currentAssignments.getOrElseUpdate(a, Nil)
    def resetAssignments() = currentAssignments = MMap[Assign, List[PropertyRef]]()

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
          crtEvents = crtEvents.filter{
            case _ : AccelerometerChanged => false
            case _ => true
          }
          crtEvents += e
        case e@BeginContact(_) =>
          crtEvents += e
        case _ => 
          crtEvents += e
      }      
    }

    /** Completely clear the history and the ongoing time step. */
    def clear(): Unit = crtEvents.synchronized {
      recording_time = 0
      max_time = 0
      history.clear()
      crtEvents.clear()
    }
    
    private var ctx: MMap[String, Value] = MMap.empty
    def get(value: String): Option[Value] = ctx.get(value)
    def set(value: String, v: Value): Unit = ctx(value) = v
    
    val methods: MMap[String, MethodDecl] = MMap[String, MethodDecl]()
    def addMethod(name: String, methodDecl: MethodDecl) = methods(name) = methodDecl
    def getMethod(name: String): MethodDecl = methods(name)
    
    final val DEFAULT_NAME: String = "SHAPE"
    /**
     * Computes a new name based on the context and the given baseName
     */
    def getNewName(baseName: String): String = {
      var prefix = DEFAULT_NAME
      if(baseName != null && baseName != "") { 
        //prefix = "\\d*\\z".r.replaceFirstIn(baseName, "")
        //var i = baseName.length - 1
        prefix = baseName.reverse.dropWhile(_.isDigit).reverse
      }
      if(prefix == "") {
        prefix = DEFAULT_NAME
      }
      var postFix = 1
      Stream.from(1).map(prefix+_).find(get(_) == None).getOrElse(DEFAULT_NAME)
    }
    
    def add(c: GameObject) = self add c
  }

  //private var mGameView: GameViewInterface = null
  //def setGameEngine(g: GameViewInterface) = {
  //  mGameView = g
  //}
  // Abstract to implement
  def storeInitialState(overwrite: Boolean)= {
    // TODO
  }
  
  /// Unnamed constant pointer always pointing to the game object
  implicit def gameObjectToGameObjectRef(g: GameObject): GameObjectRef = GameObjectRef(ObjectLiteral(g))

  /// Pointer having a name but pointing to nothing yet.
  def obj(s: String): GameObjectRef = GameObjectRef(s)
  
  def relative_dx: Expr = FingerCoordX2 - FingerCoordX1
  def relative_dy: Expr = FingerCoordY2 - FingerCoordY1
}

class EmptyGame() extends Game {
  val world = new PhysicalWorld(Vec2(0, 1.5f))

  val cat = Category("Moving objects")()
  val catAdd = Category("Moving objects2")()

  val rect1 = rectangle(cat)(name="Rectangle 1", x=2, y=0, width = 1, height = 1, fixedRotation = false)
  val rect2 = rectangle(cat)(name="Rectangle 2", x=3.4, y=0, width = 1, height = 2, fixedRotation = false)

  circle(cat)("Circle 1", 3, 2, radius = 1, fixedRotation = false)
  val c2 = circle(catAdd)("Circle 2", 2.5, 4, radius = 0.5, fixedRotation = false)
  
  val score = intbox(Category("scores")())("Score", 1, 1, value = 0)

  val cat2 = Category("Static objects")()
  val catArray = Category("Array objects")()

  val base = rectangle(cat2)("Base", 0, 8, width = 20, height = 0.5, tpe = BodyType.STATIC)

  val arr = array(catArray)("MyArray", 1.3, 4)
  
  
  val r1 = foreach(cat)("o"){ foreach(base.category)("base") {
    whenever(obj("base")("y") < obj("o")("y"))(
      obj("o")("y") := 0, 
      obj("o")("velocity") := Vec2(0, 0)
    )
  }}

  val r2 = foreach(cat, cat2)("o1", "o2") { foreach(score.category)("score") {
    whenever(Collision(obj("o1"), obj("o2"))) (
      obj("score")("value") += 1
    )
  }}

  val r3 = foreach(catAdd)("c2") {
    whenever(FingerDownOver(obj("c2")))(
    obj("c2")("radius") += 0.1
  )}
  
  val cell = arr.cells(1)(1)
  
  val r4 = whenever(Contains(cell, rect1))(
      rect1("x") := cell("x"),
      rect1("y") := cell("y"),
      rect1("velocity") := Vec2(0, 0)
    )

  register(r1)
  register(r2)
  register(r3)
  register(r4)

}
