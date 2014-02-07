package ch.epfl.lara.synthesis.kingpong.test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TypeChecker
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.rules.Context
import org.jbox2d.dynamics.BodyType
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.PhysicalWorld
class ExpressionSuite2 extends FlatSpec with ShouldMatchers {
  implicit var interpreter: Interpreter with TypeChecker = _
  implicit var context: Context = new Context {
    def add(c: GameObject): Unit = {}
    def addAssignment(a: ch.epfl.lara.synthesis.kingpong.expression.Trees.Assign,p: ch.epfl.lara.synthesis.kingpong.expression.Trees.PropertyRef): Unit = {}
    def addEvent(e: ch.epfl.lara.synthesis.kingpong.rules.Events.Event): Unit = {}
     def events: Iterable[ch.epfl.lara.synthesis.kingpong.rules.Events.Event] = ???
     def get(value: String): Option[ch.epfl.lara.synthesis.kingpong.expression.Value] = ???
     def getNewName(s: String): String = ???
     def set(value: String,v: ch.epfl.lara.synthesis.kingpong.expression.Value): Unit = ???
     def time: Long = ???
     def addMethod(name: String,methodDecl: ch.epfl.lara.synthesis.kingpong.expression.Trees.MethodDecl): Unit = ???
     def getMethod(name: String): ch.epfl.lara.synthesis.kingpong.expression.Trees.MethodDecl = ???
  }

  interpreter = new Interpreter with TypeChecker {}

  val game = new Game() {
    val world = new PhysicalWorld(Vec2(0, 0f))
    val borders = Category("Borders")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
    val blocks = Category("Blocks")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
    val paddles = Category("Paddles")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
    val balls = Category("Balls")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
    val Ball1 = circle(balls)(name="Ball1", x=2.5, y=2, radius = 0.25, velocity=Vec2(0, -5.0f))
    val Border2 = intbox(borders)(name="Border2", x=0, y=0, width=1, height=2)
    val Border3 = intbox(borders)(name="Border3", x=1, y=0)
    val paddle1 = intbox(paddles)(name="paddle1", x=0, y=0)
    val expr1 = (paddle1 toRightOf Border2) && (paddle1 toLeftOf Border3)
    val rule1 = foreach(blocks)("o"){
    whenever(paddle1("y") < obj("o")("y")) (
      obj("o")("y") := 0, 
      obj("o")("velocity") := Vec2(0, 0)
    )
    }
    val rule2 = foreach(paddles)("paddle"){
       whenever(true)(
         List(obj("paddle")("x"), obj("paddle")("width")) := Choose(List(obj("paddle")("x"), obj("paddle")("width")), obj("paddle")("left") =:= Border2("right") && obj("paddle")("right") =:= Ball1("x"))
      // Should replace by obj("ball")("y") - obj("ball")("radius") and solved
    )
    }
  }
  /*
  val paddle1 = game.paddle1 // TODO : why does it fail ??
  val Border2 = game.Border2
  val Border3 = game.Border3
  */
  "Extended functions" should "type check" in {
    interpreter.typeCheck(game.expr1, TBoolean)
  }
  "Rules" should "type check" in {
    interpreter.typeCheck(game.rule1: RuleIterator)
  }
  "Comfusy Solver" should "correctly compute programs" in {
    interpreter.typeCheck(game.rule2)
    game.rule2.evaluate(interpreter)
    game.objects foreach {_.validate()}
    game.paddle1.x.get should equal(1.5f)
    game.paddle1.width.get should equal(2f)
    
  }
  
  "Pretty Printer extended" should "correctly output mappings" in {
    val c = PrettyPrinterExtended.StringMaker()
    val c2 = PrettyPrinterExtended.print(List(game.rule1))
    c2.map.mObjects.keys should contain (game.paddles: Category)
  }
}