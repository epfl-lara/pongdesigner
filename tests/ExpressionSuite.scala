package ch.epfl.lara.synthesis.kingpong.test

import org.scalatest._

import org.jbox2d.dynamics.BodyType

import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.expression.TreeOps._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression.TypeOps._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.rules.EmptyContext
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.PhysicalWorld

class ExpressionSuite extends FlatSpec with Matchers {

  val i1 = IntegerLiteral(1)
  val i2 = IntegerLiteral(2)
  val i3 = IntegerLiteral(3)
  val i4 = IntegerLiteral(4)
  val bt = BooleanLiteral(true)
  val bf = BooleanLiteral(false)
  
  val interpreter = new Interpreter {
    def initGC() = new EmptyContext {}
    def initRC() = RecContext(Map.empty)
  }

  val game = new Game() {
    val world = new PhysicalWorld(Vec2(0, 0f))
    
    val blocks = Category("Blocks")()
    val balls = Category("Balls")()
    
    val ball1 = circle(balls)(name="ball1", x=2, y=2)
    val block1 = intbox(blocks)(name="block1", x=0, y=0)
    
    val rule1 = foreach(blocks, balls) { (block, ball) =>
      whenever(block.y < ball.y) (
        ball.y := 0, 
        ball.velocity := Vec2(0, 0)
      )
    }
    
    
//    val rule2 = foreach(paddles)("paddle"){
//      whenever(true)(
//        List(obj("paddle")("x"), obj("paddle")("width")) := Choose(List(obj("paddle")("x"), obj("paddle")("width")), obj("paddle")("left") =:= Border2("right") && obj("paddle")("right") =:= Ball1("x"))
//      // Should replace by obj("ball")("y") - obj("ball")("radius") and solved
//      )
//    }
  }

  
  //TODO test Choose expression
//  "Comfusy Solver" should "correctly compute programs" in {
//    game.rule2.evaluate(interpreter)
//    game.objects foreach {_.validate()}
//    game.paddle1.x.get should equal(1.5f)
//    game.paddle1.width.get should equal(2f)
//    
//  }
  
  //TODO test printer
//  "Pretty Printer extended" should "correctly output mappings" in {
//    val c = PrettyPrinterExtended.StringMaker()
//    val c2 = PrettyPrinterExtended.print(List(game.rule1))
//    c2.map.mObjects.keys should contain (game.paddles: Category)
//  }
  
  
  "Expression" should "be correctly built by the DSL" in {
    val e = game.ball1.x := 0
    e should be (Assign(List((ObjectLiteral(game.ball1), "x")), IntegerLiteral(0)))
  }
  
  ignore should "..." in {
    //TODO
  }
  
  "Interpreter" should "handle arithmetic expressions" in {
    val e1 = (i1 + i2) * i3 / i2
    interpreter.evaluate(e1) should be (FloatLiteral(4.5f))
    
    val e2 = (i2 * i2 + i2) % i4
    interpreter.evaluate(e2) should be (i2)
  }
  
  it should "handle boolean expressions" in {
    val e1 = !((bt || bf) && bf)
    interpreter.evaluate(e1) should be (bt)
    
    val e2 = bt && (bf || !bf) && bt
    interpreter.evaluate(e2) should be (bt)
    
    val e3 = (i1 < i2) && (i2 <= i2) && (i1 =:= i4)
    interpreter.evaluate(e3) should be (bf)
    
    val e4 = (i4 > i4) || (i3 >= i4) || i2 =:= bt || bf =:= bt
    interpreter.evaluate(e4) should be (bf)
  }
  
  it should "handle If expression" in {
    val e1 = If(i1 < i2, i2 * i2, i2 * i3)
    interpreter.evaluate(e1) should be (i4)
    
    val e2 = If(i1 >= i2, i2 * i2, i2 * i1)
    interpreter.evaluate(e2) should be (i2)
  }
  
  "TreeOps" should "correctly flatten a Block" in {
    val e = Block(i1, NOP, Block(NOP, i2, Block(i3)), NOP, i4, Block(Nil))
    val flat = flatten(e)
    flat should be (Block(i1, i2, i3, i4))
  }
  
  it should "correctly flatten a ParExpr" in {
    val e = ParExpr(List(i1, NOP, ParExpr(List(NOP, i2, Block(i3), ParExpr(List(i1)))), NOP, i4, Block(Nil)))
    val flat = flatten(e)
    flat should be (ParExpr(List(i1, i2, i3, i1, i4)))
  }
  
  "TypesOps" should "have correct subtypes" in {
    isSubtypeOf(TInt, TFloat) should be (true)
    isSubtypeOf(TFloat, TInt) should be (false)
    isSubtypeOf(TFloat, TString) should be (false)
    isSubtypeOf(TFloat, TFloat) should be (true)
    isSubtypeOf(TUnit, TString) should be (false)
    isSubtypeOf(TAny, TAny) should be (true)
    isSubtypeOf(TUnit, TAny) should be (true)
    isSubtypeOf(TInt, TAny) should be (true)
    isSubtypeOf(TFloat, TAny) should be (true)
  }
  
  
}