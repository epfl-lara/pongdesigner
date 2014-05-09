package ch.epfl.lara.synthesis.kingpong.test

import org.scalatest._

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
    def initRC() = ImmutableRecContext.empty
  }

  val game = new Game() {
    val world = new PhysicalWorld(Vec2(0, 0f))
    
    val blocks = Category("Blocks")()
    val balls = Category("Balls")()
    val arrays = Category("Arrays")()
    
    val ball1 = circle(balls)(name="ball1", x=2, y=2)
    val block1 = intbox(blocks)(name="block1", x=0, y=0)
    val block2 = intbox(blocks)(name="block2", x=1, y=0)
    val arr = array(arrays)("array1", x=0, y=0, columns=2, rows=2)
    
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
  
  it should "have a syntactic sugar for multiple assignment" in {
    val e = (game.ball1.x, game.ball1.y) := (2f, 3f)
    val let = e.asInstanceOf[Let]
    val id = let.id
    let shouldBe Let(id, Tuple(Seq(2f, 3f)), Block(Seq(
      Assign(List((ObjectLiteral(game.ball1), "x")), TupleSelect(Variable(id), 1)),
      Assign(List((ObjectLiteral(game.ball1), "y")), TupleSelect(Variable(id), 2))
    )))
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
  
  it should "handle Forall expression" in {
    val e1 = forall(game.blocks) { block =>
      block.x =:= 1f
    }
    interpreter.evaluate(e1) should be (bf)
    
    val e2 = forall(game.blocks) { block =>
      block.y =:= 0f
    }
    interpreter.evaluate(e2) should be (bt)
  }
  
  it should "handle Find expression" in {
    val e1 = find(game.blocks) { block =>
      block.x =:= 1f
    }
    interpreter.evaluate(e1) should be (ObjectLiteral(game.block2))
    
    val e2 = find(game.blocks) { block =>
      block.y =:= 1f
    }
    interpreter.evaluate(e2) should be (ObjectLiteral(null))
  }
  
  it should "correctly assign properties" in {
    game.ball1.snapshot()
    val radius = game.ball1.radius
    radius.set(1)
    
    val e1 = radius += 2.5f
    e1 should be (Assign(List((ObjectLiteral(game.ball1), "radius")), Plus(Select(ObjectLiteral(game.ball1), "radius"), FloatLiteral(2.5f))))
    radius.get should be (1f)
    interpreter.evaluate(e1) should be (UnitLiteral)
    radius.next should be (3.5f)
    radius.validate()
    radius.get should be (3.5f)
    
    // redo the assign to test the validate and the mutability.
    interpreter.evaluate(e1) should be (UnitLiteral)
    radius.next should be (6f)
    radius.validate()
    radius.get should be (6f)
    
    // test the assign with an indirect reference to the object.
    val e2 = foreach(game.balls) { ball =>
      ball.radius := 2
    }
    interpreter.evaluate(e2) should be (UnitLiteral)
    radius.next should be (2f)
    radius.validate()
    radius.get should be (2f)
    
    game.ball1.revert()
  }
  
  it should "throw an exception when assigning a RO property" in {
    val e1 = foreach(game.arr.cellsCategory) { cell =>
      cell.x := 2
    }
    an [InterpreterException] should be thrownBy interpreter.evaluate(e1)
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
  
  it should "correctly collect objects in an expression" in {
    val e = If(game.ball1.visible && game.block1.visible, game.ball1.x := game.ball1.x + game.block1.y)
    val objects = collectObjects(e)
    objects should be (Set(game.ball1, game.block1))
  }
  
  it should "correctly generalize an expression" in {
    val obj = game.ball1
    val e = If(obj.visible && game.block1.visible, obj.x := obj.x + game.block1.y)
    val generalized = generalizeToCategory(e, obj)
    assert(generalized.isInstanceOf[Foreach])
    val foreach = generalized.asInstanceOf[Foreach]
    val id = foreach.id
    generalized should be (Foreach(obj.category, id, 
      If(And(Select(Variable(id), "visible"), Select(ObjectLiteral(game.block1), "visible")), 
        Assign(List((Variable(id), "x")), Plus(Select(Variable(id), "x"), Select(ObjectLiteral(game.block1), "y"))),
        NOP)
      )
    )
  }
  
  it should "not change an expression during generalization if it doesn't contain the given object" in {
    val obj = game.ball1
    val e = If(game.block1.visible, game.block1.y += 2)
    val generalized = generalizeToCategory(e, obj)
    generalized should be (e)
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
  
  "collectFirst in TreeOps" should "compute correct ancestors" in {
    import ch.epfl.lara.synthesis.kingpong.objects._
	import scala.collection.mutable.ListBuffer
	import ch.epfl.lara.synthesis.kingpong.expression._
	import Trees._
	import Types._
	import Extractors._
	val c = IntegerLiteral(1)
	val d = IntegerLiteral(2)
	var tree: Expr = Plus(Minus(Times(d, c), d), Times(d, d))
	TreeOps.getAncestors(tree, c) should equal(
	List(Plus(Minus(Times(IntegerLiteral(2),IntegerLiteral(1)),IntegerLiteral(2)),Times(IntegerLiteral(2),IntegerLiteral(2))), Minus(Times(IntegerLiteral(2),IntegerLiteral(1)),IntegerLiteral(2)), Times(IntegerLiteral(2),IntegerLiteral(1)), IntegerLiteral(1)))
	
	tree = Block(List(c))
	
	TreeOps.getAncestors(tree, c) should equal(
	List(Block(List(IntegerLiteral(1))), IntegerLiteral(1)))
	
	tree = If(d, Block(List(c)), NOP)
	
	TreeOps.getAncestors(tree, c) should equal(
	List(If(IntegerLiteral(2),Block(List(IntegerLiteral(1))),NOP), Block(List(IntegerLiteral(1))), IntegerLiteral(1))
	)
  }
}