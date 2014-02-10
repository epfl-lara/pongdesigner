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
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

class DisambiguatorSuite extends FlatSpec with ShouldMatchers  {
  implicit val interface = (p: Property[_]) => Disambiguator.MergeMode.SequentialMerge
  
  val game = new Game() {
    val world = new PhysicalWorld(Vec2(0, 0f))
    val borders = Category("Borders")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
    val blocks = Category("Blocks")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
    val paddles = Category("Paddles")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
    val balls = Category("Balls")(friction=0, restitution=1, fixedRotation=true, tpe=BodyType.STATIC)
    val Ball1 = circle(balls)(name="Ball1", x=2.5f, y=2f, radius = 0.25, velocity=Vec2(0, -5.0f))
    val Border2 = intbox(borders)(name="Border2", x=0f, y=0f, width=1, height=2)
    val Border3 = intbox(borders)(name="Border3", x=1f, y=0f)
    val paddle1 = intbox(paddles)(name="paddle1", x=0f, y=0f)
    val expr1 = (paddle1 toRightOf Border2) && (paddle1 toLeftOf Border3)
    val rule1 = foreach(blocks)("o"){
    whenever(paddle1("y") < obj("o")("y"))(
      obj("o")("y") := 0, 
      obj("o")("velocity") := Vec2(0, 0)
    )
    }
    val rule2 = foreach(paddles)("paddle"){
       whenever(true) (
         List(obj("paddle")("x"), obj("paddle")("width")) := Choose(List(obj("paddle")("x"), obj("paddle")("width")), obj("paddle")("left") =:= Border2("right") && obj("paddle")("right") =:= Ball1("x"))
      // Should replace by obj("ball")("y") - obj("ball")("radius") and solved
    )
    }
  }
  
  def equalCode(right: Stat) = new Matcher[Stat] {
    def apply(left: Stat) = {
      MatchResult(
          right == left,
          PrettyPrinterExtended.print(List(left)).c + "\ndid not equal\n" +
          PrettyPrinterExtended.print(List(right)).c,
          ""
      )
    }
  }

  
  "ModifySequential" should "work on simple trees" in {
    import game._
    val x = game.paddle1("x").asInstanceOf[MaybeAssignable]
    val x1 = game.paddle1("x1").asInstanceOf[MaybeAssignable] // Ephemeral
    val x2 = game.paddle1("x2").asInstanceOf[MaybeAssignable] // Ephemeral
    val x3 = game.paddle1("x3").asInstanceOf[MaybeAssignable] // Ephemeral
    Disambiguator.modifyCodeSequential(
        Block(x := x + 1, x := x + 3), x.getProperty.get
    ) should equalCode (
        Block(x2 := x, x1 := x2 + 1, x := x1 + 3)
    )
    Disambiguator.modifyCodeSequential(
        If(Val("x"), Block(x := x - 1, x := x + 1, x := x + 3), x := x + 5), x.getProperty.get
    ) should equalCode (
        (x3 := x) :: If(Val("x"), Block(x2 := x3 - 1, x1 := x2 + 1, x := x1 + 3), Block(x1 := x3, x := x1 + 5))
    )
    Disambiguator.modifyCodeSequential(
        If(Val("x"), x := x + 5, Block(x := x - 1, x := x + 1, x := x + 3)), x.getProperty.get
    ) should equalCode (
        (x3 := x) :: If(Val("x"), Block(x1 := x3, x := x1 + 5), Block(x2 := x3 - 1, x1 := x2 + 1, x := x1 + 3))
    )
  }
  
  
  "Disambiguator" should "find duplicates and assignments" in {
    import game._
    val x = game.paddle1("x").asInstanceOf[MaybeAssignable]
    val x1 = game.paddle1("x1").asInstanceOf[MaybeAssignable] // Ephemeral
    Disambiguator.findDuplicates(If(Val("e"), x := x + 10, x := x + 1)) should equal (
                  Set(x.getProperty.get), Set())
    Disambiguator.findDuplicates(Block(Seq(x := x + 10, x := x + 1))) should equal (
                  Set(x.getProperty.get), Set(x.getProperty.get))
    Disambiguator.findDuplicates(Block(Seq(If(Val("e"), x := x + 10, x := x + 15), x := x * 2))) should equal (
                  Set(x.getProperty.get), Set(x.getProperty.get))
  }
  
  "Disambiguator" should "solve simple problems" in {
    import game._
    val x = game.paddle1("x").asInstanceOf[MaybeAssignable]
    val x1 = game.paddle1("x1").asInstanceOf[MaybeAssignable] // Ephemeral
    val x2 = game.paddle1("x2").asInstanceOf[MaybeAssignable] // Ephemeral
    Disambiguator(If(Val("e"), Block(x := x + 10), Block(x := x + 1))) should equalCode (
                  If(Val("e"), Block(x := x + 10), Block(x := x + 1))
    )
    Disambiguator(Block(x := x + 10, x := x + 1)) should equalCode (
                  Block(x2 := x, x1 := x2 + 10, x := x1 + 1)
    )
    Disambiguator(Block(If(Val("e"), x := x + 10, x := x + 15), x := x * 2)) should equalCode (
                  Block(x2 := x, If(Val("e"), x1 := x2 + 10, x1 := x2 + 15), x := x1 * 2)
    )
  }
  "Disambiguator" should "solve complex problems" in {
    import game._
    val x = game.paddle1("x").asInstanceOf[MaybeAssignable]
    val x1 = game.paddle1("x1").asInstanceOf[MaybeAssignable] // Ephemeral
    val x2 = game.paddle1("x2").asInstanceOf[MaybeAssignable] // Ephemeral
    val x3 = game.paddle1("x3").asInstanceOf[MaybeAssignable] // Ephemeral
    Disambiguator(Block(If(Val("e"), Block(x := x + 10, x := x + 3), x := x + 15), x := x * 2)) should equalCode (
                  Block(x3 := x, If(Val("e"), Block(x2 := x3 + 10, x1 := x2 + 3), Block(x2 := x3, x1 := x2 + 15)), x := x1 * 2)
    )
  }
  "Disambiguator" should "solve Foreach problems" in {
    import game._
    val x = game.Border2("x").asInstanceOf[MaybeAssignable]
    val x1 = game.Border2("x1").asInstanceOf[MaybeAssignable] // Ephemeral
    val x2 = game.Border2("x2").asInstanceOf[MaybeAssignable] // Ephemeral
    val x3 = game.Border2("x3").asInstanceOf[MaybeAssignable] // Ephemeral
    Disambiguator(Block(Foreach1(borders, "b", If(Val("e"), obj("b")("x") := obj("b")("x") + 1, obj("b")("x") := obj("b")("x") - 1)), x := x + 10)) should equalCode (
                  Block(x2 := x,
                        Foreach1(borders, "b",
                            If(Val("e"),
                                If(obj("b") =:= Border2,
                                    x1 := x2 + 1,
                                    obj("b")("x") := obj("b")("x") + 1),
                                If(obj("b") =:= Border2,
                                    x1 := x2 - 1,
                                    obj("b")("x") := obj("b")("x") - 1)
                            )
                        ),
                        x := x1 + 10)
    )
  }
  
}