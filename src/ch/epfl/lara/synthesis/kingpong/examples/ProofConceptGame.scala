package ch.epfl.lara.synthesis.kingpong.examples

import org.jbox2d.dynamics.BodyType
import ch.epfl.lara.synthesis.kingpong.{Game, GameCreationContext}
import ch.epfl.lara.synthesis.kingpong.PhysicalWorld
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._
import android.graphics.Color
import ch.epfl.lara.synthesis.kingpong.R

class ProofConceptGame(g: GameCreationContext, lang: String = "en") extends Game {
  val world = new PhysicalWorld(Vec2(0, 3.5f))
  
  object t {
    val instructions = "Instructions"
    val mushroom = "mushroom" 
    val collisions = "Collisions"
    val touched = "Touched"
    val stat = "stat"
    val arrobj = "arrobj"
    val MyArray = "MyArray"
    val cat_falling = "cat_falling"
    val drop = "Drop"
  }

  outputs = Map("en" -> Map(
      t.cat_falling -> "Falling objects",
      t.stat -> "Static objects",
      t.instructions -> "Touch the falling drops!",
      t.collisions -> "Collisions"
      ),
    "fr" -> Map(
      t.cat_falling -> "Objets tombants",
      t.stat -> "Objets fixes",
      t.arrobj -> "Tableaux",
      t.mushroom -> "Champignon",
      t.MyArray -> "Tableau",
      t.instructions -> "Touchez les gouttes qui tombent !",
      t.collisions -> "Tombées ",
      t.touched -> "Touchées ",
      t.drop -> "Goutte"
      ))
  fallback = Map("fr" -> "en", "en" -> "fr")
  language = lang

  val array_objects = Category(Translate("cat_falling"))()
  //val moving_objects2 = Category("Moving_objects2")()

  val color_drop = g.bitmap(R.drawable.image_drop)
  val color_snowflake = g.bitmap(R.drawable.image_snowflake)
  val rect1 = rectangle(array_objects)(name=Str(t.drop)+"1", x=2, y= -2, width = 1, height = 1, fixedRotation = true, color = color_drop)
  val rect2 = rectangle(array_objects)(name=Str(t.drop)+"2", x=1, y= -3, width = 1, height = 1, fixedRotation = true, color = color_drop)
  val rect3 = rectangle(array_objects)(name=Str(t.drop)+"3", x=0, y= -4, width = 1, height = 1, fixedRotation = true, color = color_drop)
  val rect4 = rectangle(array_objects)(name=Str(t.drop)+"4", x= -1, y= -5, width = 1, height = 1, fixedRotation = true, color = color_drop)
  val rect5 = rectangle(array_objects)(name=Str(t.drop)+"5", x= -2, y= -6, width = 1, height = 1, fixedRotation = true, color = color_drop)
  
  val cat2 = Category(Translate(t.stat))()
  val catArray = Category(Translate(t.arrobj))()

  val base = rectangle(cat2)("Base", 0, 8, width = 8, height = 0.5, tpe = BodyType.STATIC, color = Color.rgb(0, 0x80, 0))
  val base2 = rectangle(cat2)("Base2", 0, -7, width = 20, height = 0.5, tpe = BodyType.STATIC, color = Color.BLUE)
  val base3 = rectangle(cat2)("Base2", 0, -10, width = 20, height = 0.5, tpe = BodyType.STATIC, color = Color.BLUE)
  val mushroom = rectangle(cat2)(Translate(t.mushroom), 0, 1, width = 1, height = 1, color = g.bitmap(R.drawable.image_mushroom), fixedRotation = true)

  val arr = array(catArray)(Translate(t.MyArray), 0, 0, 5, 1)
  
  val scores = Category("scores")()
  val collisions = intbox(scores)(Str(t.collisions), -10, 2, width=8, height=1, value = 0)
  
  val touches = intbox(scores)(Str(t.touched), -10, 3, width=8, height=1, value = 0)
  
  val label = stringbox(scores)(t.instructions, -10, -8.5, Translate(t.instructions))
  
  //val soundtts = soundTTS(cat2)(name="soundtts",x= -10.5f, y=4f, time= 1, language="en", text="Welcome to Pong Designer, made by Lara at E P F L !")
  //val soundtts2 = soundTTS(cat2)(name="soundtts2",x= -10.5f, y=5f, time= 150, language="fr", text="Bienvenue à Pongue Dizaïneur réalisé par Lara, à l'eupéheffelle !")

  val cell = arr.cells(4)(0)
  
  // Rule allowing the objects to continue to fall down. The score.value is absolute and needs to be corrected relatively.
  // e.g. by selecting the fix button, select the score change, and then by selecting the right statement.
  val collisionsvaluestmp = collisions.value.copyEphemeral("value_acc")
  val r1 = Block(
    collisionsvaluestmp := collisions.value,
    foreach(array_objects) { b =>
    whenever(Collision(b, base))(
        b.y := -6,
        (collisionsvaluestmp := 1) orElse (collisionsvaluestmp := collisionsvaluestmp + 1),
        b.velocity := Vec2(0, 0)
    )},
    collisions.value := collisionsvaluestmp)
  
  // Rule to be corrected by providing a new example (e.g. selecting a collision, augmenting the score to 2 and accepting.)
  val touchesvaluestmp = touches.value.copyEphemeral("value_acc")
  val r2 = Block(
    touchesvaluestmp := touches.value,
    foreach(array_objects) { b =>
    whenever(isFingerDownOver(b) && b.color =!= color_snowflake)(
        (touchesvaluestmp := 1) orElse (touchesvaluestmp := touchesvaluestmp + 1),
        b.color := color_snowflake
    )
  },
    touches.value := touchesvaluestmp)
  
  // Rule to generalize so that all objects snap to the array.
  val r3 = foreach(cell.category) { cell =>
    whenever(Contains(cell, rect1))(
      (rect1.x, rect1.y) := (cell.x, cell.y),
      rect1.velocity := Vec2(0, 0),
      rect1.color := Color.BLUE
    )
  }
  
  // A growing mushroom if it hits the floor. Then pops after growth.
  val r4 = whenever(mushroom.top < 0)(
    mushroom.height := 1,
    mushroom.width := 1
  ) otherwise whenever(Colliding(mushroom, base) && mushroom.bottom > base.top - 1 )(
    let("t", mushroom.top) { t =>
      let("yh", choose(mushroom.y, mushroom.height)(mushroom.bottom =:= base.top && mushroom.top =:= t - 0.05)){ yh => Seq(
        mushroom.y := yh._1,
        mushroom.height := yh._2,
        mushroom.width := integerLiteralOne / mushroom.height,
        mushroom.velocity := Vec2(0, -0.01f))
      }
    }
  )
    
  
  register(r1)
  register(r2)
  register(r3)
  register(r4)
}
