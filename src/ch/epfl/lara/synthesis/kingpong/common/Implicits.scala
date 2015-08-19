package ch.epfl.lara.synthesis.kingpong.common

// Remove implicit warnings
import language.implicitConversions
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._
import ch.epfl.lara.synthesis.kingpong.expression._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import net.londatiga.android.QuickAction

object Implicits {
  implicit def actionConvert(f: (QuickAction, Int, Int) => Any): QuickAction.OnActionItemClickListener = {
    new QuickAction.OnActionItemClickListener() {
        override def onItemClick(quickAction: QuickAction, pos: Int, actionId: Int) {
          f(quickAction, pos, actionId)
        }
    }
  }
  
  implicit class toDelimiter(s: String) {
    def andThen(other: String) = StringDelimiter(s, other)
    def followedBy(other: String) = StringDelimiter(s, other)
  }

  implicit object IntegerIsPongType extends PongType[Int] {
    def getPongType = TInt
    def toScalaValue(e: Expr) = e match {
      case FloatLiteral(f) if f.isWhole => f.toInt
      case IntegerLiteral(i) => i
      case StringLiteral("") => 0
      case StringLiteral(s) => s.toInt
      case _ => throw InterpreterException(s"The value $e is incompatible with Int.")
    }
    def toExpr(v: Int) = IntegerLiteral(v)
    
    def clone(v: Int) = v
  }
  
  // Type only for internal use. Should not be used in Trees.
  /*implicit object LongIsPongType extends PongType[Long] {
    def getPongType = TInt
    def toScalaValue(e: Expr) = e match {
      case FloatLiteral(f) if f.isWhole => f.toLong
      case IntegerLiteral(i) => i.toLong
      case _ => throw InterpreterException(s"The value $e is incompatible with Long.")
    }
    def toExpr(v: Long) = IntegerLiteral(v.toInt)
    def clone(v: Long) = v
  }*/

  implicit object FloatIsPongType extends PongType[Float] {
    def getPongType = TFloat
    def toScalaValue(e: Expr) = e match {
      case FloatLiteral(f) => f
      case IntegerLiteral(i)   => i.toFloat
      case StringLiteral("") => 0
      case StringLiteral(s) => s.toFloat
      case BooleanLiteral(b) => if(b) 1f else 0f
      case _ => throw InterpreterException(s"The value $e is incompatible with Float.")
    }
    def toExpr(v: Float) = FloatLiteral(v)
    
    def clone(v: Float) = v
  }

  implicit object BooleanIsPongType extends PongType[Boolean] {
    def getPongType = TBoolean
    def toScalaValue(e: Expr) = e match {
      case BooleanLiteral(b) => b
      case IntegerLiteral(s) => s != 0
      case FloatLiteral(s) => s != 0
      case StringLiteral(s) => s != ""
      case _ => throw InterpreterException(s"The value $e is incompatible with Boolean.")
    }
    def toExpr(v: Boolean) = BooleanLiteral(v)

    def clone(v: Boolean) = v
  }

  implicit object StringIsPongType extends PongType[String] {
    def getPongType = TBoolean
    def toScalaValue(e: Expr) = e match {
      case StringLiteral(s) => s
      case IntegerLiteral(s) => s.toString
      case FloatLiteral(s) => s.toString
      case BooleanLiteral(s) => s.toString
      case _ => throw InterpreterException(s"The value $e is incompatible with String.")
    }
    def toExpr(v: String) = StringLiteral(v)

    def clone(v: String) = v
  }

  implicit object Vec2IsPongType extends PongType[Vec2] {
    def getPongType = TVec2
    def toScalaValue(e: Expr) = e match {
      case Tuple(Seq(FloatLiteral(x), FloatLiteral(y))) => Vec2(x, y)
      case Tuple(Seq(IntegerLiteral(x), IntegerLiteral(y))) => Vec2(x, y)
      case Tuple(Seq(IntegerLiteral(x), FloatLiteral(y))) => Vec2(x, y)
      case Tuple(Seq(FloatLiteral(x), IntegerLiteral(y))) => Vec2(x, y)
      case _ => throw InterpreterException(s"The value $e is incompatible with Vec2.")
    }
    def toExpr(v: Vec2) = Tuple(Seq(FloatLiteral(v.x), FloatLiteral(v.y)))

    def clone(v: Vec2) = v.clone
  }
  
  implicit object GameObjectIsPongType extends PongType[GameObject] {
    def getPongType = TObject
    def toScalaValue(e: Expr) = e match {
      case ObjectLiteral(o) => o
      case _ => throw InterpreterException(s"The value $e is incompatible with GameObject.")
    }
    def toExpr(o: GameObject) = ObjectLiteral(o)

    def clone(v: GameObject) = v
  }
  
    
  /** Extracts the first float of a string */
  implicit class RichString(s: String) {
    val floatRegexp = """(-?\d+(?:\.\d*)?(?:(?:e|E)-?\d+)?|-?\.(?:\d+)(?:(?:e|E)-?\d+)?)""".r
    val intRegexp = """(\d+)""".r
    val int2Regexp = """(\d+)(?=[^\d]*$)""".r
    def floatOrElse(d: Float) = try { (floatRegexp findFirstIn s) match { case Some(r) => r.toFloat case None => d}} catch { case _: Exception => d }
    def intOrElse(d: Int) = try { (intRegexp findFirstIn s) match { case Some(r) => r.toInt case None => d}} catch { case _: Exception => d }
    def int2OrElse(d: Int) = try { (int2Regexp findFirstIn s) match { case Some(r) => r.toInt case None => d}} catch { case _: Exception => d }
  }
}