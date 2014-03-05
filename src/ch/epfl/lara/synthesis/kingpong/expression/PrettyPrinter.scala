package ch.epfl.lara.synthesis.kingpong.expression

import Trees._
import ch.epfl.lara.synthesis.kingpong.objects.GameObject
import ch.epfl.lara.synthesis.kingpong.rules.Rules
import ch.epfl.lara.synthesis.kingpong.objects._
import android.text.style.CharacterStyle
import android.text.SpannableStringBuilder
import android.text.Spannable
import android.text.style.StyleSpan
import android.graphics.Typeface
import android.text.SpannableString
import android.text.Spanned
import android.text.style.ForegroundColorSpan

object PrettyPrinter extends PrettyPrinterTypical {
  override val FOR_SYMBOL = "for"
  override val IN_SYMBOL = "in"
  override val FINGER_MOVE_SYMBOL = "movedOn "
  override val FINGER_DOWN_SYMBOL = "downOn "
  override val FINGER_UP_SYMBOL = "upOn "
  override val COLLIDES_SYMBOL = "collides"
  override val IF_SYMBOL = "if"
}

trait PrettyPrinterTypical {
  import Rules._

  final val NO_INDENT = ""
  final val INDENT = "  "
  final val LF = "\n"
  val FOR_SYMBOL = "\u2200"
  val IN_SYMBOL = "\u2208"
  val TIMES_SYMBOL = "*" // "\u2219"
  val AND_SYMBOL = "\u2227"
  val OR_SYMBOL = "\u2228"
  val LESSEQ_SYMBOL = "\u2264"
  val GREATEREQ_SYMBOL = "\u2265"
  val COLLIDES_SYMBOL = "\u2605"
  val NOT_SYMBOL = "\u00AC"
  val FINGER_MOVE_SYMBOL = "\u21BA"
  val FINGER_DOWN_SYMBOL = "\u21E9"
  val FINGER_UP_SYMBOL = "\u21E7"
  val ARROW_FUNC_SYMBOL = "\u21D2"
  val IF_SYMBOL = "if"
  lazy val LANGUAGE_SYMBOLS = List(IF_SYMBOL, FOR_SYMBOL, IN_SYMBOL, COLLIDES_SYMBOL, FINGER_DOWN_SYMBOL, FINGER_MOVE_SYMBOL, FINGER_UP_SYMBOL)
  
   def setSpanOnKeywords(text: CharSequence, keywords: Seq[String], cs: (()=>CharacterStyle)*): CharSequence = {
      // Start and end refer to the points where the span will apply
      val stringText = text.toString
      val stringWOtext = stringText
      val thetext:Spannable = new SpannableString(stringWOtext)
  
      val keywordpattern = ("\\b("+keywords.reduceLeft(_ + "|" + _)+")\\b").r
      for(m <- keywordpattern findAllMatchIn stringWOtext) {
        for (c <- cs) thetext.setSpan(c(), m.start, m.end, Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
      }
      thetext
  }
    
  
  /**
   * Gives certain styles to keywords
   */
  implicit class ToSpannableString(result: CharSequence) {
    def makeStyle: CharSequence = {
      val result1 = setSpanOnKeywords(result, LANGUAGE_SYMBOLS, () => new StyleSpan(Typeface.BOLD), () => new ForegroundColorSpan(0xFF950055))
      result1
    }
  }
  
  /**
   * Prints a set of RuleIterator
   */
  def print(s: Iterable[Stat]): CharSequence = {
    val seq = s map (print(NO_INDENT, _))
    if(seq.isEmpty) "" else seq reduceLeft { (l: CharSequence, r: CharSequence) => l + LF +  r }
  }.makeStyle
  
  /**
   * Prints a RuleIterator
   */
  /*def print(r: RuleIterator): CharSequence = r match {
    case NoCategory(rule) => print(NO_INDENT, rule)
    case Foreach1(cat, name, rule) => s"$FOR_SYMBOL $name $IN_SYMBOL ${cat.name}:$LF" + print(NO_INDENT, rule)
    case Foreach2(cat1, cat2, name1, name2, rule) =>
      s"$FOR_SYMBOL $name1 $IN_SYMBOL ${cat1.name}:$LF"+ 
      s"$FOR_SYMBOL $name2 $IN_SYMBOL ${cat2.name}:$LF"+ print(NO_INDENT, rule)
    
  }*/
  /**
   * Prints a rule
   */
  /*def print(r: Rule): CharSequence = r match {
    case Whenever(cond, action) => s"$IF_SYMBOL " + print(NO_INDENT, cond) + ":" + LF + print(INDENT, action)
  }*/
  
  //def print(s: Tree, indent: String = ""): CharSequence = {
  //  val result = print(indent, s)
  //}

  private def print(indent: String, s: Tree): CharSequence = s match {
    case Delete(null, ref) => indent + s"Delete(${ref.name})"
    case Delete(name, ref) => indent + s"Delete($name)"
    case VecExpr(l) => indent + l.map(print(NO_INDENT, _)).toString.substring(4)
    case e:LeftRightBinding => val op = e match {
        case _:Plus => "+"
        case _:Minus => "-"
        case _:Times => TIMES_SYMBOL
        case _:Div => "/"
        case _:Mod => "%"
        case _:And => AND_SYMBOL
        case _:Or => OR_SYMBOL
        case _:Equals => "="
        case _:LessThan => "<"
        case _:LessEq => LESSEQ_SYMBOL
        case _:GreaterThan => ">"
        case _:GreaterEq => GREATEREQ_SYMBOL
        case _:Collision => COLLIDES_SYMBOL
      }
      print(indent, e.lhs) + " " + op + " " + print(NO_INDENT, e.rhs) // TODO : parenthesis
    case Assign(List(e), rhs: Expr) => indent + print(NO_INDENT, e) + "' = " + print(NO_INDENT, rhs)
    case Assign(prop, rhs: Expr) => indent + prop.map(print(NO_INDENT, _)).toString.substring(4) + "' = " + print(NO_INDENT, rhs)
    case Reset(prop: MaybeAssignable) => val p = print(NO_INDENT, prop)
      indent + p + "' = init_" + p 
    case Block(stats: Seq[Stat]) =>
      stats.toList match {
        case Nil => indent + "{}"
        case a::Nil => print(indent, a)
        case l => (l map (print(indent+INDENT, _)) reduceLeft (_ + LF + _))
      }
    case If(cond: Expr, s1: Stat, s2) => 
      val prem = s"$IF_SYMBOL " + print(NO_INDENT, cond) + ":" + LF
      val middle = print(indent + INDENT, s1)
      val end = if(s2 != None) indent + "else:" + LF + print(indent + INDENT, s2.get) else ""
      prem + middle + end
    case Copy(name: String, o: GameObjectRef, b: Block) =>
      indent + name + " = " + o.obj.name.get + ".copy" + LF + print(indent, b)
    case NOP => "NOP"
    case IfFunc(cond: Expr, s1: Expr, s2: Expr) =>
      indent + s"$IF_SYMBOL("+print(NO_INDENT, cond)+") " + print(NO_INDENT, s1) + " else " + print(NO_INDENT, s2)
      
    case IntegerLiteral(value: Int) => indent + value.toString
    case FloatLiteral(value: Float) => indent + value.toString
    case StringLiteral(value: String) => indent + "\"" + value + "\""
    case BooleanLiteral(value: Boolean) => indent + value.toString
    case Vec2Literal(x: Float, y: Float) => indent + "Vec2("+x+","+y+")"
    case ObjectLiteral(o) => indent + o.name.get
    case UnitLiteral => indent + "()"
    case Val(name: String) => indent + name
    case Not(expr: Expr)  => indent + NOT_SYMBOL + print(NO_INDENT, expr)
    case On(cond: Expr)   => indent + "on " + print(NO_INDENT, cond)
    case Once(cond: Expr) => indent + "once " + print(NO_INDENT, cond)
    case GameObjectRef(ObjectLiteral(obj)) => indent + obj.name.get
    case GameObjectRef(ref) => indent + print(NO_INDENT, ref)
    case FingerMoveOver(o: GameObjectRef) => indent + FINGER_MOVE_SYMBOL + print(NO_INDENT, o)
    case FingerDownOver(o: GameObjectRef) => indent + FINGER_DOWN_SYMBOL + print(NO_INDENT, o)
    case FingerUpOver(o: GameObjectRef) => indent + FINGER_UP_SYMBOL + print(NO_INDENT, o)
    case FingerCoordX1 => indent + "x1"
    case FingerCoordY1 => indent + "y1"
    case FingerCoordX2 => indent + "x2"
    case FingerCoordY2 => indent + "y2"
    case PropertyIndirect(indirectObject, p) => indent + print(NO_INDENT, indirectObject) + "." + p
    case PropertyRef(p) => p.name match {
      case "value" => indent + p.parent.name.get  // Special case: For the value (like Int or Boolean, we do not specify the "value" property)
      case "velocity" => indent + p.parent.name.get + ".velocity"
      case _ => indent + p.parent.name.get + "." + p.name
    }
    case Choose(prop, expr) => indent + "choose(" + print(NO_INDENT, prop) + s" $ARROW_FUNC_SYMBOL " + print(NO_INDENT, expr) + ")"
  }
}

