package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.objects.GameObject
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
  import Trees._
  import Extractors._
  
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
  def print(s: Iterable[Expr]): CharSequence = {
    val seq = s map (print(NO_INDENT, _))
    if(seq.isEmpty) "" else seq reduceLeft { (l: CharSequence, r: CharSequence) => l + LF +  r }
  }.makeStyle
  

  private def print(indent: String, s: Tree): CharSequence = s match {
    case Delete(obj) => indent + "Delete(" + print(NO_INDENT, obj) + ")"
    case Tuple(l) => indent + l.map(print(NO_INDENT, _)).toString.substring(4)
    case b @ BinaryOperator(lhs, rhs, _)=> val op = b match {
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
        case _:Contains => "contains"
      }
      print(indent, lhs) + " " + op + " " + print(NO_INDENT, rhs) // TODO : parenthesis
    case Assign(List((e, prop)), rhs) => indent + print(NO_INDENT, e) + "." + prop + "' = " + print(NO_INDENT, rhs)
    case Assign(props, rhs) => indent + props.map{case (e, prop) => print(NO_INDENT, e) + "." + prop}.toString.substring(4) + "' = " + print(NO_INDENT, rhs)
    case Block(exprs) =>
      exprs.toList match {
        case Nil => indent + "{}"
        case a::Nil => print(indent, a)
        case l => (l map (print(indent+INDENT, _)) reduceLeft (_ + LF + _))
      }
    case If(cond, s1, s2) => 
      val prem = s"$IF_SYMBOL " + print(NO_INDENT, cond) + ":" + LF
      val middle = print(indent + INDENT, s1)
      val end = if(s2 != NOP) indent + "else:" + LF + print(indent + INDENT, s2) else ""
      prem + middle + end
    case Copy(name, obj, b) =>
      indent + name + " = " + obj + ".copy" + LF + print(indent, b)
    case NOP => "NOP"
    
    case Choose(vars, pred) => 
      val varsString = vars.map(print(NO_INDENT, _)).mkString("(", ",", ")")
      indent + "choose(" + varsString + s" $ARROW_FUNC_SYMBOL " + print(NO_INDENT, pred) + ")"
    
    case IntegerLiteral(value: Int) => indent + value.toString
    case FloatLiteral(value: Float) => indent + value.toString
    case StringLiteral(value: String) => indent + "\"" + value + "\""
    case BooleanLiteral(value: Boolean) => indent + value.toString
    case ObjectLiteral(o) => indent + o.name.get
    case UnitLiteral => indent + "()"
    case Variable(id) => indent + id
    
    case Not(expr: Expr)  => indent + NOT_SYMBOL + print(NO_INDENT, expr)
    
    case Row(expr) => indent + print(NO_INDENT, expr) + ".row"
    case Column(expr) => indent + print(NO_INDENT, expr) + ".column"
    
    case FingerMoveOver(o) => indent + FINGER_MOVE_SYMBOL + print(NO_INDENT, o)
    case FingerDownOver(o) => indent + FINGER_DOWN_SYMBOL + print(NO_INDENT, o)
    case FingerUpOver(o) => indent + FINGER_UP_SYMBOL + print(NO_INDENT, o)
    case FingerCoordX1 => indent + "x1"
    case FingerCoordY1 => indent + "y1"
    case FingerCoordX2 => indent + "x2"
    case FingerCoordY2 => indent + "y2"
  }
}

