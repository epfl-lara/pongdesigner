package ch.epfl.lara.synthesis.kingpong.expression

import Trees._
import ch.epfl.lara.synthesis.kingpong.objects.GameObject
import ch.epfl.lara.synthesis.kingpong.rules.Rules
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.common.StringDelimiter
import ch.epfl.lara.synthesis.kingpong.common.Implicits._

object PrettyPrinterExtended extends PrettyPrinterExtendedTypical {
  override val FOR_SYMBOL = "for"
  override val IN_SYMBOL = "in"
  override val FINGER_MOVE_SYMBOL = "movedOn "
  override val FINGER_DOWN_SYMBOL = "downOn "
  override val FINGER_UP_SYMBOL = "upOn "
  override val COLLIDES_SYMBOL = "collides"
  override val IF_SYMBOL = "if"
}

trait PrettyPrinterExtendedTypical {
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
  
  
  import Rules._
  
  object Mappings {
    def apply(): Mappings = Mappings(Map[Category, List[(Int, Int)]](), Map[Int, Tree](), Map[Property[_], List[(Int, Int)]](), Map[Int, String]())
  }
  
  /**
   * Recording of different mappings to retrieve the objects and the code.
   * mObjects    : Mapping from objects to the positions of their appearing in the code
   * mPos        : Mapping from each position of the string to the corresponding tree
   * mProperties : Mapping from each property to a set of positions in the code
   * mComment    : Mapping from each position of the code to the corresponding comment
   */
  case class Mappings(mObjects: Map[Category, List[(Int, Int)]], mPos: Map[Int, Tree], mProperties: Map[Property[_], List[(Int, Int)]], mComment: Map[Int, String]) {
    def add(obj: GameObject, start: Int, end: Int): Mappings = {
      add(obj.category, start, end)
    }
    def add(obj: Category, start: Int, end: Int): Mappings = {
      this.copy(mObjects=mObjects + (obj -> ((start, end)::mObjects.getOrElse(obj, Nil))))
    }
    def add(obj: Property[_], start: Int, end: Int): Mappings = {
      this.copy(mProperties = mProperties + (obj -> ((start, end)::mProperties.getOrElse(obj, Nil))))
    }
    def add(s: Tree, start: Int, end: Int): Mappings = {
      new Mappings(addIfCategory(s, start, end, mObjects), addPositions(s, start, end, mPos), addIfProperty(s, start, end, mProperties), mComment)
    }
    def add(comment: String, start: Int, end: Int): Mappings = {
      this.copy(mComment=addComment(comment, start, end, mComment))
    }
    def addIfCategory(s: Tree, start: Int, end: Int, mm: Map[Category, List[(Int, Int)]]): Map[Category, List[(Int, Int)]] = {
      s match {
        case GameObjectRef(_, o) => mm + (o.category -> ((start, end)::mm.getOrElse(o.category, List())))
        case PropertyRef(prop) => mm + (prop.parent.category -> ((start, end)::mm.getOrElse(prop.parent.category, List())))
        case PropertyIndirect(ref, obj, prop) if obj != null=> mm + (obj.category -> ((start, end)::mm.getOrElse(obj.category, List())))
        case _ => mm
      }
    }
    def addPositions(s: Tree, start: Int, end: Int, mm: Map[Int, Tree]): Map[Int, Tree] = {
      (mm /: (start to end)) { case (map, i) =>
        map.get(i) match {
          case Some(t) => map
          case None => map + (i -> s)
        }
      }
    }
    def addComment(comment: String, start: Int, end: Int, mm: Map[Int, String]): Map[Int, String] = {
      (mm /: (start to end)) { case (map, i) =>
        map.get(i) match {
          case Some(t) => map
          case None => map + (i -> comment)
        }
      }
    }
    def addIfProperty(s: Tree, start: Int, end: Int, mm: Map[Property[_], List[(Int, Int)]]): Map[Property[_], List[(Int, Int)]] = {
      s match {
        case PropertyRef(prop) => mm + (prop -> ((start, end)::mm.getOrElse(prop, List())))
        case _ => mm
      }
    }
  }
  
  /**
   * Class holding the tree being rendered, with the mapping.
   * c   : Current string
   * size: size of the current string
   * map : Different mappings from objects to string position
   * mOpen: Mapping from any object to its starting position, for temporary purposes
   * mCommentOpen: stack of comments and their starting position
   */
  object StringMaker { def apply(): StringMaker = StringMaker(new StringBuilder, 0, Mappings(), Map[Any, Int](), Nil)}
  case class StringMaker(c: StringBuilder, size: Int, map: Mappings, mOpen: Map[Any, Int], mCommentOpen: List[(String, Int)]) {
    def +(other: String): StringMaker = { // Simple add
      if(other == "" || other == null) {
        this
      } else {
        StringMaker(c append other, size + other.size, map, mOpen, mCommentOpen)
      }
    }
    def +!(other: String)(implicit s: Tree): StringMaker = {
      StringMaker(c append other, size + other.size, map.add(s, size, size + other.size), mOpen, mCommentOpen)
    }
    /*def +!(other: String, s: GameObject): StringMaker = {
      StringMaker(c append other, size + other.size, map.add(s, size, size + other.size), mOpen, mCommentOpen)
    }*/
    def +!(other: String, s: Category): StringMaker = {
      StringMaker(c append other, size + other.size, map.add(s, size, size + other.size), mOpen, mCommentOpen)
    }
    def +~(other: Tree): StringMaker = { // Don't care about this expression in particular. But sub expressions will be recorded.
      print(this, NO_INDENT, other)
    }
    def +(other: Category): StringMaker = { // Don't care about this expression in particular. But sub expressions will be recorded.
      +!(other.name, other)
    }
    def +(other: Tree, newIndentation: String = NO_INDENT): StringMaker = { // Care about this new expression by storing its position.
      val result = print(this, newIndentation, other)
      StringMaker(result.c, result.size, result.map.add(other, size, result.size), result.mOpen, result.mCommentOpen)
    }
    def open(implicit start: Tree): StringMaker = {
      StringMaker(c, size, map, mOpen + (start -> size), mCommentOpen)
    }
    def open[T](implicit start: Property[T]): StringMaker = {
      StringMaker(c, size, map, mOpen + (start -> size), mCommentOpen)
    }
    def open(comment: String): StringMaker = {
      StringMaker(c, size, map, mOpen, (comment -> size)::mCommentOpen)
    }
    def +<(other: Tree)(implicit start: Tree): StringMaker = {
      (this open start) + other
    }
    def +<(other: String)(implicit start: Tree): StringMaker = {
      (this open start) + other
    }
    def +<(other: String, start: =>Tree): StringMaker = {
      +<(other)(start)
    }
    def +<(other: String, start: Property[_]): StringMaker = {
      (this open start) + other
    }
    def +#<(comment: String): StringMaker = {
      (this open comment)
    }
    def +#> : StringMaker = {
      val (comment, start) = mCommentOpen.head
      StringMaker(c, size, map.add(comment, start, size), mOpen, mCommentOpen.tail)
    }
    def +>(implicit t: Tree): StringMaker = {
      val start = mOpen.getOrElse(t, size)
      StringMaker(c, size, map.add(t, start, size), mOpen - t, mCommentOpen)
    }
    def +>(t: Property[_]): StringMaker = {
      val start = mOpen.getOrElse(t, size)
      StringMaker(c, size, map.add(t, start, size), mOpen - t, mCommentOpen)
    }
    def +(other: Rule): StringMaker = {
      print(this, other)
    }
    def +(other: RuleIterator): StringMaker = {
      print(this, other)
    }
  }
  
  /**
   * External printing function
   */
  def print(s: Iterable[RuleIterator], c: StringMaker = StringMaker()): StringMaker = {
    val res = printIterable[RuleIterator](c, s, print)
    res
  }
  
  /**
   * Print the definition of a set of game objects.
   */
  def printGameObjectDef(objects: Iterable[GameObject], c: StringMaker = StringMaker()) = {
    printIterable[GameObject](c, objects, { case (c, obj) =>
      val e = c + obj.className + "(" + obj.category + ")("
      val delimiter = "" andThen ","
      val e1 = (e /: obj.properties) { case (e, (name, prop)) =>
        e + delimiter.get + name + "=" +< (prop.get.toString, prop) +>(prop)
        // or  e +#< name +< (prop.get.toString, prop) +>(prop) +#>
      }
      e1 + ")"
    })
  }
  
  /**
   * Prints a set of functions printable with a function.
   */
  def printIterable[T](c: StringMaker, s: Iterable[T], f: (StringMaker, T) => StringMaker): StringMaker = {
    var e = c
    var first = true
    for(r <- s) { if(!first) e = e + LF else first = false
      e = f(e, r)
    }
    e
  }
  
  /**
   * Prints a RuleIterator
   */
  def print(c: StringMaker, r: RuleIterator): StringMaker = r match {
    case NoCategory(rule) => c + rule
    case Foreach1(cat, name, rule) => 
      c + (FOR_SYMBOL+" ") +! (name, cat) + (" " + IN_SYMBOL + " ") + cat + (":" + LF) + rule
    case Foreach2(cat1, cat2, name1, name2, rule) => 
      val c1 = cat1.objects.head
      val c2 = cat2.objects.head
      c +
      (FOR_SYMBOL+" ") +! (name1, cat1) + (" " + IN_SYMBOL + " ") + cat1 + (":" + LF) +
      (FOR_SYMBOL+" ") +! (name2, cat2) + (" " + IN_SYMBOL + " ") + cat2 + (":" + LF) + rule
  }
  /**
   * Prints a rule
   */
  def print(c: StringMaker, r: Rule): StringMaker = {
    //implicit val s_implicit = r
    r match {
      case Whenever(cond, action) => c + s"$IF_SYMBOL " + cond + s":$LF" + (action, INDENT)
    }
  }
  
  private def print(c: StringMaker, indent: String, s: Tree): StringMaker = {
    implicit val s_implicit = s
    //def augment(res: String) = (res, Map[Tree, (Int, Int)]() + (s -> (startIndex, startIndex + res.length)))
    //def mark
    s match {
    case Delete(null, ref) => c + indent +! s"Delete(${ref.name.get})"
    case Delete(name, ref) => c + indent +! s"Delete($name)"
    case Vec2Expr(lhs, rhs) => c + indent +< "(" + lhs + "," + rhs + ")" +>
    case VecExpr(l) => 
      l match {
        case Nil => c + indent +< "()" +>
        case a::Nil => c + indent + a
        case a::l =>
          (((c + indent +< "(" + a) /: l) { case (i, el) => i + "," + el }) + ")" +>
      }
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
        case _:Vec2Expr => ","
      }
      c + indent +< e.lhs + s" $op " + e.rhs +>
    case Assign(Nil, rhs: Expr) => c
    case Assign(List(e), rhs: Expr) => 
      c + indent +< e + "' = " + rhs +>
    case Assign(a::l, rhs: Expr) =>
      c + indent +< VecExpr(a::l) + "' = " + rhs +>
    case Reset(prop: MaybeAssignable) => 
      c + indent +< prop + "' = init_" + prop +>
    case Block(stats: Seq[Stat]) =>
      stats.toList match {
        case Nil => c + indent +! "{}"
        case a::Nil => c + indent +< a +>
        case a::l => 
          ((c + indent + INDENT +< a) /: l) { case (c, stat) => c + LF + indent + INDENT + stat} +>
      }
    case If(cond: Expr, s1: Stat, s2: Stat) => 
      val g = c +< s"$IF_SYMBOL " + cond + s":$LF"
      val h = if(s1 != NOP) g + (s1, indent + INDENT) else g
      val end = if(s2 != NOP) h + indent + s"else:$LF" + (s2, indent + INDENT) else h
      end +>
    case Copy(name: String, o: GameObjectRef, b: Block) =>
      c + indent +< s"$name = ${o.obj.name.get}.copy$LF" +(b, indent + INDENT) +>
    case NOP => c +! "NOP"
    case IfFunc(cond: Expr, s1: Expr, s2: Expr) =>
      c +< s"$indent$IF_SYMBOL(" + cond + ") " + s1 + " else " + s2 +>
    case IntegerLiteral(value: Int) => c + indent +< value.toString +>
    case FloatLiteral(value: Float) => c + indent +< value.toString +>
    case StringLiteral(value: String) => c + indent + "\"" + value + "\""
    case BooleanLiteral(value: Boolean) => c + indent + value.toString
    case Vec2Literal(x: Float, y: Float) => c + indent + "Vec2("+x.toString+","+y.toString+")"
    case UnitLiteral => c + indent + "()"
    case Val(name: String) => c + indent +! name
    case Not(expr: Expr)  => c + indent +< NOT_SYMBOL + expr +>
    case On(cond: Expr)   => c + indent +< "on " + cond +>
    case Once(cond: Expr) => c + indent +< "once " + cond +>
    case GameObjectRef(ref: String, _) => c + indent +! ref
    case GameObjectRef(null, obj) => c + indent +! obj.name.get
    case FingerMoveOver(o: GameObjectRef) => c + indent +< FINGER_MOVE_SYMBOL + o +>
    case FingerDownOver(o: GameObjectRef) => c + indent +< FINGER_DOWN_SYMBOL + o +>
    case FingerUpOver(o: GameObjectRef) => c + indent +< FINGER_UP_SYMBOL + o +>
    case FingerCoordX1 => c + indent +! "x1"
    case FingerCoordY1 => c + indent +! "y1"
    case FingerCoordX2 => c + indent +! "x2"
    case FingerCoordY2 => c + indent +! "y2"
    case PropertyIndirect(ref, obj, p) => c + indent +< s"$ref.$p" +>
    case PropertyRef(p) => p.name match {
      case "value" => c + indent +< p.parent.name.get +>  // Special case: For the value (like Int or Boolean, we do not specify the "value" property)
      case "velocity" => c + indent +< p.parent.name.get + ".velocity" +>
      case _ => c + indent +< p.parent.name.get + "." + p.name +>
    }
    case Choose(prop, expr) => c + indent +< "choose(" + prop + s" $ARROW_FUNC_SYMBOL " + expr + ")" +>
  }}
}
