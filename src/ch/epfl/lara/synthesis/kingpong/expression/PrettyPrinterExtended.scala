package ch.epfl.lara.synthesis.kingpong.expression

import Trees._
import Extractors._
import ch.epfl.lara.synthesis.kingpong.R
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import scala.language.postfixOps
import java.util.IdentityHashMap;
import scala.collection.JavaConversions._
import scala.util.matching._

case class PrettyPrinterExtended(ctx: Int => String) extends PrettyPrinterExtendedTypical {
  override val FOR_SYMBOL = ctx(R.string.FOR_SYMBOL)
  override val IN_SYMBOL = ctx(R.string.IN_SYMBOL)
  override val FINGER_MOVE_SYMBOL = ctx(R.string.FINGER_MOVE_SYMBOL)
  override val FINGER_DOWN_SYMBOL = ctx(R.string.FINGER_DOWN_SYMBOL)
  override val FINGER_UP_SYMBOL = ctx(R.string.FINGER_UP_SYMBOL)
  override val COLLIDES_SYMBOL = ctx(R.string.COLLIDES_SYMBOL)
  override val COLLIDING_SYMBOL = ctx(R.string.COLLIDING_SYMBOL)
  override val OUTOFCOLLISION_SYMBOL = ctx(R.string.OUTOFCOLLISION_SYMBOL)
  override val IF_SYMBOL = ctx(R.string.IF_SYMBOL)
}

object PrettyPrinterExtendedTypical extends CommonPrettyPrintingConstants {
  object Mappings {
    def apply(): Mappings = Mappings(Map[Category, List[(Int, Int)]](), Map[Int, List[Category]](), Map[Int, List[Tree]](), Map[Property[_], List[(Int, Int)]](), Map[Int, (Int, Int)](), Map[Int, String]())
  }
  /**
   * Recording of different mappings to retrieve the objects and the code.
   * mObjects    : Mapping from objects to the positions of their appearing in the code
   * mPos        : Mapping from each position of the string to the corresponding tree
   * mProperties : Mapping from each property to a set of positions in the code
   * mComment    : Mapping from each position of the code to the corresponding comment
   */
  case class Mappings(mObjects: Map[Category, List[(Int, Int)]],
      mPosCategories: Map[Int, List[Category]],
      mPos: Map[Int, List[Tree]],
      mProperties: Map[Property[_], List[(Int, Int)]],
      mConstantsPos: Map[Int, (Int, Int)],
      mComment: Map[Int, String]) {
    def add(obj: GameObject, start: Int, end: Int): Mappings = {
      add(obj.category, start, end)
    }
    def add(obj: Category, start: Int, end: Int): Mappings = {
      this.copy(mObjects=mObjects + (obj -> ((start, end)::mObjects.getOrElse(obj, Nil))), mPosCategories = addPositions(obj, start, end, mPosCategories))
    }
    def add(obj: Property[_], start: Int, end: Int): Mappings = {
      this.copy(mProperties = mProperties + (obj -> ((start, end)::mProperties.getOrElse(obj, Nil))))
    }
    def add(s: Tree, start: Int, end: Int): Mappings = {
      val p1 = s match {
        case ObjectLiteral(obj) if obj != null => Some(obj.category)
        
        //TODO Lomig to compile
        //case PropertyLiteral(prop) => Some(prop.parent.category)
        case _ =>  None
      }
      val posCategories = p1 match {
        case Some(category) => addPositions(category, start, end, mPosCategories)
        case None => mPosCategories
      }
      val constantPos = s match {
        case a: Literal[_] => 
          addBounds(start, end, mConstantsPos)
        case _ =>
          mConstantsPos
      }
      
      Mappings(addIfCategory(s, start, end, mObjects), posCategories, addPositions(s, start, end, mPos), addIfProperty(s, start, end, mProperties), constantPos, mComment)
    }
    def add(comment: String, start: Int, end: Int): Mappings = {
      this.copy(mComment=addComment(comment, start, end, mComment))
    }
    def addIfCategory(s: Tree, start: Int, end: Int, mm: Map[Category, List[(Int, Int)]]): Map[Category, List[(Int, Int)]] = {
      s match {
        case ObjectLiteral(o) if o != null => mm + (o.category -> ((start, end)::mm.getOrElse(o.category, List())))
        //TODO Lomig to compile
        //case PropertyLiteral(prop) => mm + (prop.parent.category -> ((start, end)::mm.getOrElse(prop.parent.category, List())))
        case _ => mm
      }
    }
    def addPositions[T](s: T, start: Int, end: Int, mm: Map[Int, List[T]]): Map[Int, List[T]] = {
      (mm /: (start to end)) { case (map, i) =>
        map.get(i) match {
          case Some(t) => map + (i -> (s::t))
          case None => map + (i -> (s::Nil))
        }
      }
    }
    def addBounds(start: Int, end: Int, mm: Map[Int, (Int, Int)]): Map[Int, (Int, Int)] = {
      (mm /: (start to end)) { case (map, i) =>
        map.get(i) match {
          case Some(t) => map
          case None => map + (i -> (start, end))
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
        //TODO Lomig to compile
        //case PropertyLiteral(prop) => mm + (prop -> ((start, end)::mm.getOrElse(prop, List())))
        case _ => mm
      }
    }
    lazy val mPropertyPos: Map[Int, Property[_]] = {
      Map[Int, Property[_]]() ++ mProperties.flatMap{
        case (p, l) => l flatMap { case (start, end) => (start to end) map { case i => i -> p }}
      }
    }
    def insertPositions(start: Int, oldEnd: Int, newEnd: Int): Mappings = {
      if(oldEnd == newEnd) return this;
      val amount = newEnd - oldEnd
      val lowerBound = Math.min(newEnd, oldEnd)
      val isLower = newEnd < oldEnd;
      @inline def map(i: Int): Int = {
        if(i >= oldEnd) i + amount else if(isLower && newEnd <= i) newEnd else i
      }
      val newMPosRaw = mPos.map{ case (k, v) => (map(k), v)}
      val newMPos = if(!isLower && mPos.containsKey(oldEnd)) {
        val newElem = mPos(oldEnd)
        newMPosRaw ++ (for(i <- (oldEnd until newEnd)) yield (i -> newElem))
      } else {
        newMPosRaw
      }
      this.copy(
          mObjects.mapValues(_.map{case (i,j) => (map(i), map(j))}).view.force,
          mPosCategories.map{ case (k, v) => (map(k), v)},
          newMPos,
          mProperties.mapValues(_.map{case (i,j) => (map(i), map(j))}).view.force,
          mConstantsPos.map{ case (k, (v1, v2)) => (map(k), (map(v1), map(v2)))},
          mComment.map{ case (k, v) => (map(k), v)}
       )
    }
    def replace(oldTree: Tree, newTree: Tree, start: Int, oldEnd: Int, newEnd: Int): Mappings = {
      
      val newMPos = if(oldTree.isInstanceOf[Expr] && newTree.isInstanceOf[Expr]) {
        val mapCache = new IdentityHashMap[Expr, Expr]()
        mapCache(oldTree.asInstanceOf[Expr]) = newTree.asInstanceOf[Expr];
        val res = mPos.map{ case (k, v) => (k, { // The most general tree is at the beginning.
          if(v.isEmpty) v else {
            v.head match {
              case head: Expr =>
		            var found = false
		            TreeOps.preTraversal(t => found ||= t eq oldTree)(head)
		            if(!found) v else { // If not found, we do not perform any replacement.
		              v.map{
				            case expr: Expr => 
				              if(mapCache.containsKey(expr)) {
				                mapCache(expr)
				              } else {
				                TreeOps.postMap((exBefore, exAfter) => {
				                  if(mapCache.containsKey(exBefore)) {
				                    Some(mapCache(exBefore))
				                  } else if(exBefore ne exAfter) {
				                    mapCache(exBefore) = exAfter;
				                    Some(exAfter)
				                  } else None
				                })(expr) 
				              }
				            case f => f
		            } }
              case _ => v
            } }
        })}
        res
      } else mPos
      
      this.copy(mObjects: Map[Category, List[(Int, Int)]],
      mPosCategories: Map[Int, List[Category]],
      newMPos,
      mProperties: Map[Property[_], List[(Int, Int)]],
      mConstantsPos: Map[Int, (Int, Int)],
      mComment: Map[Int, String]).insertPositions(start, oldEnd, newEnd)
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
  object StringMaker { def apply(self: PrettyPrinterExtendedTypical): StringMaker = new StringMaker(new StringBuilder, self, 0, Mappings(), Map[Any, Int](), Nil, true)}
  
  val FormatReg = new Regex("(?:%(\\d{1,2}))?\\$s")
  val splitReg = new Regex("(?<=(%\\d{1,2})?\\$s)|(%\\d{1,2})?\\$s")
  
	case class StringMaker(c: StringBuilder, printer: PrettyPrinterExtendedTypical, size: Int, map: Mappings, mOpen: Map[Any, Int], mCommentOpen: List[(String, Int)], displayComments: Boolean) {
	  def +(other: String): StringMaker = { // Simple add
	    if(other == "" || other == null) {
	      this
	    } else {
	      StringMaker(c append other, printer, size + other.size, map, mOpen, mCommentOpen, displayComments)
	    }
	  }
	  /*def +<>(other: String)(implicit s: Tree): StringMaker = {
	    StringMaker(c append other, size + other.size, map.add(s, size, size + other.size), mOpen, mCommentOpen)
	  }*/
	  def +!(other: String, s: Category): StringMaker = {
	    StringMaker(c append other, printer, size + other.size, map.add(s, size, size + other.size), mOpen, mCommentOpen, displayComments)
	  }
	  def +(other: Tree): StringMaker = { // Don't care about this expression in particular. But sub expressions will be recorded.
	    printer.print(this, NO_INDENT, other)
	  }
	  def +(other: Category): StringMaker = {
	    if(other.name != null) {
	      +!(other.name, other)
	    } else this
	  }
	  def +(other: Tree, newIndentation: String = NO_INDENT): StringMaker = { // Care about this new expression by storing its position.
	    val result = printer.print(this, newIndentation, other)
	    StringMaker(result.c, printer, result.size, result.map.add(other, size, result.size - 1), result.mOpen, result.mCommentOpen, displayComments)
	  }
	  def open(implicit start: Tree): StringMaker = {
	    StringMaker(c, printer, size, map, mOpen + (start -> size), mCommentOpen, displayComments)
	  }
	  def open[T](implicit start: Property[T]): StringMaker = {
	    StringMaker(c, printer, size, map, mOpen + (start -> size), mCommentOpen, displayComments)
	  }
	  def open(comment: String): StringMaker = {
	    StringMaker(c, printer, size, map, mOpen, (comment -> size)::mCommentOpen, displayComments)
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
	  /** Add a comment to the code, to be colored in another color. */
	  def +#<(comment: String): StringMaker = {
	    (this open comment)
	  }
	  def +#> : StringMaker = {
	    val (comment, start) = mCommentOpen.head
	    StringMaker(c, printer, size, map.add(comment, start, size - 1), mOpen, mCommentOpen.tail, displayComments)
	  }
	  def +>(implicit t: Tree): StringMaker = {
	    val start = mOpen.getOrElse(t, size)
	    StringMaker(c, printer, size, map.add(t, start, size - 1), mOpen - t, mCommentOpen, displayComments)
	  }
	  def +>(t: Property[_]): StringMaker = {
	    val start = mOpen.getOrElse(t, size)
	    StringMaker(c, printer, size, map.add(t, start, size - 1), mOpen - t, mCommentOpen, displayComments)
	  }
	  /**
	   * Formats a given template with the given arguments Possible formatting are:
	   * $s : index-based
	   * %d$s where d is a number : Position-based.
	   */
	  def format(template: String, arguments: Any*): StringMaker = {
	    val itemsSep = 0::splitReg.findAllMatchIn(template).map(m => m.start).toList;
	    val items = (itemsSep.foldRight((template, Nil: List[String])){case (itemSep, (template, list)) => {
	      val (newTemplate, newItem) = template.splitAt(itemSep)
	      (newTemplate, newItem::list)
	    }})._2
	    val args = arguments.toArray
	    var currentIndex = 0
	    (this /: items)((c, item) => {
	      item match {
	        case FormatReg(sd) =>
	          val d = if(sd == "" || sd == null) {
	            val res = currentIndex
	            currentIndex+=1;
	            res
	          } else {
	            currentIndex+=1;
	            sd.toInt - 1
	          }
	          if(d < args.length) {
	            args(d) match {
	              case o: GameObject => c + ObjectLiteral(o)
	              case t: Tree => c + t
	              case ct: Category => c + ct
	              case s: String => c + s
	              case r => c + r.toString()
	            }
	          } else {
	            c
	          }
	        case s: String => c + s
	      }
	    })
	  }
	}

}

trait PrettyPrinterExtendedTypical extends CommonPrettyPrintingConstants { self =>
  def ctx: Int => String
  
  import PrettyPrinterExtendedTypical._
  lazy val LANGUAGE_SYMBOLS = List(IF_SYMBOL, FOR_SYMBOL, IN_SYMBOL, COLLIDES_SYMBOL, FINGER_DOWN_SYMBOL, FINGER_MOVE_SYMBOL, FINGER_UP_SYMBOL, LET_SYMBOL)
  
  /**
   * External printing function
   */
  def print(s: Traversable[Expr], c: StringMaker = StringMaker(self)): StringMaker = {
    val res = printIterable[Expr](c, s, print)
    res
  }
  
  /**
   * External printing function
   */
  def print(s: Tree): StringMaker = {
    val res = print(StringMaker(self), s)
    res
  }
  
  /**
   * Print the definition of a set of game objects.
   */
  val accepted_properties = List("x", "y", "radius", "width", "height", "velocity", "color", "visible", "friction", "restitution", "linear-damping", "value", "language", "text", "time")
  def printGameObjectDef(objects: Iterable[GameObject], c: StringMaker = StringMaker(self)) = {
    val ending = printIterable[GameObject](c, objects, { case (c, obj) =>
      val name = obj.name.get
      val e = c + "val " + name + "=" + obj.getClass.getName.replaceAll(""".*\.""", "") + LF + "  category=" + obj.category + LF
      val delimiter = "  " andThen (LF + "  ")
      val e1 = (e /: accepted_properties) { case (e, name) => obj.getProperty(name) match {
        case Some(prop) => 
          e + delimiter.get + name + "=" +< (prop.get.toString, prop) +>(prop)
        case None =>
          e
      }}
        // or  e +#< name +< (prop.get.toString, prop) +>(prop) +#>
      e1
    })
    ending + LF
  }
  
  /**
   * Prints a set of functions printable with a function.
   */
  def printIterable[T](c: StringMaker, s: Traversable[T], f: (StringMaker, T) => StringMaker): StringMaker = {
    val delimiter = "" andThen LF
    (c /: s) { case (e, r) => f(e + delimiter.get, r) }
  }
  
  /**
   * Prints a Stat
   */
  /*def print(c: StringMaker, r: RuleIterator): StringMaker = r match {
    case NoCategory(rule) => c + rule
    case Foreach1(cat, name, rule) => 
      c + (FOR_SYMBOL+" ") +! (name, cat) + (" " + IN_SYMBOL + " ") + cat + (":" + LF) + rule
    case Foreach2(cat1, cat2, name1, name2, rule) => 
      val c1 = cat1.objects.head
      val c2 = cat2.objects.head
      c +
      (FOR_SYMBOL+" ") +! (name1, cat1) + (" " + IN_SYMBOL + " ") + cat1 + (":" + LF) +
      (FOR_SYMBOL+" ") +! (name2, cat2) + (" " + IN_SYMBOL + " ") + cat2 + (":" + LF) + rule
  }*/
  /**
   * Prints a rule
   */
  /*def print(c: StringMaker, r: Rule): StringMaker = {
    //implicit val s_implicit = r
    r match {
      case Whenever(cond, action) => c + s"$IF_SYMBOL " + cond + s":$LF" + (action, INDENT)
    }
  }*/
  private def print(c: StringMaker, s: Tree): StringMaker = print(c, NO_INDENT, s)
  
  private def print(c: StringMaker, indent: String, s: Tree): StringMaker = {
    implicit val s_implicit = s
    //def augment(res: String) = (res, Map[Tree, (Int, Int)]() + (s -> (startIndex, startIndex + res.length)))
    //def mark

    s match {
      case e: Tree with Commented if c.displayComments && e.comment != null => e.comment(c.open(s)) +>
      case i: Identifier => c +< i.toString +>
      case Let(id, expr, body) =>
        c + indent +< s"$LET_SYMBOL " + id + s" $LET_ASSIGN_SYMBOL " + expr + LF + (body, indent) +>
      
      case MethodCall(method, Nil) => c +< method + "()" +>
      case MethodCall(method, args) => ((c +< method + "(" + args.head) /: args.tail) { case (c, arg) => c + ", " +  arg } + ")" +>
      case ParExpr(Nil) => c
      case ParExpr(a::l) => c +< a + ("//" + l.size + ctx(R.string.parexpr_more)) +>
      case TupleSelect(expr, index) =>
        //TODO this only applies to pair of coordinates...
        c + indent + expr + "." + (if(index == 1) "x" else "y")
      
      case Foreach(cat, id, body) =>
        c + indent +< s"$FOR_SYMBOL " +! (id.toString, cat) + s" $IN_SYMBOL " + cat + s":$LF" + body +>
      case Forall(category, id, body) =>
        c +! (category.name, category) +< ("."+ctx(R.string.tree_forall)+"{") + id + " => " + body + "}" +>
      case Find(category, id, body) =>
        c +! (category.name, category) +< ("."+ctx(R.string.tree_find)+"{") + id + " => " + body + "}" +>
        
    case Delete(obj) => c + indent +< (ctx(R.string.tree_delete) + "(") + obj + ")" +>
    case Tuple(exprs) => 
      exprs match {
        case Seq() => c + indent +< "()" +>
        case Seq(a) => c + indent + a
        case _ =>
          (((c + indent +< "(" + exprs.head) /: exprs.tail) { case (i, el) => i + "," + el }) + ")" +>
      }
    case Assign((e, prop), rhs) =>
      c + indent +< e + "." + prop + "' = " + rhs +>
    case Block(exprs) =>
      exprs.toList match {
        case Nil => c + indent +< "{}" +>
        case a::Nil => c + indent +< a +>
        case a::l => 
          ((c + indent + INDENT +< a) /: l) { case (c, stat) => c + LF + indent + INDENT + stat} +>
      }
    case If(cond, s1, s2) =>
      val g = c + indent +< s"$IF_SYMBOL " + cond + s":$LF"
      val h = g + (s1, indent + INDENT)
      val end = if(s2 != UnitLiteral) h + LF + indent + (ctx(R.string.tree_else)+":"+LF) + (s2, indent + INDENT) else h
      end +>
    case Copy(obj, id, block) =>
      c + indent +< s"$id = " + obj + ("."+ctx(R.string.tree_copy)+LF) + (block, indent + INDENT) +>

    case Choose(a::q, pred, body) => 
      (((c + indent +< (ctx(R.string.tree_choose) + "(") + a) /: q){ case (c, v) => c + "," + v}) + ")" + s" $ARROW_FUNC_SYMBOL " + pred + ")" +>
    
    case IntegerLiteral(value: Int) => c + indent +< value.toString +>
    case FloatLiteral(value: Float) => c + indent +< value.toString +>
    case StringLiteral(value: String) => c + indent + "\"" + value + "\""
    case BooleanLiteral(value: Boolean) => c + indent + value.toString
    case UnitLiteral => c + indent + "()"
    case ObjectLiteral(null) => c + indent +< "null" +>
    case ObjectLiteral(obj) => c + indent +< obj.name.get +>
    
    case Select(expr, property) => c + indent +< expr + "." + property +>
    case Variable(id) => c + indent +< id.toString +>
    case Not(expr: Expr) => c + indent +< NOT_SYMBOL + expr +>
    
    case ContainingCell(_, obj) => c + indent +< "cell(" + obj + ")" +>
    case Apply(arr, col, row) => c + indent +< arr + "(" + col + "," + row + ")" +>

    case FingerMoveOver(obj, _, UnitLiteral) => c + indent +< FINGER_MOVE_SYMBOL + " " + obj +>
    case FingerMoveOver(obj, id, block) => c + indent +< FINGER_MOVE_SYMBOL + " " + obj + s":$LF" + (block, indent + INDENT) +>
    case FingerUpOver(obj, id, block) => c + indent +< FINGER_UP_SYMBOL + " " + obj + (block, indent + INDENT) +>
    case FingerDownOver(obj, id, block) => c + indent +< FINGER_DOWN_SYMBOL + " " + obj + (block, indent + INDENT) +>
    case IsFingerDownOver(o) => c + indent +< FINGER_DOWN_SYMBOL + " " + o +>
    case IsFingerUpOver(o) => c + indent +< FINGER_UP_SYMBOL + " " + o +>
    
    case b @ BinaryOperator(lhs, rhs, _) => val op = b match {
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
        case _:Colliding => COLLIDING_SYMBOL
        case _:OutOfCollision => OUTOFCOLLISION_SYMBOL
        case _:Contains => ctx(R.string.tree_contains)
        case _:ApplyForce => ctx(R.string.tree_applyforce)
        case _ => "["+b.getClass().getName()+"]"
      }
      c + indent +< lhs + s" $op " + rhs +>
  }}
}

