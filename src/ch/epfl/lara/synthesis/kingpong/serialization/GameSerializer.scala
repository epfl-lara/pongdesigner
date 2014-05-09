package ch.epfl.lara.synthesis.kingpong.serialization

import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.expression.Extractors._
import org.json.{JSONArray, JSONException, JSONObject}
import scala.collection.mutable.ListBuffer

object GameSerializer {
   private val CLASS_TAG = "__class__"

  /** A converter to convert from JSONObject to Expr and vice-versa*/
  abstract class Converter[T <: Expr : Builder](TAG: String) {
    def apply(expr: T): JSONObject = {
      val json = baseObject(expr)
      for((str, f) <- map) {
        json.put(str, f(expr))
      }
      for((str, f) <- mapSeq) {
        f(expr) map exprToJson foreach { j =>
	        json.accumulate(str, j)
	      }
      }
      json
    }
    def unapply(json: JSONObject): Option[T] = {
       if(json.getString(CLASS_TAG) == TAG) {
	      Some(implicitly[Builder[T]].apply((for((str, f) <- map) yield jsonToExpr(json.getJSONObject(str)))
	          ++
	          (for((str, f) <- (mapSeq: Seq[(String, T => Seq[Expr])]); j <- json.getJSONObjectSeq(str)) yield (jsonToExpr(j)))
	      ))
       } else None
    }
    
    /**
     * The list of parameters used to build this object
     */
    var map = ListBuffer[(String, T => Expr)]()
    var mapSeq = ListBuffer[(String, T => Seq[Expr])]()
    implicit class RichMap(s: String) {
      def <->(t: T => Expr): Unit = map += ((s, (t)))
      def <++>(t: T => Seq[Expr]): Unit = mapSeq += ((s, t))
    }
  }
  
  /** If converter */
  case object IfConverter extends Converter[If]("If") {
    "cond" <-> (_.cond)
    "then" <-> (_.thenn)
    "else" <-> (_.elze)
  }
  
  /** Block converter */
  case object BlockConverter extends Converter[Block]("Block") {
    "exprs" <++> (_.exprs)
  }

  def identifierToJson(id: Identifier): JSONObject = {
    val json = new JSONObject()
    json.put("name", id.name)
    json.put("id", id.id)
    json
  }

  def jsonToIdentifier(json: JSONObject): Identifier = {
    FreshIdentifier(json.getString("name"), json.getInt("id"))
  }

  def exprToJson(expr: Expr): JSONObject = expr match {
    case BooleanLiteral(value) =>
      val json = baseObject(expr)
      json.put("value", value)

    case IntegerLiteral(value) =>
      val json = baseObject(expr)
      json.put("value", value)

    case Variable(id) =>
      val json = baseObject(expr)
      json.put("id", identifierToJson(id))

    case e:Block => BlockConverter(e)

    case e:If => IfConverter(e)

    case _ => ???
  }

  def jsonToExpr(json: JSONObject): Expr = json match {
       case IfConverter(i) => i
       case BlockConverter(b) => b
       
	    case _ =>
		    json.getString(CLASS_TAG) match {
		    case "BooleanLiteral" =>
		      BooleanLiteral(json.getBoolean("value"))
		
		    case "IntegerLiteral" =>
		      IntegerLiteral(json.getInt("value"))
		
		    case "Variable" =>
		      Variable(jsonToIdentifier(json.getJSONObject("id")))
		
		    case "Block" =>
		      val exprs = json.getJSONObjectSeq("exprs") map jsonToExpr
		      Block(exprs)

		    case _ => ???
	  }
  }

  def gameToJson(game: Game): JSONObject = ???
  def jsonToGame(json: JSONObject): Game = ???

  private def baseObject(expr: Expr): JSONObject = {
    new JSONObject().put(CLASS_TAG, expr.getClass.getSimpleName)
  }

  private implicit class RichJSONObject(val obj: JSONObject) extends AnyVal {
    def getJSONObjectSeq(name: String): Seq[JSONObject] = {
      val array = obj.getJSONArray(name)
      for (i <- 0 to array.length) yield array.getJSONObject(i)
    }
  }

//  private implicit class RichJSONArray(array: JSONArray) extends AnyVal {
//    def toJSONObjectSeq: Seq[JSONObject] = {
//      for (i <- 0 to array.length) yield array.getJSONObject(i)
//    }
//  }

}
