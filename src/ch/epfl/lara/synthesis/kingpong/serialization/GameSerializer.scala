package ch.epfl.lara.synthesis.kingpong.serialization

import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.Game

import org.json.{JSONArray, JSONException, JSONObject}

object GameSerializer {

  private val CLASS_TAG = "__class__"

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

    case Block(exprs) =>
      val json = baseObject(expr)
      exprs map exprToJson foreach { j =>
        json.accumulate("exprs", j)
      }
      json

    case If(e1, e2, e3) =>
      val json = baseObject(expr)
      json.put("cond", exprToJson(e1))
      json.put("then", exprToJson(e2))
      json.put("else", exprToJson(e3))

    case _ => ???
  }

  def jsonToExpr(json: JSONObject): Expr = json.getString(CLASS_TAG) match {
    case "BooleanLiteral" =>
      BooleanLiteral(json.getBoolean("value"))

    case "IntegerLiteral" =>
      IntegerLiteral(json.getInt("value"))

    case "Variable" =>
      Variable(jsonToIdentifier(json.getJSONObject("id")))

    case "Block" =>
      val exprs = json.getJSONObjectSeq("exprs") map jsonToExpr
      Block(exprs)

    case "If" =>
      val e1 = jsonToExpr(json.getJSONObject("cond"))
      val e2 = jsonToExpr(json.getJSONObject("then"))
      val e3 = jsonToExpr(json.getJSONObject("else"))
      If(e1, e2, e3)


    case _ => ???
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
