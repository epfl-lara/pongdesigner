package ch.epfl.lara.synthesis.kingpong.serialization

import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.expression.Extractors._
import org.json.{JSONArray, JSONException, JSONObject}
import scala.collection.mutable.ListBuffer
import ch.epfl.lara.synthesis.kingpong.objects.Category
import ch.epfl.lara.synthesis.kingpong.objects.GameObject
import scala.collection.mutable.ArrayBuffer

object GameSerializer {
  private val CLASS_TAG = "__class__"

  /**
   * the context used to create the game
   */
  class SerializerContext {
    var objects = ArrayBuffer[GameObject]()
    var map = Map[String, GameObject]()
    var categories = Map[String, Category]()
  }
    
  sealed trait Converter[T <: Expr] {
    def apply(expr: T): JSONObject
    def unapply(json: JSONObject)(implicit ctx: SerializerContext): Option[T]
  }
  /** A converter to convert from JSONObject to Expr and vice-versa*/
  abstract class ConverterBuilder[T <: Expr : Builder](TAG: String) extends Converter[T] {
    def apply(expr: T): JSONObject = {
      val json = baseObject(expr)
      for((str, f) <- map) {
        json.put(str, exprToJson(f(expr)))
      }
      for((str, f) <- mapSeq) {
        f(expr) map exprToJson foreach { j =>
	        json.accumulate(str, j)
	      }
      }
      json
    }
    def unapply(json: JSONObject)(implicit ctx: SerializerContext): Option[T] = {
       if(json.getString(CLASS_TAG) == TAG) {
	      Some(implicitly[Builder[T]].apply((for((str, f) <- map) yield jsonToExpr(json.getJSONObject(str)))
	          ++
	          (for((str, f) <- (mapSeq: Seq[(String, T => Seq[Expr])]); j <- json.getJSONObjectSeq(str)) yield (jsonToExpr(j)))
	      ))
       } else None
    }
    
    /**
     * The list of parameters used to build this object.
     * Syntax:
     * "property" <-> (_.property)
     * If a list of expr instead of a single expr
     * "property" <++> (_.property)
     */
    var map = ListBuffer[(String, T => Expr)]()
    var mapSeq = ListBuffer[(String, T => Seq[Expr])]()
    implicit class RichMap(s: String) {
      def <->(t: T => Expr): Unit = map += ((s, (t)))
      def <++>(t: T => Seq[Expr]): Unit = mapSeq += ((s, t))
    }
  }
  
  /** A converter to convert from JSONObject to Expr and vice-versa*/
  abstract class CustomConverter[T <: Expr](TAG: String) extends Converter[T] {
    def apply(expr: T): JSONObject = {
      val json = baseObject(expr)
      for((str, f) <- map) {
        json.put(str, f(expr))
      }
      json
    }
    implicit var implicitJSON: JSONObject = null
    var unbuilder: () => T = null
    implicit var implicitContext:  SerializerContext = null
    def unbuild(f: => T) = unbuilder = {() => f}
    
    //def unbuild(f: SerializerContext => T) = unbuilder = f
    //def unbuild(json: JSONObject): T
        
    def unapply(json: JSONObject)(implicit ctx: SerializerContext): Option[T] = {
      implicitContext = ctx
       if(json.getString(CLASS_TAG) == TAG) {
         implicitJSON = json
         Some(unbuilder())
       } else None
    }
    var map = ListBuffer[(String, T => JSONObject)]()
    implicit class RichMap(s: String) {
      def <->(t: T => Expr): Unit = map += ((s, e => exprToJson(t(e))))
      def <*>(t: T => String): Unit = map += ((s, e => stringToJson(t(e))))
      def <=>(t: T => Identifier): Unit = map += ((s, e => identifierToJson(t(e))))
      def <&>(t: T => Boolean): Unit = map += ((s, e => booleanToJson(t(e))))
      def <+>(t: T => Int): Unit = map += ((s, e => intToJson(t(e))))
      def <~>(t: T => Float): Unit = map += ((s, e => floatToJson(t(e))))
      def <==>(t: T => Seq[Identifier]): Unit = map += ((s, e => identifierSeqToJson(s, t(e))))
      def <-->(t: T => Seq[Expr]): Unit = map += ((s, e => exprSeqToJson(s, t(e))))
    }
    
    def _1(implicit json: JSONObject): JSONObject = json.getJSONObject(map(0)._1)
    def _2(implicit json: JSONObject): JSONObject = json.getJSONObject(map(1)._1)
    def _3(implicit json: JSONObject): JSONObject = json.getJSONObject(map(2)._1)
    
    def __1(implicit json: JSONObject): List[JSONObject] = json.getJSONObjectSeq(map(0)._1).toList
    def __2(implicit json: JSONObject): List[JSONObject] = json.getJSONObjectSeq(map(1)._1).toList
    def __3(implicit json: JSONObject): List[JSONObject] = json.getJSONObjectSeq(map(2)._1).toList
    implicit def internalJsonToIdentifier(json: JSONObject): Identifier = jsonToIdentifier(json)
    implicit def internalJsonToExpr(json: JSONObject): Expr = jsonToExpr(json)
    implicit def internalJsonToString(json: JSONObject): String = jsonToString(json)
    implicit def internalJsonToBoolean(json: JSONObject): Boolean = jsonToBoolean(json)
    implicit def internalJsonToInt(json: JSONObject): Int = jsonToInt(json)
    implicit def internalJsonToFloat(json: JSONObject): Float = jsonToFloat(json)
    implicit def internalJsonToExprSeq(json: List[JSONObject]): List[Expr]= json.map(jsonToExpr)
    implicit def internalJsonToIdentifierList(json: List[JSONObject]): List[Identifier]= json.map(jsonToIdentifier).toList
    implicit def internalJsonToGameObject(json: JSONObject): GameObject = implicitContext.map.getOrElse(jsonToString(json), null)
    implicit def internalJsonToCategory(json: JSONObject): Category = implicitContext.categories.getOrElse(jsonToString(json), null)
  }
  
  trait BinaryConverter[T <: Expr { def lhs: Expr; def rhs: Expr }] extends ConverterBuilder[T] { self: ConverterBuilder[T] =>
    "lhs" <-> (_.lhs)
    "rhs" <-> (_.rhs)
  }
  
  /** Binary converter */
  implicit case object EqualsConverter extends ConverterBuilder[Equals]("Equals") with BinaryConverter[Equals]
  implicit case object PlusConverter extends ConverterBuilder[Plus]("Plus") with BinaryConverter[Plus]
  implicit case object MinusConverter extends ConverterBuilder[Minus]("Minus") with BinaryConverter[Minus]
  implicit case object TimesConverter extends ConverterBuilder[Times]("Times") with BinaryConverter[Times]
  implicit case object DivConverter extends ConverterBuilder[Div]("Div") with BinaryConverter[Div]
  implicit case object ModConverter extends ConverterBuilder[Mod]("Mod") with BinaryConverter[Mod]

  implicit case object LessThanConverter extends ConverterBuilder[LessThan]("LessThan") with BinaryConverter[LessThan]
  implicit case object GreaterThanConverter extends ConverterBuilder[GreaterThan]("GreaterThan") with BinaryConverter[GreaterThan]
  implicit case object LessEqConverter extends ConverterBuilder[LessEq]("LessEq") with BinaryConverter[LessEq]
  implicit case object GreaterEqConverter extends ConverterBuilder[GreaterEq]("GreaterEq") with BinaryConverter[GreaterEq]
  implicit case object AndConverter extends ConverterBuilder[And]("And") with BinaryConverter[And]
  implicit case object OrConverter extends ConverterBuilder[Or]("Or") with BinaryConverter[Or]

  implicit case object CollisionConverter extends ConverterBuilder[Collision]("Collision") with BinaryConverter[Collision]
  implicit case object ContainsConverter extends ConverterBuilder[Contains]("Contains") with BinaryConverter[Contains]
  implicit case object LetConverter extends CustomConverter[Let]("Let") {
    "id" <=> (_.id)
    "expr" <-> (_.expr)
    "body" <-> (_.body)
    unbuild(Let(_1, _2, _3))
  }
  implicit case object CopyConverter extends CustomConverter[Copy]("Copy") {
    "obj" <-> (_.obj)
    "id" <=> (_.id)
    "body" <-> (_.body)
    unbuild(Copy(_1, _2, _3))
  }
  implicit case object ApplyForceConverter extends CustomConverter[ApplyForce]("ApplyForce") {
    "obj" <-> (_.obj)
    "force" <-> (_.force)
    unbuild(ApplyForce(_1, _2))
  }
  implicit case object AssignConverter extends CustomConverter[Assign]("Assign") {
    "obj" <-> (_.prop._1)
    "prop" <*> (_.prop._2)
    "rhs" <-> (_.rhs)
    unbuild(Assign((_1, _2),_3))
  }
  implicit case object SelectConverter extends CustomConverter[Select]("Select") {
    "expr" <-> (_.expr)
    "property" <*> (_.property)
    unbuild(Select(_1, _2))
  }
  implicit case object ContainingCellConverter extends CustomConverter[ContainingCell]("ContainingCell") {
    "array" <-> (_.array)
    "obj" <-> (_.obj)
    unbuild(ContainingCell(_1, _2))
  }
  implicit case object FingerMoveOverConverter  extends CustomConverter[FingerMoveOver]("FingerMoveOver") {
    "obj" <-> (_.obj)
    "id" <=> (_.id)
    "block" <-> (_.block)
    unbuild(FingerMoveOver(_1, _2, _3))
  }
  implicit case object FingerUpOverConverter   extends CustomConverter[FingerUpOver]("FingerUpOver") {
    "obj" <-> (_.obj)
    "id" <=> (_.id)
    "block" <-> (_.block)
    unbuild(FingerUpOver(_1, _2, _3))
  }
  implicit case object FingerDownOverConverter  extends CustomConverter[FingerDownOver]("FingerDownOver") {
    "obj" <-> (_.obj)
    "id" <=> (_.id)
    "block" <-> (_.block)
    unbuild(FingerDownOver(_1, _2, _3))
  }
  
  // Literals
  implicit case object BooleanLiteralConverter extends CustomConverter[BooleanLiteral]("BooleanLiteral") {
    "value" <&> (_.value)
    unbuild(BooleanLiteral(_1))
  }
  implicit case object IntegerLiteralConverter extends CustomConverter[IntegerLiteral]("IntegerLiteral") {
    "value" <+> (_.value)
    unbuild(IntegerLiteral(_1))
  }
  implicit case object FloatLiteralConverter extends CustomConverter[FloatLiteral]("FloatLiteral") {
    "value" <~> (_.value)
    unbuild(FloatLiteral(_1))
  }
  implicit case object StringLiteralConverter extends CustomConverter[StringLiteral]("StringLiteral") {
    "value" <*> (_.value)
    unbuild(StringLiteral(_1))
  }
  
  
  implicit case object VariableConverter extends CustomConverter[Variable]("Variable") {
    "id" <=> (_.id)
    unbuild(Variable(_1))
  }

  /** If converter */
  implicit case object IfConverter extends ConverterBuilder[If]("If") {
    "cond" <-> (_.cond)
    "then" <-> (_.thenn)
    "else" <-> (_.elze)
  }
  
  /** Apply converter */
  implicit case object ApplyConverter extends CustomConverter[Apply]("Apply") {
    "obj" <-> (_.obj)
    "column" <-> (_.column)
    "row" <-> (_.row)
    unbuild(Apply(_1,_2,_3))
  }
  
  /** Block converter */
  implicit case object BlockConverter extends ConverterBuilder[Block]("Block") {
    "exprs" <++> (_.exprs)
  }
  
  /** Choose converter */
  implicit case object ChooseConverter extends CustomConverter[Choose]("Choose") {
    "vars" <==> (_.vars)
    "constraint" <-> (_.constraint)
    unbuild(Choose(__1, _2))
  }
  
  /** Column converter */
  implicit case object ColumnConverter extends CustomConverter[Column]("Column") {
    "obj" <-> (_.obj)
    unbuild(Column(_1))
  }
  
  /** Row converter */
  implicit case object RowConverter extends CustomConverter[Row]("Row") {
    "obj" <-> (_.obj)
    unbuild(Row(_1))
  }

  
  
  /** Debug converter */
  implicit case object DebugConverter extends CustomConverter[Debug]("Debug") {
    "message" <*> (_.message)
    "exprs" <--> (_.exprs)
    unbuild(Debug(_1, __2))
  }
  
  /** Delete converter */
  implicit case object DeleteConverter extends CustomConverter[Delete]("Delete") {
    "obj" <-> (_.obj)
    unbuild(Delete(_1))
  }
  
  /** Find converter */
  implicit case object FindConverter extends CustomConverter[Find]("Find") {
    "category" <*> (_.category.name)
    "id" <=> (_.id)
    "body" <-> (_.body)
    unbuild(Find(_1, _2, _3))
  }

  /** Count converter */
  implicit case object CountConverter extends CustomConverter[Count]("Count") {
    "category" <*> (_.category.name)
    unbuild(Count(_1))
  }
  
  /** Forall converter */
  implicit case object ForallConverter extends CustomConverter[Forall]("Forall") {
    "category" <*> (_.category.name)
    "id" <=> (_.id)
    "body" <-> (_.body)
    unbuild(Forall(_1, _2, _3))
  }
  
  /** Foreach converter */
  implicit case object ForeachConverter extends CustomConverter[Foreach]("Foreach") {
    "category" <*> (_.category.name)
    "id" <=> (_.id)
    "body" <-> (_.body)
    unbuild(Foreach(_1, _2, _3))
  }
  
  /** IsFingerDownOver converter */
  implicit case object IsFingerDownOverConverter extends CustomConverter[IsFingerDownOver]("IsFingerDownOver") {
    "o" <-> (_.o)
    unbuild(IsFingerDownOver(_1))
  }
  
  /** IsFingerUpOver converter */
  implicit case object IsFingerUpOverConverter extends CustomConverter[IsFingerUpOver]("IsFingerUpOver") {
    "o" <-> (_.o)
    unbuild(IsFingerUpOver(_1))
  }
  
  /** IsFingerUpOver converter */
  implicit case object MethodCallConverter extends CustomConverter[MethodCall]("MethodCall") {
    "name" <*> (_.name)
    "l" <--> (_.l)
    unbuild(MethodCall(_1, __2))
  }
  
  /** IsFingerUpOver converter */
  implicit case object NotConverter extends CustomConverter[Not]("Not") {
    "o" <-> (_.o)
    unbuild(Not(_1))
  }
  
  /** ParExpr converter */
  implicit case object ParExprConverter extends CustomConverter[ParExpr]("ParExpr") {
    "exprs" <--> (_.exprs)
    unbuild(ParExpr(__1))
  }
  
  /** Tuple converter */
  implicit case object TupleConverter extends CustomConverter[Tuple]("Tuple") {
    "exprs" <--> (_.exprs)
    unbuild(Tuple(__1))
  }
  
  /** TupleSelect converter */
  implicit case object TupleSelectConverter extends CustomConverter[TupleSelect]("Tuple") {
    "tuple" <-> (_.tuple)
    "index" <+> (_.index)
    unbuild(new TupleSelect(_1, _2))
  }
  
  /** UnitLiteral converter */
  implicit case object UnitLiteralConverter extends CustomConverter[UnitLiteral.type]("UnitLiteral") {
    unbuild(UnitLiteral)
  }
  
  /** NOP converter */
  implicit case object NOPConverter extends CustomConverter[NOP.type]("NOP") {
    unbuild(NOP)
  }
  
  /** TupleSelect converter */
  implicit case object ObjectLiteralConverter extends CustomConverter[ObjectLiteral]("ObjectLiteral") {
    "name" <*> (_.value.name.get)
    unbuild(new ObjectLiteral(_1))
  }
  
  /***
   * Base type converter functions (identifiers, ints, etc.)
   */
  def identifierToJson(id: Identifier): JSONObject = {
    val json = new JSONObject()
    json.put("name", id.name)
    json.put("id", id.id)
    json
  }
  def jsonToIdentifier(json: JSONObject): Identifier = {
    FreshIdentifier(json.getString("name"), json.getInt("id"))
  }
  def identifierSeqToJson(name: String,ids: Seq[Identifier]): JSONObject = {
    val json = new JSONObject()
    for(id<-ids;j = identifierToJson(id)) json.accumulate(name, j)
    json
  }
  def exprSeqToJson(name: String, ids: Seq[Expr]): JSONObject = {
    val json = new JSONObject()
    for(id<-ids;j = exprToJson(id)) json.accumulate(name, j)
    json
  }
  
  def stringToJson(str: String): JSONObject = {
    val json = new JSONObject()
    json.put("s", str)
  }
  def jsonToString(json: JSONObject): String = {
    json.getString("s")
  }
  
  def booleanToJson(b: Boolean): JSONObject = {
    val json = new JSONObject()
    json.put("value", b)
  }
  def jsonToBoolean(json: JSONObject): Boolean = {
    json.getBoolean("value")
  }
  def intToJson(b: Int): JSONObject = {
    val json = new JSONObject()
    json.put("value", b)
  }
  def jsonToInt(json: JSONObject): Int = {
    json.getInt("value")
  }
  def floatToJson(b: Float): JSONObject = {
    val json = new JSONObject()
    json.put("value", b)
  }
  def jsonToFloat(json: JSONObject): Float = {
    json.getDouble("value").toFloat
  }
  
  def toJson[T <: Expr](e: T)(implicit converter: Converter[T]): JSONObject = {
    converter(e)
  }

  def exprToJson(expr: Expr): JSONObject = expr match {
    case e:BooleanLiteral => toJson(e)
    case e:IntegerLiteral => toJson(e)
    case e:Variable => toJson(e)
    case e:Block => toJson(e)
    case e:If    => toJson(e)
    case e:Equals => toJson(e)
    case e:Plus => toJson(e)
    case e:Minus => toJson(e)
    case e:Times => toJson(e)
    case e:Div => toJson(e)
    case e:Mod => toJson(e)
    
    case e:LessThan => toJson(e)
    case e:GreaterThan => toJson(e)
    case e:LessEq => toJson(e)
    case e:GreaterEq => toJson(e)
    case e:And => toJson(e)
    case e:Or => toJson(e)
    
    case e:Collision => toJson(e)
    case e:Contains => toJson(e)
    case e:Let => toJson(e)
    case e:Copy => toJson(e)
    case e:ApplyForce => toJson(e)
    case e:Assign => toJson(e)
    case e:ContainingCell => toJson(e)
    
    case e:FingerMoveOver => toJson(e)
    case e:FingerUpOver => toJson(e)
    case e:FingerDownOver => toJson(e)
    
    case e:Apply => toJson(e)
		case e:Choose => toJson(e)
		case e:Column => toJson(e)
		
		case e:Count => toJson(e)
		case e:Debug => toJson(e)
		case e:Delete => toJson(e)
		case e:Find => toJson(e)
		case e:FloatLiteral => toJson(e)
		case e:Forall => toJson(e)
		case e:Foreach => toJson(e)
		case e:IsFingerDownOver => toJson(e)
		
		case e:IsFingerUpOver => toJson(e)
		case e:MethodCall => toJson(e)
		case e@NOP => toJson(e) 
		case e:Not => toJson(e)
		case e:ObjectLiteral => toJson(e)
		case e:ParExpr => toJson(e)
		case e:Row => toJson(e)
		case e:Select => toJson(e)
		case e:StringLiteral => toJson(e)
		
		case e:Tuple => toJson(e)
		case e:TupleSelect => toJson(e)
		case e@UnitLiteral => toJson(e)
    //
  }

  def jsonToExpr(json: JSONObject)(implicit ctx: SerializerContext): Expr = json match {
    case BooleanLiteralConverter(e) => e
    case IntegerLiteralConverter(e) => e
    case VariableConverter(e) => e
    case BlockConverter(e) => e
    case IfConverter(e) => e
    case EqualsConverter(e) => e
    case PlusConverter(e) => e
    case MinusConverter(e) => e
    case TimesConverter(e) => e
    case DivConverter(e) => e
    case ModConverter(e) => e
    
    case LessThanConverter(e) => e
    case GreaterThanConverter(e) => e
    case LessEqConverter(e) => e
    case GreaterEqConverter(e) => e
    case AndConverter(e) => e
    case OrConverter(e) => e
    
    case CollisionConverter(e) => e
    case ContainsConverter(e) => e
    case LetConverter(e) => e
    case CopyConverter(e) => e
    case ApplyForceConverter(e) => e
    case AssignConverter(e) => e
    case ContainingCellConverter(e) => e
    
    case FingerMoveOverConverter(e) => e
    case FingerUpOverConverter(e) => e
    case FingerDownOverConverter(e) => e
    
    case ApplyConverter(e) => e
		case ChooseConverter(e) => e
		case ColumnConverter(e) => e
		
		case CountConverter(e) => e
		case DebugConverter(e) => e
		case DeleteConverter(e) => e
		case FindConverter(e) => e
		case FloatLiteralConverter(e) => e
		case ForallConverter(e) => e
		case ForeachConverter(e) => e
		case IsFingerDownOverConverter(e) => e
		
		case IsFingerUpOverConverter(e) => e
		case MethodCallConverter(e) => e
		case NOPConverter(e) => e 
		case NotConverter(e) => e
		case ObjectLiteralConverter(e) => e
		case ParExprConverter(e) => e
		case RowConverter(e) => e
		case SelectConverter(e) => e
		case StringLiteralConverter(e) => e
		
		case TupleConverter(e) => e
		case TupleSelectConverter(e) => e
		case UnitLiteralConverter(e) => e
		case _ => throw new Exception(s"Json could not parse $json")
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
