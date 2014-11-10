package ch.epfl.lara.synthesis.kingpong.serialization

import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.expression.Extractors._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.PhysicalWorld
import org.json.{JSONObject, JSONTokener}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ArrayBuffer, HashMap => MMap}
import org.jbox2d.dynamics.BodyType
import android.graphics.Canvas
import android.graphics.Paint
import android.graphics.Bitmap
import android.graphics.drawable.BitmapDrawable
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import android.graphics.BitmapFactory
import ch.epfl.lara.synthesis.kingpong.GameView
import android.util.Base64
import android.graphics.drawable.Drawable

object GameSerializer {
  private val CLASS_TAG = "__class__"

  /**
   * the context used to create the game
   */
  class SerializerContext(
    val objects: ArrayBuffer[GameObject] = ArrayBuffer[GameObject](),
    val map: MMap[String, GameObject] = MMap[String, GameObject](),
    val identifiers: MMap[(String, Int), Identifier] = MMap[(String, Int), Identifier](),
    val categories: MMap[String, CategoryObject] = MMap[String, CategoryObject]()
    ) {
    def add(o: GameObject) = {
      if(o != null) {
	      objects += o
	      map += o.name.get -> o
      }
    }
    def add(c: CategoryObject) = {
      if(c != null && !(categories contains c.name)) {
        categories += c.name -> c
      }
    }
    def getOrElseUpdate(i: Identifier): Identifier = {
      identifiers.get((i.name, i.id)) match {
        case Some(id) => id
        case None => 
          identifiers += ((i.name, i.id) -> i)
          i
      }
    }
    def copy: SerializerContext = {
      new SerializerContext(objects, map, identifiers.clone(), categories)
    }
  }
    
  sealed trait Converter[T] {
    def apply(expr: T): JSONObject
    def unapply(json: JSONObject)(implicit ctx: SerializerContext): Option[T]
  }
  /** A converter to convert from JSONObject to Expr and vice-versa*/
  abstract class ConverterBuilder[T <: Expr : Builder](TAG: String) extends Converter[T] {
    def apply(expr: T): JSONObject = {
      val json = new JSONObject().put(CLASS_TAG, TAG)
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
      def <-->(t: T => Seq[Expr]): Unit = mapSeq += ((s, t))
    }
  }
  
  /** A converter to convert from JSONObject to Expr and vice-versa*/
  abstract class CustomConverter[T](TAG: String) extends Converter[T] {
    def apply(expr: T): JSONObject = {
      val json = new JSONObject().put(CLASS_TAG, TAG)
      for((str, f) <- map) {
        json.put(str, f(expr))
      }
      json
    }
    implicit var implicitJSON: JSONObject = null
    private var unbuilder: () => T = null
    private var beforeUnbuilder: () => Unit = null
    private var afterUnbuilder: () => Unit = null
    implicit var implicitContext:  SerializerContext = null
    def unbuild(f: => T) = unbuilder = {() => f}
    def beforeUnbuild(f: => Unit) = beforeUnbuilder = () => f
    def afterUnbuild(f: => Unit) = afterUnbuilder = () => f
    //def unbuild(f: SerializerContext => T) = unbuilder = f
    //def unbuild(json: JSONObject): T
        
    def unapply(json: JSONObject)(implicit ctx: SerializerContext): Option[T] = {
      implicitContext = ctx
       if(json.getString(CLASS_TAG) == TAG) {
         implicitJSON = json
         if(beforeUnbuilder != null) beforeUnbuilder()
         val res = Some(unbuilder())
         if(afterUnbuilder != null) afterUnbuilder()
         res
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
    def _4(implicit json: JSONObject): JSONObject = json.getJSONObject(map(3)._1)
    def _5(implicit json: JSONObject): JSONObject = json.getJSONObject(map(4)._1)
    def _6(implicit json: JSONObject): JSONObject = json.getJSONObject(map(5)._1)
    def _7(implicit json: JSONObject): JSONObject = json.getJSONObject(map(6)._1)
    
    def __1(implicit json: JSONObject): List[JSONObject] = json.getJSONObject(map(0)._1).getJSONObjectSeq(map(0)._1).toList
    def __2(implicit json: JSONObject): List[JSONObject] = json.getJSONObject(map(1)._1).getJSONObjectSeq(map(1)._1).toList
    def __3(implicit json: JSONObject): List[JSONObject] = json.getJSONObject(map(2)._1).getJSONObjectSeq(map(2)._1).toList
    implicit def internalJsonToIdentifier(json: JSONObject): Identifier = {
      val id = jsonToIdentifier(json)
      implicitContext.getOrElseUpdate(id)
    }
    implicit def internalJsonToExpr(json: JSONObject): Expr = jsonToExpr(json)
    implicit def internalJsonToString(json: JSONObject): String = jsonToString(json)
    implicit def internalJsonToBoolean(json: JSONObject): Boolean = jsonToBoolean(json)
    implicit def internalJsonToInt(json: JSONObject): Int = jsonToInt(json)
    implicit def internalJsonToFloat(json: JSONObject): Float = jsonToFloat(json)
    implicit def internalJsonToExprSeq(json: List[JSONObject]): List[Expr]= json.map(jsonToExpr)
    implicit def internalJsonToIdentifierList(json: List[JSONObject]): List[Identifier]= json.map(jsonToIdentifier).toList
    implicit def internalJsonToGameObject(json: JSONObject): GameObject = implicitContext.map.getOrElse(jsonToString(json), null)
    implicit def internalJsonToCategory(json: JSONObject): CategoryObject = implicitContext.categories.getOrElse(jsonToString(json), null)
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
  implicit case object CollidingConverter extends ConverterBuilder[Colliding]("Colliding") with BinaryConverter[Colliding]
  implicit case object OutOfCollisionConverter extends ConverterBuilder[OutOfCollision]("OutOfCollision") with BinaryConverter[OutOfCollision]
  implicit case object ContainsConverter extends ConverterBuilder[Contains]("Contains") with BinaryConverter[Contains]
  implicit case object ContainsTotallyConverter extends ConverterBuilder[ContainsTotally]("ContainsTotally") with BinaryConverter[ContainsTotally]
  
  
   
  trait SaveRestoreIdentifierContext[T] extends CustomConverter[T] {
    var savedContext: SerializerContext = null
    beforeUnbuild{
      savedContext = implicitContext
      implicitContext = implicitContext.copy
    }
    afterUnbuild{
      implicitContext = savedContext
    }
  }
  
  implicit case object LetConverter extends CustomConverter[Let]("Let") with SaveRestoreIdentifierContext[Let] {
    "id" <=> (_.id)
    "expr" <-> (_.expr)
    "body" <-> (_.body)
    unbuild(Let(_1, _2, _3))
  }
  implicit case object CopyConverter extends CustomConverter[Copy]("Copy") with SaveRestoreIdentifierContext[Copy] {
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
    "exprs" <--> (_.exprs)
  }
  
  /** Choose converter */
  implicit case object ChooseConverter extends CustomConverter[Choose]("Choose") {
    "vars" <--> (_.vars.toSeq.asInstanceOf[Seq[Expr]])
    "constraint" <-> (_.constraint)
    "code" <-> (_.code)
    unbuild(Choose(__1, _2, _3))
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
  
  /** Forall converter */
  implicit case object ForallConverter extends CustomConverter[Forall]("Forall") with SaveRestoreIdentifierContext[Forall] {
    "category" <*> (_.category.name)
    "id" <=> (_.id)
    "body" <-> (_.body)
    unbuild(Forall(_1, _2, _3))
  }
  
  /** Foreach converter */
  implicit case object ForeachConverter extends CustomConverter[Foreach]("Foreach") with SaveRestoreIdentifierContext[Foreach] {
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
  implicit case object TupleSelectConverter extends CustomConverter[TupleSelect]("TupleSelect") {
    "tuple" <-> (_.tuple)
    "index" <+> (_.index)
    unbuild(new TupleSelect(_1, _2))
  }
  
  /** UnitLiteral converter */
  implicit case object UnitLiteralConverter extends CustomConverter[UnitLiteral.type]("UnitLiteral") {
    unbuild(UnitLiteral)
  }
  
  /** TupleSelect converter */
  implicit case object ObjectLiteralConverter extends CustomConverter[ObjectLiteral]("ObjectLiteral") {
    "value" <*> (_.value.name.get)
    unbuild(new ObjectLiteral(_1))
  }
  
  
  /**
   * Other kind of converters
   */
  /** TupleSelect converter */
  implicit case object DrawingElementConverter extends CustomConverter[DrawingElement]("DrawingElement") {
    "time" <+> (_.time)
    "fromx" <~> (_.fromx)
    "fromy" <~> (_.fromx)
    "tox" <~> (_.fromx)
    "toy" <~> (_.fromx)
    "width" <~> (_.fromx)
    "color" <+> (_.color)
    unbuild(DrawingElement(_1, _2, _3, _4, _5, _6, _7))
  }
  
  /**
   * Other kind of converters
   */
  /** TupleSelect converter */
  implicit case object SoundRecordedConverter extends CustomConverter[SoundRecorded]("SoundRecorded") {
    "number" <+> (_.number)
    "startTime" <+> (_.startTime)
    "endTime" <+> (_.endTime)
    "uri" <*> (_.uri)
    unbuild(SoundRecorded(null, _1, _2, _3, _4))
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
  
  def toJson[T](e: T)(implicit converter: Converter[T]): JSONObject = {
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
    case e:Colliding => toJson(e)
    case e:OutOfCollision => toJson(e)
    case e:Contains => toJson(e)
    case e:ContainsTotally => toJson(e)
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

		case e:Debug => toJson(e)
		case e:Delete => toJson(e)
		case e:Find => toJson(e)
		case e:FloatLiteral => toJson(e)
		case e:Forall => toJson(e)
		case e:Foreach => toJson(e)
		case e:IsFingerDownOver => toJson(e)
		
		case e:IsFingerUpOver => toJson(e)
		case e:MethodCall => toJson(e)
		case e:Not => toJson(e)
		case e:ObjectLiteral => toJson(e)
		case e:ParExpr => toJson(e)
		case e:Select => toJson(e)
		case e:StringLiteral => toJson(e)
		
		case e:Tuple => toJson(e)
		case e:TupleSelect => toJson(e)
		case e@UnitLiteral => toJson(e)
    // Please also add any element in the method below jsonToExpr.
  }
  
  def fromJson[T](json: JSONObject)(implicit converter: Converter[T], ctx: SerializerContext): Option[T] = converter.unapply(json)

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
    case CollidingConverter(e) => e
    case OutOfCollisionConverter(e) => e
    case ContainsConverter(e) => e
    case ContainsTotallyConverter(e) => e
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

		case DebugConverter(e) => e
		case DeleteConverter(e) => e
		case FindConverter(e) => e
		case FloatLiteralConverter(e) => e
		case ForallConverter(e) => e
		case ForeachConverter(e) => e
		case IsFingerDownOverConverter(e) => e
		
		case IsFingerUpOverConverter(e) => e
		case MethodCallConverter(e) => e
		case NotConverter(e) => e
		case ObjectLiteralConverter(e) => e
		case ParExprConverter(e) => e
		case SelectConverter(e) => e
		case StringLiteralConverter(e) => e
		
		case TupleConverter(e) => e
		case TupleSelectConverter(e) => e
		case UnitLiteralConverter(e) => e
		case _ => throw new Exception(s"Json could not parse $json")
  }
  
 
  def categoryToJson(category: CategoryObject): JSONObject = {
    val json = new JSONObject().put(CLASS_TAG, "category")
    def jsonPutSimple(json: JSONObject, name: String, e: Expr) = e match {
      case FloatLiteral(f) => json.put(name, f)
      case IntegerLiteral(i) => json.put(name, i)
      case BooleanLiteral(b) => json.put(name, b)
      case _ => json.put(name, exprToJson(e))
    }
    
    json.put("name", category.name)
    jsonPutSimple(json, "angle", category.angle)
		jsonPutSimple(json, "width", category.width)
		jsonPutSimple(json, "height", category.height)
		jsonPutSimple(json, "cellWidth", category.cellWidth)
		jsonPutSimple(json, "cellHeight", category.cellHeight)
		jsonPutSimple(json, "radius", category.radius)
		jsonPutSimple(json, "value", category.value)
		jsonPutSimple(json, "randomMinValue", category.randomMinValue)
		jsonPutSimple(json, "randomMaxValue", category.randomMaxValue)
		jsonPutSimple(json, "visible", category.visible)
		jsonPutSimple(json, "velocity", category.velocity)
		jsonPutSimple(json, "angularVelocity", category.angularVelocity)
		jsonPutSimple(json, "density", category.density)
		jsonPutSimple(json, "friction", category.friction)
		jsonPutSimple(json, "restitution", category.restitution)
		jsonPutSimple(json, "linearDamping", category.linearDamping)
		jsonPutSimple(json, "fixedRotation", category.fixedRotation)
		jsonPutSimple(json, "color", category.color)
		jsonPutSimple(json, "sensor", category.sensor)
		json.put("type", if(category.tpe == BodyType.STATIC) "STATIC" else if(category.tpe == BodyType.DYNAMIC) "DYNAMIC" else "KINEMATIC" )
		jsonPutSimple(json, "stroke_width", category.stroke_width)
		jsonPutSimple(json, "color_drawing", category.color_drawing)
		jsonPutSimple(json, "recording", category.recording)
		jsonPutSimple(json, "language", category.language)
		jsonPutSimple(json, "text", category.text)
		jsonPutSimple(json, "time", category.time)
		jsonPutSimple(json, "displayName", category.displayName)
		json
  }
  
  def jsonToCategory(json: JSONObject, game: Game)(implicit ctx: SerializerContext): CategoryObject = {
    val name = json.getString("name")
    var created = false
    val jexpr = (str: String) => jsonToExprWithShortcut(json, str)
    val category = ctx.categories.getOrElse(name, { created = true; new CategoryObject(
        game,
        name,
        jexpr("angle"),
				jexpr("width"),
				jexpr("height"),
				jexpr("cellWidth"),
				jexpr("cellHeight"),
				jexpr("radius"),
				jexpr("value"),
				jexpr("randomMinValue"),
				jexpr("randomMaxValue"),
				jexpr("visible"),
				jexpr("velocity"),
				jexpr("angularVelocity"),
				jexpr("density"),
				jexpr("friction"),
				jexpr("restitution"),
				jexpr("linearDamping"),
				jexpr("fixedRotation"),
				jexpr("color"),
				jexpr("sensor"),
				{ val s = json.optString("type", json.optString("tpe")); if(s == "STATIC") BodyType.STATIC else if(s == "DYNAMIC") BodyType.DYNAMIC else BodyType.KINEMATIC},
				jexpr("stroke_width"),
				jexpr("color_drawing"),
				jexpr("recording"),
				jexpr("language"),
				jexpr("text"),
				jexpr("time"),
				if(json.has("displayName")) jexpr("displayName") else name
				)}
    )
    if(created) {
      ctx.categories += name -> category
      category
    } else {
      category
    }
  }
  
    
  // Returns the value as an Expr. Can handle unambiguous basic types.
  def jsonToExprWithShortcut(json: JSONObject, name: String)(implicit ctx: SerializerContext): Expr = {
    val maybeString = json.optString(name, null)
    val initValue = try {
      if(maybeString != null && maybeString.length() > 0) {
        if(maybeString.indexOf('.') >= 0) {
          FloatLiteral(maybeString.toFloat)
        } else if(maybeString.indexOf("a") >= 0 || maybeString.indexOf("u") >= 0){
          BooleanLiteral(maybeString.toBoolean)
        } else {
          IntegerLiteral(maybeString.toInt)
        }
      } else {
        jsonToExpr(json.getJSONObject(name))
      }
    } catch {
      case e: Exception =>
        jsonToExpr(json.getJSONObject(name))
    }
    initValue
  }
  
  // Transforms a game object to a JSON object
  def gameObjectToJson(g: GameObject): JSONObject = {
    val cl = g.getClass().getName()
    val json = new JSONObject().put(CLASS_TAG, cl)
    json.put("category", g.category.name)
    for(p <- g.properties) {
      p match {
        case p:AliasProperty[_] => // Store nothing
        case p:ConstProperty[_] => //json.put(p.name, exprToJson(p.expr))
        case p:NamedProperty[_] => //json.put(p.name, exprToJson(p.expr))
        case p:HistoricalProperty[_] =>
          p.init match {
            case IntegerLiteral(i) =>
              json.put(p.name, i)
            case FloatLiteral(f) =>
              json.put(p.name, f)
            case BooleanLiteral(b) =>
              json.put(p.name, b)
            case _ =>
              json.put(p.name, exprToJson(p.init))
          }
        case _ => println(s"Unknown property not stored : $p")
      }
    }
    g match {
      case p: PhysicalObject =>
        json.put("type", if(p.tpe == BodyType.STATIC) "STATIC" else if(p.tpe == BodyType.DYNAMIC) "DYNAMIC" else "KINEMATIC" )
      case _ =>
    }
    g match {
      case g: DrawingObject =>
        val elements = g.getDrawingElements
        for(elem <- elements) {
          json.accumulate("DrawingElements", toJson(elem))
        }
      case g: SoundRecorder =>
        val elements = g.getRecordings
        for(elem <- elements) {
          // TODO : Save sounds apart for zipping
          json.accumulate("SoundsRecorded", toJson(elem))
        }
      case d: Array2D =>
        
      case _ =>
    }
    json
  }

  // Converts JSON to a game object.
  def jsonToGameObject(json: JSONObject)(implicit game: Game, ctx: SerializerContext): Option[GameObject] = {
    val cl = json.get(CLASS_TAG)
    println("Loading as game object:" + json)
    implicit val planned = GameObject.PLANNED_SINCE_BEGINNING;
    val category: CategoryObject = ctx.categories.getOrElse(if(json has "category") json getString "category" else "", DefaultCategory("", game))
    val obj: GameObject =
           if(cl == classOf[Rectangle].getName()) { rectangle(category)("", 0, 0)
    } else if(cl == classOf[Circle].getName()) {    circle(category)("", 0, 0)
    } else if(cl == classOf[Character].getName()) { character(category)("", 0, 0)
    } else if(cl == classOf[IntBox].getName()) { intbox(category)("", 0, 0)
    } else if(cl == classOf[StringBox].getName()) { stringbox(category)("", 0, 0)
    } else if(cl == classOf[BooleanBox].getName()) { booleanbox(category)("", 0, 0)
    } else if(cl == classOf[Joystick].getName()) { joystick(category)("", 0, 0)
    } else if(cl == classOf[RandomGenerator].getName()) { randomGenerator(category)("", 0, 0)
    } else if(cl == classOf[Array2D].getName()) {
      val numRows =jsonToExpr(json.getJSONObject("numRows"))
      val numCols = jsonToExpr(json.getJSONObject("numColumns"))
      array(category)("", 0, 0, numCols, numRows)
    } else if(cl == classOf[SoundTTS].getName()) { soundTTS(category)("", 0, 0)
    } else if(cl == classOf[DrawingObject].getName()) { drawingObject(category)("", 0, 0)
    //} else if(cl == classOf[SoundRecorded].getName()) { soundrecorded(category)("", 0, 0)
    } else if(cl == classOf[SoundRecorder].getName()) { soundRecorder(category)("", 0, 0)
    } else null
    if(category.name == "" && obj != null) {
      val category = DefaultCategory(obj)
      obj.setCategory(category)
    }
    if(obj != null) {
	    for(p <- obj.properties) {
	      p match {
	        case p:AliasProperty[_] => // Store nothing
	        case p:ConstProperty[_] => // p.jsonToInt(json.get(p.name))
	        case p:HistoricalProperty[_] => 
            val initValue = jsonToExprWithShortcut(json, p.name)
            p.setInit(initValue)
	        case _ => println(s"Unknown property not stored : $p")
	      }
	    }
	    obj match {
	      case p: PhysicalObject =>
	        p.tpe = { val s = json.optString("type", json.optString("tpe")); if(s == "STATIC") BodyType.STATIC else if(s == "DYNAMIC") BodyType.DYNAMIC else BodyType.KINEMATIC}
	      case _ =>
	    }
	    obj match {
	      case obj: DrawingObject =>
	        for(j <- json.getJSONObjectSeq("DrawingElements"); d <- fromJson[DrawingElement](j)) {
	          obj.addDrawingElement(d)
	        }
	      case obj: SoundRecorder =>
	        for(j <- json.getJSONObjectSeq("SoundsRecorded"); d <- fromJson[SoundRecorded](j)) {
	          // TODO : Recover sounds from zip before inserting them there.
	          d.recorder = obj
	          obj.recordings += d
	        }
	      case _ =>
	    }
	    Some(obj)
    } else None
  }
  
  def bitmapToJson(key: Int, bitmap: Bitmap): JSONObject = {
    val json = new JSONObject().put(CLASS_TAG, "bitmap")
    json.put("id", key)
    json.put("content", BitMapToString(bitmap))
    json
  }
  
  def jsonToBitmap(json: JSONObject): Option[(Int, Bitmap)] = {
    val c = json.get(CLASS_TAG)
    if(!json.has("id") || !json.has("content")) return None
    Some((json.getInt("id"), StringToBitMap(json.getString("content"))))
  }

  /**
   * Converts a game to JSON
   */
  def gameToJson(game: Game, bitmaps: List[(Int, Bitmap)]): JSONObject = {
    val json = new JSONObject().put(CLASS_TAG, "Game")
    
    for(obj <- game.objects if obj.plannedFromBeginning == GameObject.PLANNED_SINCE_BEGINNING) {
      json.accumulate("objects", gameObjectToJson(obj))
    }
    for((key, bitmap) <- bitmaps if bitmap != null) {
      json.accumulate("bitmaps", bitmapToJson(key, bitmap))
    }
    for(cat <- game.objects.map(_.category).toList.distinct if cat.name != null) {
      json.accumulate("categories", categoryToJson(cat))
    }
    for(rule <- game.rules) {
      json.accumulate("rules", exprToJson(rule))
    }
    json
  }
    
  /**
   * Converts a json to a game
   */
  def jsonToGame(json: JSONObject): Option[(Game, List[(Int, Bitmap)])] = {
    if(json.get(CLASS_TAG) != "Game") return None;
    implicit val game = new Game { val world = new PhysicalWorld(new org.jbox2d.common.Vec2(0, 0f)) }
    implicit val ctx = new SerializerContext
    if(json.has("categories")) {
	    for(j <- json.getJSONObjectSeq("categories");
	        cat = jsonToCategory(j, game)) {
	      ctx.add(cat)
	    }
    }
    if(json.has("objects")) {
	    for(j <- json.getJSONObjectSeq("objects");
	        someobj = jsonToGameObject(j);
	        obj <- someobj) {
	      ctx.add(obj)
	    }
    }
    if(json.has("rules")) {
	    for(j <- json.getJSONObjectSeq("rules")) {
	      game.addRule(jsonToExpr(j))
	    }
    }
    val snd = if(json.has("bitmaps")) {
      (for(json <- json.getJSONObjectSeq("bitmaps"); res <- jsonToBitmap(json)) yield res).toList
    } else Nil
    Some((game, snd))
  }

  private implicit class RichJSONObject(val obj: JSONObject) extends AnyVal {
    def getJSONObjectSeq(name: String): Seq[JSONObject] = {
      if(obj.optJSONArray(name) != null) {
        val array = obj.optJSONArray(name)
        for (i <- 0 until array.length) yield array.getJSONObject(i)
      } else if(obj.optJSONObject(name) != null) {
        List(obj.optJSONObject(name))
      } else Nil
    }
  }
  
  /**
   * Parses a game from a file.
   */
  def loadGame(content: String, callbackProgress: (Int, Int, String) => Unit = null): (Game, List[(Int, Bitmap)]) = {
    if(callbackProgress != null) callbackProgress(10, 100, "Parsing json...")
    val json = new JSONTokener(content).nextValue().asInstanceOf[JSONObject]
    if(callbackProgress != null) callbackProgress(50, 100, "Recreating game...")
    val game = jsonToGame(json)
    if(callbackProgress != null) callbackProgress(100, 100, "Finish!")
    game.getOrElse(null)
  }
  
  /**
   * Writes a game to a file.
   */
  def saveGame(game: Game, bitmaps: List[(Int, Bitmap)], writer: String => Unit, callbackProgress: (Int, Int) => Unit = null): Unit = {
    if(callbackProgress != null) callbackProgress(10, 100)
    val json = gameToJson(game, bitmaps)
    if(callbackProgress != null) callbackProgress(50, 100)
    val content = json.toString
    if(callbackProgress != null) callbackProgress(80, 100)
    writer(content)
    if(callbackProgress != null) callbackProgress(100, 100)
  }
  
  /**
   * @param bitmap
   * @return converting bitmap and return a string
   */
  def BitMapToString(bitmap: Bitmap): String = {
    val baos=new  ByteArrayOutputStream();
    val w = bitmap.getWidth()
    val h = bitmap.getHeight()
    val r = Math.sqrt(w * h / 10000).toFloat // 10k pixels images max (1k in memory)
    val newBitmap = Bitmap.createScaledBitmap(bitmap, (w/r).toInt, (h/r).toInt, true)
    newBitmap.compress(Bitmap.CompressFormat.PNG, 100, baos);
    newBitmap.recycle();
    val b=baos.toByteArray();
    val temp=Base64.encodeToString(b, Base64.DEFAULT);
    return temp;
  }
  
  /**
   * @param encodedString
   * @return bitmap (from given string)
   */
  def StringToBitMap(encodedString: String): Bitmap = {
	  try{
	    val encodeByte = Base64.decode(encodedString,Base64.DEFAULT)
	    val bitmap=BitmapFactory.decodeByteArray(encodeByte, 0, encodeByte.length)
	    bitmap
	  } catch {
	  case e: Exception =>
	    println(e.getMessage());
	    null
	  }
  }
}
