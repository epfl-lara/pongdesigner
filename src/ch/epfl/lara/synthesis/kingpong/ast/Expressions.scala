package ch.epfl.lara.synthesis.kingpong.ast;

import scala.collection.immutable.List
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.GameShapes._
import android.util.Log

object EScreenHeight {
  def apply(): Expression = EIdent("screenHeight")
}
object EScreenWidth {
  def apply(): Expression = EIdent("screenWidth")
}
object Expression {
  /**
   * When the expression is computed, the type it can contain
   */
  final val NO_TYPE = 0
  final val SHAPE_TYPE = 1
  final val NUMBER_TYPE = 2
  final val BOOLEAN_TYPE = 3
  final val STRING_TYPE = 4
  final val ARENA_TYPE = 5
  
  /** The None value */
  final val NONE = ENone()
  
  /** If the ParallelExpressions displays all the expressions when converted to a string*/
  final val PRINTALL_PARALLEL = false
  
  /** Executes a list of Expressions in a given context */
  def execute(code: List[Expression], context: HashMap[String, Expression]) = {
    code foreach {
      expr =>
        
        Expression.evaluateExpression(context, expr)
    }
  }
  
  /** Evaluates / Executes an expression and store its result inside itself */
  def evaluateExpression(context: HashMap[String, Expression], t: Expression): Expression = {
    t match {
      case CompiledBlock(f) => f()
      case CompiledBoolean(f) => t store (f())
      case EConstantBoolean(value) => t store value
      case EConstantNumber(value) => t store value
      case ERandomInterval(minValue, maxValue) => t store (Game.randomgenerator.nextFloat()*(maxValue - minValue) + minValue)
      case ERandomNumber(values) => t store (values(Game.randomgenerator.nextInt(values.size)))
      case EConstantString(value) => t store value
      case EIdentShape(value) => t store value
      case EIdent(ident) => 
        context.getOrElse(ident, null) match {
          case null => 
          case EConstantBoolean(value) => t store value
          case EConstantNumber(value) => t store value
          case EConstantString(value) => t store value
          case EIdentShape(value) => t store value
          case EIdentArena(value) => t store value
          case _ =>
        }        
      case EApply(ESelect(expression, method), List(argument)) =>
        evaluateExpression(context, argument)
        evaluateExpression(context, expression)
        //Log.d("Expressions.scala", "Recognizing operation '" + method + "' between " + expression + " (type = " + expression.typeExpr + ") and " + argument + " (type = " + argument.typeExpr + ")")
        (expression.typeExpr, method, argument.typeExpr) match {
          case (NO_TYPE, "setCurrentArena", SHAPE_TYPE) =>
            
          case (SHAPE_TYPE, "x_$eq", NUMBER_TYPE) =>
            expression.shape_value.x = argument.number_value
            t store NO_TYPE
          case (SHAPE_TYPE, "y_$eq", NUMBER_TYPE) =>
            expression.shape_value.y = argument.number_value
            t store NO_TYPE
          case (SHAPE_TYPE, "color_$eq", NUMBER_TYPE) =>
            expression.shape_value.color = argument.number_value.toInt
            t store NO_TYPE
          case (SHAPE_TYPE, "velocity_$eq", NUMBER_TYPE) =>
            expression.shape_value.velocity = argument.number_value
            t store NO_TYPE
          case (SHAPE_TYPE, "velocity_x_$eq", NUMBER_TYPE) =>
            expression.shape_value.velocity_x = argument.number_value
            t store NO_TYPE
          case (SHAPE_TYPE, "velocity_y_$eq", NUMBER_TYPE) =>
            expression.shape_value.velocity_y = argument.number_value
            t store NO_TYPE
          case (SHAPE_TYPE, "angle_$eq", NUMBER_TYPE) =>
            expression.shape_value.angle = argument.number_value
            t store NO_TYPE
          case (SHAPE_TYPE, "noVelocity_$eq", BOOLEAN_TYPE) =>
            expression.shape_value.noVelocity = argument.boolean_value
            t store NO_TYPE
          case (SHAPE_TYPE, "visible_$eq", BOOLEAN_TYPE) =>
            expression.shape_value.visible = argument.boolean_value
            t store NO_TYPE
          case (SHAPE_TYPE, "radius_$eq", NUMBER_TYPE) =>
            expression.shape_value.asInstanceOf[Circle].radius = argument.number_value.toInt
            t store NO_TYPE
          case (SHAPE_TYPE, "value_$eq", NUMBER_TYPE) =>
            expression.shape_value.asInstanceOf[IntegerBox].value = argument.number_value.toInt
            t store NO_TYPE
          case (SHAPE_TYPE, "width_$eq", NUMBER_TYPE) =>
            expression.shape_value.asInstanceOf[Rectangular].width = argument.number_value.toInt
            t store NO_TYPE
          case (SHAPE_TYPE, "height_$eq", NUMBER_TYPE) =>
            expression.shape_value.asInstanceOf[Rectangular].height = argument.number_value.toInt
            t store NO_TYPE
          case (SHAPE_TYPE, "text_$eq", STRING_TYPE) =>
            expression.shape_value.asInstanceOf[TextBox].text = argument.string_value
            t store NO_TYPE
          case (SHAPE_TYPE, "$plus$eq", NUMBER_TYPE) =>
            expression.shape_value match {
              case s:IntegerBox => s += argument.number_value.toInt
              case _ => 
            }
            t store NO_TYPE
          case (NUMBER_TYPE, op, NUMBER_TYPE) =>
            //Log.d("Expressions.scala", "number operation!")
            val a = expression.number_value
            val b = argument.number_value
            op match {
              case "$plus" => t store (a + b)
                
              case "$minus" => t store (a - b)
              case "$times" => t store (a * b)
              case "$div" => t store (a / b)
              case "$greater" => t store (a > b)
              case "$less" =>
                //Log.d("Comparison", "Comparison between " + a + " < " + b)
                t store (a < b)
                //Log.d("Comparison", "Result is " + t.boolean_value)
              case "$greater$eq" => t store (a >= b)
              case "$less$eq" => t store (a <= b)
              case "$eq$eq" => t store (a == b)
              case "$bang$eq" => t store (a != b)
              case "$eq" | "$plus$eq" | "$minus$eq" | "$times$eq" | "$div$eq" =>
                val result = op match {
                  case "$plus$eq" =>
                    a + b
                  case "$minus$eq" =>
                    a - b
                  case "$times$eq" =>
                    a * b
                  case "$div$eq" =>
                    a / b
                  case "$eq" =>
                    b
                }
                expression match {
                  case ESelect(subexpr, "x") => subexpr.shape_value.x = result
                  case ESelect(subexpr, "y") => subexpr.shape_value.y = result
                  case ESelect(subexpr, "color") => subexpr.shape_value.color = result.toInt
                  case ESelect(subexpr, "velocity") => subexpr.shape_value.velocity = result
                  case ESelect(subexpr, "velocity_x") =>subexpr.shape_value.velocity_x = result
                  case ESelect(subexpr, "velocity_y") => subexpr.shape_value.velocity_y = result
                  case ESelect(subexpr, "angle") => subexpr.shape_value.angle = result
                  case ESelect(subexpr, "width") => subexpr.shape_value.asInstanceOf[Rectangular].width = result.toInt
                  case ESelect(subexpr, "height") => subexpr.shape_value.asInstanceOf[Rectangular].height = result.toInt
                  case ESelect(subexpr, "radius") => subexpr.shape_value.asInstanceOf[Circle].radius = result
                  case ESelect(subexpr, "value") => subexpr.shape_value.asInstanceOf[IntegerBox].value = result.toInt
                  // You don't assign to prev_* elements
                  case _ => 
                }
              case _ => throw new Exception("unrecognized number operator : '" + op + "'")
            }
          case (STRING_TYPE, op, STRING_TYPE) =>
            //Log.d("Expressions.scala", "number operation!")
            val a = expression.string_value
            val b = argument.string_value
            op match {
              case "$plus" => t store (a + b)
              case "$greater" => t store (a > b)
              case "$less" =>
                //Log.d("Comparison", "Comparison between " + a + " < " + b)
                t store (a < b)
                //Log.d("Comparison", "Result is " + t.boolean_value)
              case "$greater$eq" => t store (a >= b)
              case "$less$eq" => t store (a <= b)
              case "$eq$eq" => t store (a == b)
              case "$bang$eq" => t store (a != b)
              case "$eq" | "$plus$eq" =>
                val result = op match {
                  case "$plus$eq" =>
                    a + b
                  case "$eq" =>
                    b
                }
                expression match {
                  case ESelect(subexpr, "text") =>
                    subexpr.shape_value.asInstanceOf[TextBox].text = result
                  case ESelect(subexpr, "prev_text") =>
                    subexpr.shape_value.asInstanceOf[TextBox].prev_text = result
                  case _ => 
                }
              case _ => throw new Exception("unrecognized number operator : '" + op + "'")
            }
          case (BOOLEAN_TYPE, op, BOOLEAN_TYPE) =>
            val a = expression.boolean_value
            val b = argument.boolean_value
            //Log.d("Expressions.scala", "boolean operation! values = " + a + " and " + b)
            op match {
              case "$amp$amp" => t store (a && b)
              case "$bar$bar" => t store (a || b)
              case "$eq" =>
                expression match {
                  case ESelect(subexpr, "visible") =>
                    subexpr.shape_value.visible = b
                  case ESelect(subexpr, "prev_visible") =>
                    subexpr.shape_value.visible = b
                  case _ => 
                }
              case _ => throw new Exception("unrecognized boolean operator : '" + op + "'")
            }
          case _ => Log.d("Expressions.scala", "Unrecognized operation '" + method + "' between " + expression + " (type = " + expression.typeExpr + ") and " + argument + " (type = " + argument.typeExpr + ")")
        }
      case EApply(ESelect(expression, method), values) =>
        values foreach (evaluateExpression(context, _))
        evaluateExpression(context, expression)
        expression.typeExpr match {
          case _ => //TODO if any with multiple arguments.
        }
      case ESelect(expression, parameter) =>
        evaluateExpression(context, expression)
        expression.typeExpr match {
          case SHAPE_TYPE =>
            parameter match {
              case "x" => t store expression.shape_value.x
              case "y" => t store expression.shape_value.y              
              case "velocity" => t store expression.shape_value.velocity
              case "velocity_x" => t store expression.shape_value.velocity_x
              case "velocity_y" => t store expression.shape_value.velocity_y
              case "visible" => t store expression.shape_value.visible
              case "color" => t store expression.shape_value.color
              case "angle" => t store expression.shape_value.angle
              case "radius" => t store expression.shape_value.asInstanceOf[Circle].radius
              case "width" => t store expression.shape_value.asInstanceOf[Rectangular].width
              case "height" => t store expression.shape_value.asInstanceOf[Rectangular].height
              case "value" => t store expression.shape_value.asInstanceOf[IntegerBox].value
              case "text" => t store expression.shape_value.asInstanceOf[TextBox].text
              case "prev_x" => t store expression.shape_value.prev_x
              case "prev_y" => t store expression.shape_value.prev_y              
              case "prev_velocity" => t store expression.shape_value.prev_velocity
              case "prev_velocity_x" => t store expression.shape_value.prev_velocity_x
              case "prev_velocity_y" => t store expression.shape_value.prev_velocity_y
              case "prev_visible" => t store expression.shape_value.prev_visible
              case "prev_color" => t store expression.shape_value.prev_color
              case "prev_angle" => t store expression.shape_value.prev_angle
              case "prev_radius" => t store expression.shape_value.asInstanceOf[Circle].prev_radius
              case "prev_width" => t store expression.shape_value.asInstanceOf[Rectangular].prev_width
              case "prev_height" => t store expression.shape_value.asInstanceOf[Rectangular].prev_height
              case "prev_value" => t store expression.shape_value.asInstanceOf[IntegerBox].prev_value
              case "prev_text" => t store expression.shape_value.asInstanceOf[TextBox].prev_text
            }
          case _ => ()
        }
      case IfCode(condition, codeIfTrue, codeIfFalse) =>
        evaluateExpression(context, condition)
        condition.typeExpr match {
          case BOOLEAN_TYPE => 
            var lastExpr: Expression = Expression.NONE
            (if(condition.boolean_value) codeIfTrue else codeIfFalse) foreach { e =>
              evaluateExpression(context, e)
              lastExpr = e
            }
            if(lastExpr != Expression.NONE)  {
              t store lastExpr
            }
        }
      case ParallelExpressions(expression::q) =>
        evaluateExpression(context, expression)
        t store expression
      case _ => () 
    }
    t
  }
  
  /** Converts a list of expressions to a string representation */
  def expressionListToScalaString(code: List[Expression], indent: String, context: HashMap[String, Expression], modifiableConstants: ArrayBuffer[(Int, Int, Int, Expression)] = null, currentLine: Option[Int] = None, currentCol: Option[Int] = None): String = {
    code match {
      case Nil => ""
      case code =>
        var line = currentLine
        var col = currentCol
        var result = ""
        var first = true
        code.flatMap{
          case EApply(a@ESelect(e, "$eq"), List(ParallelExpressions(b::q))) if a == b => Nil
          case t => List(t)
        }.foreach{ expr =>
          if(!first) result += "\n"
          line = currentLine map {l => l + result.count(_ == '\n')}
          col = currentCol map {_ => 0 }
          result += expr.toScalaString(indent, context, modifiableConstants, line, col)
          first = false
        }
        result
    }
  }
}

/** An expression which allows to build code to modify the game. */
sealed trait Expression extends NotNull {
  import Expression._
  /** Operators to build an expression from a combination of two expressions */
  def +(e: Expression): Expression = EApply(ESelect(this, "$plus"), List(e))
  def -(e: Expression): Expression = EApply(ESelect(this, "$minus"), List(e))
  def *(e: Expression): Expression = EApply(ESelect(this, "$times"), List(e))
  def /(e: Expression): Expression = EApply(ESelect(this, "$div"), List(e))
  def +=(e: Expression): Expression = EApply(ESelect(this, "$plus$eq"), List(e))
  def -=(e: Expression): Expression = EApply(ESelect(this, "$minus$eq"), List(e))
  def *=(e: Expression): Expression = EApply(ESelect(this, "$times$eq"), List(e))
  def /=(e: Expression): Expression = EApply(ESelect(this, "$div$eq"), List(e))
  def !=(e: Expression): Expression = EApply(ESelect(this, "$bang$eq"), List(e))
  def >(e: Expression): Expression = EApply(ESelect(this, "$greater"), List(e))
  def <(e: Expression): Expression = EApply(ESelect(this, "$less"), List(e))
  def >=(e: Expression): Expression = EApply(ESelect(this, "$greater$eq"), List(e))
  def <=(e: Expression): Expression = EApply(ESelect(this, "$less$eq"), List(e))
  def equals(e: Expression): Expression = EApply(ESelect(this, "$eq$eq"), List(e))
  def or(e: Expression): Expression = EApply(ESelect(this, "$bar$bar"), List(e))
  def and(e: Expression): Expression = EApply(ESelect(this, "$amp$amp"), List(e))
  
  /** Checks whether a condition is none or not*/
  def isNone:Boolean = this match {
    case ENone() => true
    case _ => false
  }

  /** The type of the expression after it has been evaluated. */
  var typeExpr = NO_TYPE
  private var storedValue: Any = null
  def store(g: GameShapes.Shape): Unit = {storedValue = g; typeExpr = SHAPE_TYPE}
  def store(f: Float): Unit = {storedValue = f; typeExpr = NUMBER_TYPE}
  def store(i: Int): Unit = {storedValue = i.toFloat; typeExpr = NUMBER_TYPE}
  def store(b: Boolean): Unit = {storedValue = if(b) 1.0f else 0.0f; typeExpr = BOOLEAN_TYPE}
  def store(s: String): Unit = {storedValue = s; typeExpr = STRING_TYPE}
  def store(s: Arena): Unit = {storedValue = s; typeExpr = ARENA_TYPE}
  def store(e: Expression): Unit = e.typeExpr match {
    case SHAPE_TYPE if typeExpr == SHAPE_TYPE || typeExpr == NO_TYPE => store(e.shape_value)
    case NUMBER_TYPE if typeExpr == NUMBER_TYPE || typeExpr == NO_TYPE => store(e.number_value)
    case BOOLEAN_TYPE if typeExpr == BOOLEAN_TYPE || typeExpr == NO_TYPE => store(e.boolean_value)
    case STRING_TYPE if typeExpr == STRING_TYPE || typeExpr == NO_TYPE => store(e.string_value)
    case ARENA_TYPE if typeExpr == ARENA_TYPE || typeExpr == NO_TYPE => store(e.arena_value)
    case _ => 
  }
  /** The possibles values of this expression */
  def shape_value: GameShapes.Shape = storedValue.asInstanceOf[GameShapes.Shape]
  def arena_value: GameShapes.Arena = storedValue.asInstanceOf[GameShapes.Arena]
  def number_value: Float = storedValue.asInstanceOf[Float]
  def boolean_value: Boolean = storedValue.asInstanceOf[Float] != 0
  def string_value: String = storedValue.asInstanceOf[String]
  def evaluate(context: HashMap[String, Expression]): Expression = {
    Expression.evaluateExpression(context, this)
  }
  
  // def toScalaString:String = toScalaString("", null)
  
  /** Converts the expression to a valid scala string */
  def toScalaString(indent: String, context: HashMap[String, Expression], modifiableConstants: ArrayBuffer[(Int, Int, Int, Expression)] = null, currentLine: Option[Int] = None, currentCol: Option[Int] = None):String = this match  {
    case ETop() => indent + "this"
    case EIdent(name) =>
      if(context != null) {
        context.getOrElse(name, ETop()) match {
          case EIdentArena(arena) => indent + arena.mName
          case EIdentShape(shape) => indent + shape.mName
          case _ => indent + name
        }
      } else {
        indent + name
      }
    case EIdentArena(arena) => indent + arena.mName
    case EIdentShape(shape) =>
      indent + shape.mName
    case EApply(ESelect(expression, method), List(argument)) =>
      val obj = expression.toScalaString("", context, modifiableConstants, currentLine, currentCol)
      var start: String = ""
      var end:String = ""
      if(method.endsWith("_$eq")) {
        start = indent + obj + "." + method.substring(0, method.length() - 4) + " = "
      } else if(method == "$minus") {
        start = indent + obj + " - "
      } else if(method == "$plus") {
        start = indent + obj + " + "
      } else if(method == "$times") {
        start = indent + obj + " * "
      } else if(method == "$div") {
        start = indent + obj + " / "
      } else if(method == "$greater") {
        start = indent + obj + " > "
      } else if(method == "$less") {
        start = indent + obj + " < "
      } else if(method == "$less$eq") {
        start = indent + obj + " <= "
      } else if(method == "$greater$eq") {
        start = indent + obj + " >= "
      } else if(method == "$eq$eq") {
        start = indent + obj + " == "
      } else if(method == "$plus$eq") {
        start = indent + obj + " += "
      } else if(method == "$minus$eq") {
        start = indent + obj + " -= "
      } else if(method == "$times$eq") {
        start = indent + obj + " *= "
      } else if(method == "$div$eq") {
        start = indent + obj + " /= "
      } else if(method == "$bar$bar") {
        start = indent + obj + " || "
      } else if(method == "$amp$amp") {
        start = indent + obj + " && "
      } else if(method == "$bang$eq") {
        start = indent + obj + " != "
      } else if(method == "$eq") {
        start = indent + obj + " = "
      } else {
        if(expression == ETop()) {
          start = indent + method + "("
          end = ")"
        } else {
          start = indent + obj + "." + method + "("
          end = ")"
        }
      }
      val value = argument.toScalaString("", context, modifiableConstants, currentLine, currentCol map {c => c + start.size})
      start + value + end
    case EApply(ESelect(expression, method), values) =>
      val prefix = if(expression == ETop()) {
        indent
      } else {
        val obj = expression.toScalaString("", context, modifiableConstants, currentLine, currentCol)
        indent + obj + "."
      }
      if(values == Nil) {
        prefix + method + "()"
      } else { // More than two elements.
        var start = prefix + method + "("
        var col = currentCol map {c => c + start.size}
        var first = true
        values foreach { value =>
          if(!first) {
            start = ", " + start
          }
          col = currentCol map {c => c + start.size}
          var s = value.toScalaString("", context, modifiableConstants, currentLine, col)
          start = start + s
          first = false
        }
        start + ")"
      }
    case ESelect(expression, parameter) =>
      val obj = expression.toScalaString("", context)
      indent + obj + "." + parameter
    case cb@CompiledBlock(f) => 
      (if(cb.exprRepresentation != Nil) Expression.expressionListToScalaString(cb.exprRepresentation, indent, context) else indent + "//Compiled block")
    case cb@CompiledBoolean(f) => 
      if(cb.exprRepresentation != Expression.NONE) cb.exprRepresentation.toScalaString(indent, context) else indent + "/*Compiled Boolean*/false"
    case EConstantString(value) =>
      indent + "\"" + value + "\""
    case EConstantBoolean(value) =>
      val valueString = value.toString
      currentLine match { case Some(line) =>
        currentCol match { case Some(col) =>
             modifiableConstants += ((line, col + indent.size, valueString.size, this))
          case _ => } case _ => }
      indent + valueString
    case EConstantNumber(value) =>
      val int_part = Math.round(value)
      val valueString =  (if(int_part==value) int_part.toString else (((value*1000).toInt.toFloat/1000).toString + "f"))
      currentLine match { case Some(line) =>
        currentCol match { case Some(col) =>
             modifiableConstants += ((line, col + indent.size, valueString.size, this))
          case _ => } case _ => }
      indent + valueString
    case ERandomInterval(minValue, maxValue) =>
      indent + "randomInterval(" + minValue + "f, " + maxValue + "f)"
    case ERandomNumber(values) =>
      indent + "random(" + values.map{v => v.toString() + "f"}.reduceLeft(_ + ", " + _) + ")"
    case i@IfCode(condition, codeIfTrue, codeIfFalse) =>
      val headerString = i.headerToScalaString(indent, context, modifiableConstants, currentLine, currentCol)
      val trueCodeString = expressionListToScalaString(codeIfTrue, indent + "  ", context, modifiableConstants, currentLine map { line => line + 1 }, currentCol)
      var result = headerString + (if(trueCodeString != "") "\n" + trueCodeString else "") +  "\n" + indent + "}"
      if(codeIfFalse == Nil) {
        result
      } else {
        result = result + " else {\n"
        val newLine = currentLine map { line => line + result.count(_ == '\n') + 1 }
        val falseCodeString = expressionListToScalaString(codeIfFalse, indent + "  ", context, modifiableConstants, newLine, currentCol)
        result + falseCodeString + "\n" + indent + "}"
      }
    case ValDefCode(variable, content) =>
      var prefix = indent + "val " + variable.name + " = " 
      prefix + content.toScalaString("", context, modifiableConstants, currentLine, currentCol map { col => col + prefix.size })
    case ParallelExpressions(l@(a::Nil)) =>
      a.toScalaString(indent, context, modifiableConstants, currentLine, currentCol)
    case ParallelExpressions(l@(a::q)) =>
      a.toScalaString(indent, context, modifiableConstants, currentLine, currentCol) + " // " + (if(!PRINTALL_PARALLEL) q.size + " more" else ("or: " + q.map (_.toScalaString("", context)).reduceLeft{(a:String, b:String) => a + "," + b }))
    case _ =>
      super.toString()
  }
  
  /** Checks whether this expression contains a certain identifier */
  def contains(name: String): Boolean = this match {
    case EIdentShape(shape) if shape != null => shape.mName == name
    case EIdent(mName) => mName == name
    case ESelect(obj, property) => obj contains name
    case EApply(obj, args) => (obj contains name) || args.foldLeft(false){ case (res, e) => res || e.contains(name) }
    case ValDefCode(variable, content) => (variable contains name) || (content contains name)
    case IfCode(condition, codeIfTrue, codeIfFalse) => (condition contains name) || codeIfTrue.foldLeft(false){ case (res, e) => res || e.contains(name) } ||
                                                       codeIfFalse.foldLeft(false){ case (res, e) => res || e.contains(name) }
    case _ => false
  }
  /** Checks whether this expression contains a certain shape */
  def contains(shape: Shape): Boolean = this match {
    case EIdentShape(s) if s != null => s == shape
    case EIdent(mName) => mName == shape.mName
    case ESelect(obj, property) => obj contains shape
    case EApply(obj, args) => (obj contains shape) || args.foldLeft(false){ case (res, e) => res || e.contains(shape) }
    case ValDefCode(variable, content) => (variable contains shape) || (content contains shape)
    case IfCode(condition, codeIfTrue, codeIfFalse) => (condition contains shape) || codeIfTrue.foldLeft(false){ case (res, e) => res || e.contains(shape) } ||
                                                       codeIfFalse.foldLeft(false){ case (res, e) => res || e.contains(shape) }
    case _ => false
  }
  /** Replaces the oldShape in the expression by a newShape, creating a new expression */
  def replaceShape(oldShape: Shape, newShape: Shape): Expression = this match {
    case EIdentShape(shape) if shape != null && shape == oldShape => EIdentShape(newShape)
    case EIdent(mName) if(mName == oldShape.mName) => EIdent(newShape.mName)
    case ESelect(obj, property) => ESelect(obj.replaceShape(oldShape, newShape), property)
    case EApply(obj, args) => EApply(obj.replaceShape(oldShape, newShape), args.map{ e => e.replaceShape(oldShape, newShape) })
    case ValDefCode(variable, content) => ValDefCode(variable.replaceShape(oldShape, newShape).asInstanceOf[NamedExpression], content.replaceShape(oldShape, newShape))
    case IfCode(condition, codeIfTrue, codeIfFalse) =>
      IfCode(condition.replaceShape(oldShape, newShape), codeIfTrue.map{ e => e.replaceShape(oldShape, newShape) },
          codeIfFalse.map{ e => e.replaceShape(oldShape, newShape) })
    case ParallelExpressions(list) => ParallelExpressions(list map { e => e.replaceShape(oldShape, newShape) })
    case t => t
  }
  
  /** Checks whether the expression modifies the shape s */
  def modifies(s: Shape): Boolean = {
    this match {
      case EApply(ESelect(ESelect(EIdentShape(t), parameter), op), argument) if t==s && (op=="$eq" || op=="$plus$eq"|| op=="$minus$eq" || op=="$times$eq" || op=="$div$eq") => true
      case ParallelExpressions(a::q) => a modifies s
      case IfCode(condition, codeIfTrue, codeIfFalse) => 
        codeIfTrue.foldLeft(false){ case (res, expr) => res || (expr modifies s) } || 
        codeIfFalse.foldLeft(false){ case (res, expr) => res || (expr modifies s) }
      case _ => false
    }
  }
  /** Checks whether the expression modifies the attribute of the shape s */
  def modifiesAttribute(s: Shape, attribute: String): Boolean = {
    this match {
      case EApply(ESelect(ESelect(EIdentShape(t), parameter), op), argument) if t == s && parameter == attribute && (op=="$eq" || op=="$plus$eq"|| op=="$minus$eq" || op=="$times$eq" || op=="$div$eq") => true
      case ParallelExpressions(a::q) => a.modifiesAttribute(s, attribute)
      case IfCode(condition, codeIfTrue, codeIfFalse) => 
        codeIfTrue.foldLeft(false){ case (res, expr) => res || (expr.modifiesAttribute(s, attribute)) } || 
        codeIfFalse.foldLeft(false){ case (res, expr) => res || (expr.modifiesAttribute(s, attribute)) }
      case _ => false
    }
  }
  /** Merge this expression with another one which should produce the same result */
  def merge(e: Expression): Expression = {
    (this, e) match {
      case (ParallelExpressions(l1), ParallelExpressions(l2)) =>
        val intersection = l1 intersect l2
        val cross_merge = l1.flatMap(e1 => l2 flatMap {
            e2 => val e_merge = e1 merge e2
            if(e_merge != Expression.NONE) {
              List(e_merge)
            } else Nil
        })
        (intersection ++ cross_merge).distinct match {
          case Nil => Expression.NONE
          case (a::Nil) => a
          case l => ParallelExpressions(l)
        }
      case (ParallelExpressions(l1), e2) =>  ParallelExpressions(l1) merge ParallelExpressions(List(e2))
      case (e1, ParallelExpressions(l2)) =>  ParallelExpressions(List(e1)) merge e
      case (e1, e2) if e1 == e2 => e1
      case (EApply(e1, f1), EApply(e2,  f2)) => 
        val e_merge = e1 merge e2
        val f_merge = (f1 zip f2) map {case (a, b) => a merge b}
        if((e_merge == Expression.NONE) || (f_merge contains Expression.NONE)) {
          Expression.NONE
        } else {
          EApply(e_merge, f_merge)
        }
      case (EConstantNumber(value1), EConstantNumber(value2)) =>  ERandomNumber(List(value1, value2))
      case (ERandomNumber(l), EConstantNumber(value)) => ERandomNumber(value :: l)
      case (EConstantNumber(value), ERandomNumber(l)) => ERandomNumber(value :: l)
      case (ERandomNumber(l1), ERandomNumber(l2)) => ERandomNumber(l1 ++ l2)
      case _ => Expression.NONE
    }
  }
}

/**
 * Creates constants
 */
object EConstant {
  def apply(b: Boolean) = EConstantBoolean(b)
  def apply(b: Float) = EConstantNumber(b)
  def apply(b: Int) = EConstantNumber(b.toFloat)
  def apply(b: String) = EConstantString(b)
}
trait NamedExpression extends Expression {
  def name: String
}
trait ModifiableValue extends Expression {
  def setValue(value: Float)
}
case class ETop extends Expression
case class EIdentArena(value: Arena) extends Expression with NamedExpression { store(value) ; def name = value.mName }//; override def toString = super.toString}
case class EIdentShape(shape: Shape) extends Expression with NamedExpression {
  store(shape) ; def name = shape.mName
  def x: Expression = ESelect(this, "x")
  def x_=(e: Expression): Expression = EApply(ESelect(ESelect(this, "x"), "$eq"), List(e))
  def prev_x: Expression = ESelect(this, "prev_x")
  def y: Expression = ESelect(this, "y")
  def y_=(e: Expression): Expression = EApply(ESelect(ESelect(this, "y"), "$eq"), List(e))
  def prev_y: Expression = ESelect(this, "prev_y")
  def angle: Expression = ESelect(this, "angle")
  def angle_=(e: Expression): Expression = EApply(ESelect(ESelect(this, "angle"), "$eq"), List(e))
  def prev_angle: Expression = ESelect(this, "prev_angle")
  def velocity: Expression = ESelect(this, "velocity")
  def velocity_=(e: Expression): Expression = EApply(ESelect(ESelect(this, "velocity"), "$eq"), List(e))
  def prev_velocity: Expression = ESelect(this, "prev_velocity")
  def velocity_x: Expression = ESelect(this, "velocity_x")
  def velocity_x_=(e: Expression): Expression = EApply(ESelect(ESelect(this, "velocity_x"), "$eq"), List(e))
  def prev_velocity_x: Expression = ESelect(this, "prev_velocity_x")
  def velocity_y: Expression = ESelect(this, "velocity_y")
  def velocity_y_=(e: Expression): Expression = EApply(ESelect(ESelect(this, "velocity_y"), "$eq"), List(e))
  def prev_velocity_y: Expression = ESelect(this, "prev_velocity_y")
  def width: Expression = ESelect(this, "width")
  def width_=(e: Expression): Expression = EApply(ESelect(ESelect(this, "width"), "$eq"), List(e))
  def prev_width: Expression = ESelect(this, "prev_width")
  def height: Expression = ESelect(this, "height")
  def height_=(e: Expression): Expression = EApply(ESelect(ESelect(this, "height"), "$eq"), List(e))
  def prev_height: Expression = ESelect(this, "prev_height")
  def radius: Expression = ESelect(this, "radius")
  def radius_=(e: Expression): Expression = EApply(ESelect(ESelect(this, "radius"), "$eq"), List(e))
  def prev_radius: Expression = ESelect(this, "prev_radius")
  def color: Expression = ESelect(this, "color")
  def color_=(e: Expression): Expression = EApply(ESelect(ESelect(this, "color"), "$eq"), List(e))
  def prev_color: Expression = ESelect(this, "prev_color")
  def value: Expression = ESelect(this, "value")
  def value_=(e: Expression): Expression = EApply(ESelect(ESelect(this, "value"), "$eq"), List(e))
  def prev_value: Expression = ESelect(this, "prev_value")
  //def prev_value_=(e: Expression): Expression = EApply(ESelect(ESelect(this, "prev_value"), "$eq"), List(e))
  def visible: Expression = ESelect(this, "visible")
  def visible_=(e: Expression): Expression = EApply(ESelect(ESelect(this, "visible"), "$eq"), List(e))
  def prev_visible: Expression = ESelect(this, "prev_visible")
  def text: Expression = ESelect(this, "text")
  def text_=(e: Expression): Expression = EApply(ESelect(ESelect(this, "text"), "$eq"), List(e))
  def prev_text: Expression = ESelect(this, "prev_text")
  //def prev_texttext_=(e: Expression): Expression = EApply(ESelect(ESelect(this, "prev_text"), "$eq"), List(e))
  
}
case class EIdent(mName: String) extends Expression with NamedExpression { def name = mName;
  override def toString = "EIdent(\"" + mName + "\")"
} //override def toString = toScalaString("", null) }

object EConstantNumber {
  def apply(d: Double):EConstantNumber = EConstantNumber(d.toFloat)
}
case class EConstantBoolean(value: Boolean) extends Expression { store(value) } //; override def toString = toScalaString("", null) }
case class EConstantNumber(var value: Float) extends ModifiableValue    { store(value) ; def setValue(v: Float) = { value = v; store(v)}; } //override def toString = toScalaString("", null) }
case class ERandomInterval(minValue: Float, maxValue: Float) extends Expression { store(minValue); } //override def toString = toScalaString("", null)  }
case class ERandomNumber(values: List[Float]) extends Expression { store(0); } //override def toString = toScalaString("", null)  }
case class EConstantString(value: String) extends Expression { store(value);
  override def toString = "EConstantString(\"" + value + "\")"
} //override def toString = toScalaString("", null) }
case class ESelect(obj: Expression, property: String) extends Expression { 
  override def toString = "ESelect(" + obj.toString + ", \"" + property + "\")"
  
} //override def toString = toScalaString("", null) }
case class EApply(obj: Expression, arguments: List[Expression]) extends Expression { } //override def toString = toScalaString("", null) }
case class ValDefCode(variable: NamedExpression, content: Expression) extends Expression { } //override def toString = toScalaString("", null) }
case class IfCode(condition: Expression, codeIfTrue: List[Expression], codeIfFalse: List[Expression]) extends Expression {
  //override def toString = toScalaString("", null)
  def headerToScalaString(indent: String, context: HashMap[String, Expression], modifiableConstants: ArrayBuffer[(Int, Int, Int, Expression)] = null, currentLine: Option[Int] = None, currentCol: Option[Int] = None) = {
    val prefix = indent + "if("
    prefix + condition.toScalaString("", context, modifiableConstants, currentLine, currentCol map { c => c + prefix.size })+ ") {"
  }
}
/** Special expression where only the first one is evaluated, but the others are equivalent till something changes*/
case class ParallelExpressions(var expressions: List[Expression]) extends Expression  { } //override def toString = toScalaString("", null) }
case class ENone() extends Expression
/** Contains a function that can be executed, but keeps a modifyable representation of it.*/
case class CompiledBlock(f: () => Unit) extends Expression {
  var exprRepresentation: List[Expression] = Nil
  override def toString = exprRepresentation.toString()
  //override def toString = toScalaString("", null)
}
/** Contains a function with argument that can be executed, but keeps a modifyable representation of it */
case class CompiledProgram[T](f: T => Unit) extends Expression {
  var argName: String = null
  var argType: String = null
  var exprRepresentation: List[Expression] = Nil
  override def toString = exprRepresentation.toString()
  //override def toString = toScalaString("", null)
}
/** Contains a boolean evaluated at run time, with the modifyable representation of it */
case class CompiledBoolean(f: () => Boolean) extends Expression {
  var exprRepresentation: Expression = Expression.NONE
  override def toString = exprRepresentation.toString()
  //override def toString = toScalaString("", null)
}

/**
 * Rules that can be stored and executing in games.
 */
sealed trait ReactiveRule {
  /** Checks if a rule contains a particular shape. */
  def contains(shape: GameShapes.Shape): Boolean = this match {
    case WhenEverRule(condition, code) =>
      condition.contains(shape) || code.foldLeft(false){ case (res, e) => res || e.contains(shape)  }
    case WhenFingerMovesOnRule(obj, coords, code) =>
      obj.contains(shape) || code.foldLeft(false){ case (res, e) => res || e.contains(shape)  }
    case WhenFingerDownOnRule(obj, code) =>
      obj.contains(shape) || code.foldLeft(false){ case (res, e) => res || e.contains(shape)  }
    case WhenFingerUpOnRule(obj, code) =>
      obj.contains(shape) || code.foldLeft(false){ case (res, e) => res || e.contains(shape)  }
    case WhenCollisionBetweenRule(obj1, obj2, code) =>
      obj1.contains(shape) || obj2.contains(shape) || code.foldLeft(false){ case (res, e) => res || e.contains(shape)  }
    case WhenIntegerChangesRule(obj, coords, code) =>
      obj.contains(shape) || code.foldLeft(false){ case (res, e) => res || e.contains(shape)  }
    case NoCollisionBetweenRule(obj1, obj2) =>
      obj1.contains(shape) || obj2.contains(shape)
    case NoCollisionEffectBetweenRule(obj1, obj2) =>
      obj1.contains(shape) || obj2.contains(shape)
  }
  
  /** Checks if a rule contains a particular name. */
  def contains(name: String): Boolean = this match {
    case WhenEverRule(condition, code) =>
      condition.contains(name) || code.foldLeft(false){ case (res, e) => res || e.contains(name)  }
    case WhenFingerMovesOnRule(obj, coords, code) =>
      obj.contains(name) || code.foldLeft(false){ case (res, e) => res || e.contains(name)  }
    case WhenFingerDownOnRule(obj, code) =>
      obj.contains(name) || code.foldLeft(false){ case (res, e) => res || e.contains(name)  }
    case WhenFingerUpOnRule(obj, code) =>
      obj.contains(name) || code.foldLeft(false){ case (res, e) => res || e.contains(name)  }
    case WhenCollisionBetweenRule(obj1, obj2, code) =>
      obj1.contains(name) || obj2.contains(name) || code.foldLeft(false){ case (res, e) => res || e.contains(name)  }
    case WhenIntegerChangesRule(obj, coords, code) =>
      obj.contains(name) || code.foldLeft(false){ case (res, e) => res || e.contains(name)  }
    case NoCollisionBetweenRule(obj1, obj2) =>
      obj1.contains(name) || obj2.contains(name)
    case NoCollisionEffectBetweenRule(obj1, obj2) =>
      obj1.contains(name) || obj2.contains(name)
  }
  
  /** Replaces a shape by another in this rule, creating a new rule.*/
  def replaceShape(oldShape: Shape, newShape: Shape): ReactiveRule = this match {
      case WhenEverRule(condition, code) =>
        WhenEverRule(condition.replaceShape(oldShape, newShape), code.map{ e => e.replaceShape(oldShape, newShape) })
      case WhenFingerMovesOnRule(obj, coords, code) =>
        WhenFingerMovesOnRule(obj.replaceShape(oldShape, newShape).asInstanceOf[NamedExpression], coords, code.map{ e => e.replaceShape(oldShape, newShape) })
      case WhenFingerDownOnRule(obj, code) =>
        WhenFingerDownOnRule(obj.replaceShape(oldShape, newShape).asInstanceOf[NamedExpression], code.map{ e => e.replaceShape(oldShape, newShape) })
      case WhenFingerUpOnRule(obj, code) =>
        WhenFingerUpOnRule(obj.replaceShape(oldShape, newShape).asInstanceOf[NamedExpression], code.map{ e => e.replaceShape(oldShape, newShape) })
      case WhenCollisionBetweenRule(obj1, obj2, code) =>
        WhenCollisionBetweenRule(obj1.replaceShape(oldShape, newShape).asInstanceOf[NamedExpression], obj2.replaceShape(oldShape, newShape).asInstanceOf[NamedExpression], code.map{ e => e.replaceShape(oldShape, newShape) })
      case WhenIntegerChangesRule(obj, coords, code) =>
        WhenIntegerChangesRule(obj.replaceShape(oldShape, newShape).asInstanceOf[NamedExpression], coords, code.map{ e => e.replaceShape(oldShape, newShape) })
      case NoCollisionBetweenRule(obj1, obj2) =>
        NoCollisionBetweenRule(obj1.replaceShape(oldShape, newShape).asInstanceOf[NamedExpression], obj2.replaceShape(oldShape, newShape).asInstanceOf[NamedExpression])
      case NoCollisionEffectBetweenRule(obj1, obj2) =>
        NoCollisionEffectBetweenRule(obj1.replaceShape(oldShape, newShape).asInstanceOf[NamedExpression], obj2.replaceShape(oldShape, newShape).asInstanceOf[NamedExpression])
  }
  
  /** Remove the parts of the code where a shape appears. */
  def removeShapeFromCode(oldShape: Shape) = {
    code = code.filterNot((expr:Expression) => expr.contains(oldShape))
  }
  
  /** Converts the header of this rule to a scala string */
  def headerToScalaString(indent: String, context: HashMap[String, Expression], modifiableConstants: ArrayBuffer[(Int, Int, Int, Expression)] = null, currentLine: Option[Int] = None, currentCol: Option[Int] = None): String = this match {
    case WhenEverRule(condition, code) =>
      val prefix = indent + "WhenEver("
      prefix + condition.toScalaString("", context, modifiableConstants, currentLine, currentCol map { c => c + prefix.size }) + ") {"
    case WhenFingerMovesOnRule(is, coords, code) =>
      var prefix = indent + "WhenFingerMovesOn(";
      prefix = prefix + is.toScalaString("", context, modifiableConstants, currentLine, currentCol map { c => c + prefix.size }) + ") { (" + (coords.map(_.toScalaString("", context)).reduceLeft(_ + ", " + _)) + ") =>"
      prefix
    case WhenCollisionBetweenRule(shape1, shape2, code) =>
      var prefix:String = indent + "WhenCollisionBetween("
      prefix = prefix + shape1.toScalaString("", context, modifiableConstants, currentLine, currentCol map { c => c + prefix.size }) + ", ";
      prefix + shape2.toScalaString("", context, modifiableConstants, currentLine, currentCol map { c => c + prefix.size }) + ") {";
    case WhenFingerDownOnRule(is, code) =>
      val prefix = indent + "WhenFingerDownOn("
      prefix + is.toScalaString("", context, modifiableConstants, currentLine, currentCol map { c => c + prefix.size }) + ") {"
    case WhenFingerUpOnRule(is, code) =>
      val prefix = indent + "WhenFingerUpOn("
      prefix + is.toScalaString("", context, modifiableConstants, currentLine, currentCol map { c => c + prefix.size }) + ") {"
    case WhenIntegerChangesRule(is, coords, code) =>
      val prefix = indent + "WhenIntegerChanges("
      prefix + is.toScalaString("", context, modifiableConstants, currentLine, currentCol map { c => c + prefix.size }) + ") { (" + (coords map (_.toScalaString("", context)) reduceLeft (_ + ", " + _)) + ") =>"
    case NoCollisionBetweenRule(shape1, shape2) =>
      var prefix = indent + "NoCollisionBetween("
      prefix = prefix + shape1.toScalaString("", context, modifiableConstants, currentLine, currentCol map { c => c + prefix.size }) + ", "
      prefix + shape2.toScalaString("", context, modifiableConstants, currentLine, currentCol map { c => c + prefix.size }) + ")"
    case NoCollisionEffectBetweenRule(shape1, shape2) =>
      var prefix = indent + "NoCollisionEffectBetween("
      prefix = prefix + shape1.toScalaString("", context, modifiableConstants, currentLine, currentCol map { c => c + prefix.size }) + ", "
      prefix + shape2.toScalaString("", context, modifiableConstants, currentLine, currentCol map { c => c + prefix.size }) + ")"
  }
  /** Converts the body of this rule to a scala string */
  def bodyToScalaString(indent: String, context: HashMap[String, Expression], modifiableConstants: ArrayBuffer[(Int, Int, Int, Expression)] = null, currentLine: Option[Int] = None, currentCol: Option[Int] = None): String = {
    Expression.expressionListToScalaString(code, indent + "  ", context, modifiableConstants, currentLine, currentCol)
  }
  /** Converts the footer of this rule to a scala string */
  def footerToScalaString(indent: String): String = indent + "}"
  
  /** Makes a scala string out of this rule */
  def toScalaString(indent: String, context: HashMap[String, Expression], modifiableConstants: ArrayBuffer[(Int, Int, Int, Expression)] = null, currentLine: Option[Int] = None, currentCol: Option[Int] = None):String = this match {
    case NoCollisionBetweenRule(shape1, shape2) =>
      headerToScalaString(indent, context, modifiableConstants, currentLine, currentCol)
    case NoCollisionEffectBetweenRule(shape1, shape2) =>
      headerToScalaString(indent, context, modifiableConstants, currentLine, currentCol)
    case _ =>
      headerToScalaString(indent, context, modifiableConstants, currentLine, currentCol) + "\n" +
      bodyToScalaString(indent, context, modifiableConstants, currentLine map { line => line + 1 }, currentCol) + "\n" +
      footerToScalaString(indent)
  }
  
  /** Converts the code only to a string */
  def codeToString():String = {
    code match {
      case ((c@CompiledBlock(f))::l) => (c.exprRepresentation ++ l).toString
      case _ => code.toString()
    }
  }
  
  /** Removes all the compiled block by replacing them to their modifyable representation */
  def removeCompiledBlocks() = {
    code = code flatMap {
      case c@CompiledBlock(f) => c.exprRepresentation
      case c => List(c)
    }
  }
  
  /** Removes all the compiled block by replacing them to their modifyable representation */
  def toCompleteScalaString(indent: String, context: HashMap[String, Expression]):String = this match {
    case NoCollisionBetweenRule(shape1, shape2) =>
      headerToScalaString(indent, context)
    case NoCollisionEffectBetweenRule(shape1, shape2) =>
      headerToScalaString(indent, context)
    case w@WhenEverRule(condition, code) =>
      headerToScalaString(indent, context) + "\n" +
      bodyToScalaString(indent, context) + "\n" +
      footerToScalaString(indent) + ".represents(" + condition.toString() + "," + codeToString() + ")"
    case _ =>
      headerToScalaString(indent, context) + "\n" +
      bodyToScalaString(indent, context) + "\n" +
      footerToScalaString(indent) + ".represents(" + codeToString() + ")"
  }
  
  def code: List[Expression]
  def code_=(e: List[Expression])

  /** Adds a modifyable representation to the compiled code */
  def represents(e: List[Expression]):this.type = {
    code match {
      case (co@CompiledBlock(f))::Nil =>
        co.exprRepresentation = co.exprRepresentation ++ e
      case _ => throw new Exception("Wrong usage of represents(e, l) on " + this)
    }
    this
  }
  /** Adds a modifyable representation to the compiled code when the rule is a WhenEver rule*/
  def represents(e: Expression, l: List[Expression]): this.type = {
    this match {
      case WhenEverRule(cb@CompiledBoolean(f1), List(co@CompiledBlock(f2))) =>
        cb.exprRepresentation = e
        co.exprRepresentation = l
      case _ => throw new Exception("Wrong usage of represents(e, l) on " + this)
    }
    this
  }
}

/** Rules that handle events */
case class WhenEverRule(condition: Expression, var code: List[Expression]) extends ReactiveRule
case class WhenFingerMovesOnRule(obj: NamedExpression, coords: List[EIdent], var code: List[Expression]) extends ReactiveRule
case class WhenFingerDownOnRule(obj: NamedExpression, var code: List[Expression]) extends ReactiveRule
case class WhenFingerUpOnRule(obj: NamedExpression, var code: List[Expression]) extends ReactiveRule
case class WhenCollisionBetweenRule(obj1: NamedExpression, obj2: NamedExpression, var code: List[Expression]) extends ReactiveRule
case class WhenIntegerChangesRule(obj: NamedExpression, coords: List[EIdent], var code: List[Expression]) extends ReactiveRule
case class NoCollisionBetweenRule(obj1: NamedExpression, obj2: NamedExpression) extends ReactiveRule { var code:List[Expression] = Nil }
case class NoCollisionEffectBetweenRule(obj1: NamedExpression, obj2: NamedExpression) extends ReactiveRule { var code:List[Expression] = Nil }


