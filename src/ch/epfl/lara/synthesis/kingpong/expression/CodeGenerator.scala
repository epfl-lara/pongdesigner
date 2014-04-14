package ch.epfl.lara.synthesis.kingpong.expression

import android.content.Context
import scala.collection.mutable.HashMap
import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.common._
import org.jbox2d.common.Vec2
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.rules.Events._
import CodeTemplates._

trait CodeHandler {
  /*import Expression.Subtype._*/
  /** Conversions */
  def coord(i: Int): Expr = { val res: Expr = i; /*res.setSubtype(COORD_SUBTYPE);*/ res }
  def coord(i: Float): Expr = { val res: Expr = i; /*res.setSubtype(COORD_SUBTYPE);*/ res }
  def number(i: Int): Expr = { val res: Expr = i; /*res.setSubtype(SCORE_SUBTYPE);*/ res }
  def color(i: Int): Expr =  { val res: Expr = i; /*res.setSubtype(COLOR_SUBTYPE);*/ res }
  def speed(i: Vec2): Expr =  { val res: Expr = i; /*res.setSubtype(SPEED_SUBTYPE);*/ res }
  def factor(i: Float): Expr =  { val res: Expr = i; /*res.setSubtype(FACTOR_SUBTYPE);*/ res }
  def angle(i: Float): Expr = { val res: Expr = i; /*res.setSubtype(ANGLE_SUBTYPE);*/ res }
  
  /** Identifiers used when generating code */
  /*val xFrom_ident = EIdent("xFrom")
  val xTo_ident = EIdent("xTo")
  val yFrom_ident = EIdent("yFrom")
  val yTo_ident = EIdent("yTo")
  val x_ident = EIdent("x")
  val y_ident = EIdent("y")
  val newValue_ident = EIdent("newValue")
  val dx_ident = Val("dx")
  val dy_ident = Val("dy")
  */
  
  /** Determines if two differences are almost the same up to a 40% factor */
  def almostTheSameDiff(val1:Float, val2:Float): Boolean = {
    if(val1 != 0) {
      Math.abs(val2 - val1)/Math.abs(val1) < 0.4
    } else {
      false
    }
  }
  
  /** Determines if two quantities are almost the same to a delta factor */
  def almostTheSame(val1:Float, val2:Float, delta: Float): Boolean = {
    Math.abs(val1 - val2) < delta
  }

}

/**
 * CodeGenerator contains methods used to generate code given a set of shapes that has been modified.
 */
object CodeGenerator extends CodeHandler {
  
  //implicit def funcConvertToExpressionFloat(i: Float): Expression = { val res = EConstant(i); res.setSubtype(NO_TYPE); res }

  //implicit def funcConvertToExpressionString(i: String): Expression = { EConstant(i) }
  /*implicit def funcConvertToExpressionString(i: Boolean): Expression = { EConstant(i) }*/
  
  
  //val oldValue_ident = EIdent("oldValue")

  
  
  /** Creates a code that correspond to the current transform of the game and according to the causal event. */
  /*def recoverCodeFromWorldModification(game: Game, causeEvent: TriggerEvent, new_condition: Expression, existing_code: List[Expression]): List[Expression] = {
    val causeEventCode = if(causeEvent != null) causeEvent.code else -1

    val xFrom = if(causeEvent != null) causeEvent.x1 else 0
    val yFrom = if(causeEvent != null) causeEvent.y1 else 0
    val xTo = if(causeEvent != null) causeEvent.x2 else 0
    val yTo = if(causeEvent != null) causeEvent.y2 else 0
    val shapeEvent = if(causeEvent != null) causeEvent.shape1 else null
    val eventNewValue = yTo.toInt
    val movementIsHorizontal = Math.abs(xTo - xFrom) > 10 * Math.abs(yTo - yFrom)
    val movementIsVertical = Math.abs(yTo - yFrom) > 10 * Math.abs(xTo - xFrom)

    var variablesToStore = new HashMap[Expression, Int]()
    var currentVariableCounter:Int = 1
    
    CodeTemplates.initialize(causeEvent, game.getArena)
    val new_code = CodeTemplates.recover()
    
    // We merge the existing code and the generated code.
    // Check for existing conditions.
    // TODO : No more merging. Should use constraint juxtaposition or replacement.
    MergeTree.merge_code_recursive(game.getArena, existing_code, new_code, new_condition)
  }
  
  /**
   * Recovers an existing rule from an event or creates a new one.
   */
  def getRuleFromEvent(game: Game, causeEvent: TriggerEvent): Option[ReactiveRule] = {
    val result = causeEvent.code match {
      case COLLISION_EVENT =>
          val lookupPair = GameShapes.ShapePair(causeEvent.shape1, causeEvent.shape2)
          Some(game.added_whenCollisionRules.getOrElse(lookupPair, WhenCollisionBetweenRule(EIdentShape(causeEvent.shape1), EIdentShape(causeEvent.shape2), Nil)))
      case INTEGER_CHANGE_EVENT | INTEGER_EQUAL_EVENT | INTEGER_GREATER_EQUAL_EVENT | INTEGER_LESS_EQUAL_EVENT | INTEGER_POSITIVE_EVENT | INTEGER_NEGATIVE_EVENT =>
        val d = causeEvent.shape1.asInstanceOf[IntegerBox]
        val shape_ident = EIdentShape(causeEvent.shape1)
        Some(game.added_whenIntegerChangesRules.getOrElse(causeEvent.shape1.asInstanceOf[IntegerBox],
            WhenIntegerChangesRule(EIdentShape(causeEvent.shape1), List(newValue_ident), Nil)
        ))
      case TOUCHDOWN_EVENT | TOUCHUP_EVENT => 
          val down_event = causeEvent
          var shapeBelow: GameShapes.Shape = causeEvent.shape1
          var minDistance = -1f
          if(shapeBelow == null) { // Look for a shape is the one below is not specified in the causeEvent
            game.getArena foreach { shape =>
              val x = down_event.x1
              val y = down_event.y1
              if(shape.selectableBy(x, y)) {
                val newDist = shape.distanceSelection(x, y)
                if(newDist < minDistance || minDistance == -1) {
                  shapeBelow = shape
                  minDistance = newDist
                }
              }
            }
          }
          if(shapeBelow != null) {
            if(causeEvent.code == TOUCHUP_EVENT) {
              Some(game.added_whenFingerUpOnRules.getOrElse(shapeBelow, WhenFingerUpOnRule(EIdentShape(shapeBelow), Nil)))
            } else {
              Some(game.added_whenFingerDownOnRules.getOrElse(shapeBelow, WhenFingerDownOnRule(EIdentShape(shapeBelow), Nil)))
            }
          } else {
            None
          }
          
        case BEYOND_SCREEN_EVENT | BEYOND_SIDE_EVENT => 
          val shape_ident = EIdentShape(causeEvent.shape1)

          var conditions:List[Expression] = Nil
          causeEvent.shape1 match {
            case r:Rectangular =>
              if(causeEvent.x1 == 0 || causeEvent.code == BEYOND_SCREEN_EVENT) {
                conditions = (shape_ident.x + shape_ident.width < coord(0))::conditions
              }
              if(causeEvent.x1 == game.layoutWidth || causeEvent.code == BEYOND_SCREEN_EVENT) {
                conditions = (shape_ident.x > ELayoutWidth())::conditions
              }
              if(causeEvent.y1 == 0 || causeEvent.code == BEYOND_SCREEN_EVENT) {
                conditions = (shape_ident.y + shape_ident.height < coord(0))::conditions
              }
              if(causeEvent.y1 == game.layoutHeight || causeEvent.code == BEYOND_SCREEN_EVENT) {
                conditions = (shape_ident.y > ELayoutHeight())::conditions
              }
            case c:Circle =>
              if(causeEvent.x1 == 0 || causeEvent.code == BEYOND_SCREEN_EVENT) {
                conditions = (shape_ident.x + shape_ident.radius < coord(0))::conditions
              }
              if(causeEvent.x1 == game.layoutWidth || causeEvent.code == BEYOND_SCREEN_EVENT) {
                conditions = (shape_ident.x - shape_ident.radius > ELayoutWidth())::conditions
              }
              if(causeEvent.y1 == 0 || causeEvent.code == BEYOND_SCREEN_EVENT) {
                conditions = (shape_ident.y + shape_ident.radius < coord(0))::conditions
              }
              if(causeEvent.y1 == game.layoutHeight || causeEvent.code == BEYOND_SCREEN_EVENT) {
                conditions = (shape_ident.y - shape_ident.radius > ELayoutHeight())::conditions
              }
            case _ =>
              Expression.NONE
          }
          //Log.d("test" ,"causeEvent.code = " + causeEvent.code + ", condition size = " + conditions.length)
          conditions match {
            case Nil =>
              None
            case a::Nil =>
              game.init_rules find {
                case w@WhenEverRule(c, code) if c == a => true
                case _ => false
              } match {
                case Some(c) => Some(c)
                case None => val res = WhenEverRule(a, Nil)
                  res.shape = causeEvent.shape1
                  Some(res)
              }
            case l =>
              val condition = conditions.reduceLeft(_ or _)
              game.init_rules find {
                case w@WhenEverRule(c, code) if c == condition => true
                case _ => false
              } match {
                case Some(c) => Some(c)
                case None => Some(WhenEverRule(condition, Nil))
              }
          }
        case TOUCHMOVE_EVENT =>
          var shapeBelow: GameShapes.Shape = causeEvent.shape1
          var minDistance = -1f
          if(shapeBelow == null) { // Look for a shape is the one below is not specified in the causeEvent
            game.getArena foreach { shape =>
              val x = causeEvent.x2
              val y = causeEvent.y2
              if(shape.selectableBy(x, y)) {
                val newDist = shape.distanceSelection(x, y)
                if(newDist < minDistance || minDistance == -1) {
                  shapeBelow = shape
                  minDistance = newDist
                }
              }
            }
          }
          if(shapeBelow != null) {
            Some(game.added_whenFingerMovesOnRules.getOrElse(shapeBelow, WhenFingerMovesOnRule(EIdentShape(shapeBelow), List(EIdent("xFrom"), EIdent("yFrom"), EIdent("xTo"), EIdent("yTo")), Nil)))
          } else None
    }
    result map (game.giveRuleNewCoordinates(_))
  }
  */
  
  import JBox2DInterface._
  /**
   * Creates a rule based on the selected event and the status of the game.
   * This function generates the condition to which the code has been modified.
   **/
  def createRule(context: Context, game: Game, conditionEvent: List[(Event, Int)], conditionConfig: List[GameObject]): Unit = {
    // Need to store the conditions so that we can rely on them rather than on code.
    //val templaceContext = new TemplateContext(conditionEvent.head._1, game.objects)
    val stmts = CodeTemplates.inferStatements(conditionEvent, game.objects)
    // Now we try to merge these statement with the conditions.
    conditionEvent map {
      case (FingerDown(v, objs), i) if objs.size > 0 =>
        (objs, 1, (obj: List[TreeDSL.Proxy]) => FingerDownOver(obj.head))
      case (FingerUp(v, objs), i) if objs.size > 0 =>
        (objs, 1, (obj: List[TreeDSL.Proxy]) => FingerUpOver(obj.head))
      case (FingerMove(u, v, objs), i) if objs.size > 0 =>
        (objs, 1, (obj: List[TreeDSL.Proxy]) => isFingerMoveOver(obj.head))
      case (BeginContact(contact), i) =>
        (Set(contact.objectA, contact.objectB), 2, (obj: List[TreeDSL.Proxy]) => Collision(obj.head, obj.tail.head))
      case _ => NOP
    }
    
    
    //The condition firing the event.
    
    /*val res = context.getResources()
    def askChoicesIfNeeded(r: ReactiveRule) = {
      if(r.code != Nil) {
        r.removeCompiledBlocks()
        game.insertRule(r, causeTimestamp)
        if(ruleHandler != null) ruleHandler(r)
      }    
    }

    if(causeEvent != null || existing_rule != null) { // The existing rule is prioritary.
      val ruleToModify = if(existing_rule != null) Some(existing_rule) else getRuleFromEvent(game, causeEvent)
      ruleToModify match {
        case Some(w) =>
          val condition:Expression = if(causeEvent != null && existing_rule == null) { causeEvent.code match {
            case INTEGER_CHANGE_EVENT =>
              EConstantBoolean(true)
            case INTEGER_EQUAL_EVENT =>
              newValue_ident equals number(causeEvent.y2.toInt)
            case INTEGER_GREATER_EQUAL_EVENT =>
              newValue_ident >= number(causeEvent.y2.toInt)
            case INTEGER_LESS_EQUAL_EVENT =>
              newValue_ident <= number(causeEvent.y2.toInt)
            case INTEGER_POSITIVE_EVENT =>
              newValue_ident >= number(1)
            case INTEGER_NEGATIVE_EVENT =>
              newValue_ident <= number(-1)
            case _ =>
              Expression.NONE
          } } else {
            if(existing_rule != null) {
              existing_rule match {
                case WhenIntegerChangesRule(EIdentShape(i: IntegerBox), List(newValueIdent), code) =>
                  newValueIdent ~= EConstantNumber(i.prev_value)
                case _ =>
                  Expression.NONE
              }
            } else { Expression.NONE }
            
          }
          w.removeCompiledBlocks()
          w.code = CodeGenerator.recoverCodeFromWorldModification(game, causeEvent, condition, w.code)
          askChoicesIfNeeded(w)
        case None =>
          //CodeGenerator.recoverCodeFromWorldModification(game, causeEvent, Nil)
      }
    }*/
  }
  
  /**
   * Duplicates all rules concerning shape1 for shape2
   * This function should be deprecated once the categories are used.
   **/
  /*
  def duplicateRuleContaining(game: Game, shape1: GameShapes.Shape, shape2: GameShapes.Shape) = {
    game.init_rules foreach {
      rule =>
        if(rule.contains(shape1)) {
          // If the shape is in the condition, we duplicate the rule.
          if(rule.conditionContains(shape1)) {
            val new_rule = rule.replaceShape(shape1, shape2)
            game.giveRuleNewCoordinates(new_rule)
            game.insertRule(new_rule, game.currentTime + 1) // Add the rule in the future
          } else {
            rule.code = Expression.duplicateCodeModifying(rule.code, shape1, shape2)
          }
        }
    }
  }*/
}