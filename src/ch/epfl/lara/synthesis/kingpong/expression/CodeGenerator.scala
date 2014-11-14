package ch.epfl.lara.synthesis.kingpong.expression

import android.util.Log

import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.rules.Events._

trait CodeHandler {

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

  /**
   * Create a rule based on the selected event and the status of the game.
   * This function generates the condition to which the code has been modified.
   * @param game the game for which the rule must be created.
   * @param conditionEvent the events that will participate in the rule condition.
   * @param conditionObjects the objects that will participate in the rule condition.
   * @param actionObjects the objects selected manually by the user that will participate in the rule action (or body).
   **/
  def createRule(
      game: Game,
      conditionEvent: List[(Event, Int)],
      conditionObjects: Set[GameObject],
      actionObjects: Set[GameObject]): Unit = {

    // All objects that will participate in the rule body:
    // a) the objects selected by the user
    // b) the objects linked to the selected events
    // c) the objects for which one property has changed
    val templateObjects: Set[GameObject] = actionObjects.toSet ++
                                           conditionEvent.flatMap(_._1.objects) ++
                                           game.aliveObjects.filter(_.properties.exists(p => p.get != p.next))

    // Infer the rule body using templates
    val ruleBody = CodeTemplates.inferStatements(game, conditionEvent, templateObjects)

    // Check if the body is already guarded by a finger move
    val alreadyMoveGuarded = TreeOps.exists(_.isInstanceOf[FingerMoveOver])(ruleBody)
    //TODO this simple check is not enough for complex cases like parallel expressions that contains or not a guard

    val stateConditionOpt = stateCondition(game, conditionObjects)

    // Get the conditions expressions from the selected events
    val conditionsExpr = stateConditionOpt.toList ::: conditionEvent.map(_._1).collect {
      case FingerDown(_, objs) if objs.size > 0 =>
        isFingerDownOver(objs.head.expr)
      case FingerUp(_, objs) if objs.size > 0 =>
        isFingerUpOver(objs.head.expr)
      case FingerMove(_, _, objs) if objs.size > 0 && !alreadyMoveGuarded =>
        isFingerMoveOver(objs.head.expr)
      case BeginContact(_, objectA, objectB) =>
        Collision(objectA.expr, objectB.expr)
    }

    // Construct the final rule from the conditions (if any) and the inferred body
    val rule: Expr = conditionsExpr match {
      case Nil =>
        ruleBody
      case _ =>
        whenever(and(conditionsExpr))(ruleBody: _*)
    }

    Log.d("kingpong",
      s"""Create new rule with condition events:
         |  ${conditionEvent.map(_._1).mkString("[", ", ", "]")}
         |and with condition objects:
         |  ${conditionObjects.mkString("[", ", ", "]")}
         |and with action objects:
         |  ${actionObjects.mkString("[", ", ", "]")}
         |The inferred action objects are:
         |  ${templateObjects.mkString("[", ", ", "]")}
         |Inferred rule body:
         |  $ruleBody
         |Inferred final rule:
         |  $rule""".stripMargin)

    // Rule body:
//    List(ParExpr(List(
//      FingerMoveOver(ObjectLiteral(Rectangle 2), move, Assign((ObjectLiteral(Rectangle 2), x), Plus(Select(ObjectLiteral(Rectangle 2), x), Minus(TupleSelect(TupleSelect(Variable(move), 2), 1), TupleSelect(TupleSelect(Variable(move), 1), 1))) ) ),
//      Assign((ObjectLiteral(Rectangle 2), x), FloatLiteral(8.0)),
//      Assign((ObjectLiteral(Rectangle 2), x), Plus(Select(ObjectLiteral(Rectangle 2), x), FloatLiteral(3.3262281)) )
//    ) ) )

    // If the rule contains objects not planed from the beginning, generalize on them directly.
    var toGeneralize = Set[GameObject]()
    TreeOps.preTraversal(expr => {
      expr match {
        case ObjectLiteral(o) if o != null && o.plannedFromBeginning != GameObject.PLANNED_SINCE_BEGINNING =>
          toGeneralize += o;
        case _ =>
      }
    })(rule)
    val generalizedRule = toGeneralize.foldLeft(rule)((rule, obj) => {
      TreeOps.generalizeToCategory(rule, obj)
    })
    game.addRule(generalizedRule)
    game.validateNextToCurrent()
    //Apply the rule body and stores each next statement to the prev
  }

  /**
   * Try to create an expression that reflects the position of the given objects.
   */
  private def stateCondition(game: Game, objects: Set[GameObject]): Option[Expr] = {
    val posObjects = objects collect {
      case pos: Positionable => pos
    }

    if (posObjects.size == objects.size) {
      //TODO for the moment only test the state condition with arrays
      stateArrayCondition(game, posObjects)
    } else {
      None
    }
  }

  private def stateArrayCondition(game: Game, objects: Set[Positionable]): Option[Expr] = {
    def arraysForObject(obj: Positionable): Set[Array2D] = game.aliveObjects.collect {
      case array: Array2D if array.contains(obj.center.get) => array
    }.toSet

    // Get one common array containing all objects, if any
    val arrayOpt = objects.size match {
      case 0 => None
      case _ => objects.map(arraysForObject).reduceLeft(_ intersect _).headOption
    }

    arrayOpt map { array =>
      val contains = objects.map(obj => Contains(array.containingCellExpr(obj.center.get), obj)).toList
      and(contains)
    }
  }

}