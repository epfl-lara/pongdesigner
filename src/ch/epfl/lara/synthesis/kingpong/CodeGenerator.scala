package ch.epfl.lara.synthesis.kingpong

import android.content.Context
import android.util.Log
import scala.collection.mutable.HashMap

/**
 * CodeGenerator contains methods used to generate code given a set of shapes that has been modified.
 */
object CodeGenerator {
  import TriggerEvent._
  import GameShapes._
  
  /** Implicit conversions */
  implicit def funcConvertToExpression(i: Int): Expression = { EConstant(i) }
  implicit def funcConvertToExpressionFloat(i: Float): Expression = { EConstant(i) }
  implicit def funcConvertToExpressionString(i: String): Expression = { EConstant(i) }
  /*implicit def funcConvertToExpressionString(i: Boolean): Expression = { EConstant(i) }*/
  
  /** Determines if two quantities are almost the same up to a 40% factor */
  def almostTheSame(val1:Float, val2:Float): Boolean = {
    if(val1 != 0) {
      Math.abs(val2 - val1)/Math.abs(val1) < 0.4
    } else {
      false
    }
  }

  /** Identifiers used when generating code */
  val xFrom_ident = EIdent("xFrom")
  val xTo_ident = EIdent("xTo")
  val yFrom_ident = EIdent("yFrom")
  val yTo_ident = EIdent("yTo")
  val newValue_ident = EIdent("newValue")
  val oldValue_ident = EIdent("oldValue")

  /**
   * Merges codeA and codeB following the order of the set shapes
   */
  def merge_code(shapes: Traversable[Shape], codeA: List[Expression], codeB: List[Expression]): List[Expression] = {
      var code1 = codeA
      var code2 = codeB
      var merged_code:List[Expression] = Nil
      shapes foreach {
        case shape =>
          shape foreachAttribute {
            attribute =>
              var attributeMightBeModified = true
              var mergedRulesForAttribute: List[Expression] = Nil
              code1 match {
                case a::q if a.modifiesAttribute(shape, attribute) =>
                  code1 = q
                  mergedRulesForAttribute = a :: mergedRulesForAttribute
                case a::q =>
                case Nil =>
              }
              code2 match {
                case a::q if a.modifiesAttribute(shape, attribute) =>
                  code2 = q
                  mergedRulesForAttribute = a :: mergedRulesForAttribute
                case a::q =>
                case Nil =>
              }
              mergedRulesForAttribute match {
                case Nil =>
                case a::Nil =>
                  merged_code = a :: merged_code
                case a::b::Nil =>
                  //Log.d("CodeGenerator.scala", "Merging " + a + " and " + b)
                  var merged = a merge b
                  //Log.d("CodeGenerator.scala", "Result = " + merged)
                  if(merged == Expression.NONE) {
                    // TODO : Let the user choose between the two expressions.
                    merged = ParallelExpressions(a::b::Nil)
                    //throw new Exception("Unmergeable expressions:" + a + ", " + b)
                  }
                  merged_code = (a merge b)::merged_code
                case _ => // Should not happen
                  throw new Exception("Too many rules to merge for only one attribute")
              }
          }
      }
      merged_code.reverse
    }
    
    
    
    /**
     * Recursively inserts a conditional code If(new_condition, new_code, Nil) into an existing well-formed code,
     * that is of the form
     *    wellFormedCode(i) ::= If(newValue <= i, simple_code, (simple_code | wellFormedCode(j)) where j > i
     *    wellFormedCode(i) ::= If(newValue == i, simple_code, simple_code)
     * Only the last one might be an equality, for simplicity purposes.
     * 
     * If the condition is not about newValue, then the code is the following:
     *    wellFormedCode ::= If(condition, simple_code, (simple_code | wellFormedCode) 
     */
    def merge_code_recursive(shapes: Traversable[Shape], existing_code: List[Expression], new_code: List[Expression], new_condition: Expression, minReachableInt: Option[Float] = None): List[Expression] = {
      (existing_code, new_condition) match {
        case (Nil, Expression.NONE) | (Nil, EConstantBoolean(true)) =>
          new_code
          // existing code : if(condition). New condition is the same
        case (IfCode(cond, codeIfTrue, codeIfFalse)::Nil, condition) if cond == condition =>
          IfCode(cond, merge_code(shapes, new_code, codeIfTrue), codeIfFalse)::Nil

        // existing code : if(newValue <= n1). New condition: if(newValue <= n2)
        case (IfCode(oldCondition@EApply(ESelect(EIdent("newValue"), "$less$eq"), List(EConstantNumber(n1))), codeIfTrue, codeIfFalse)::Nil,
              EApply(ESelect(EIdent("newValue"), "$less$eq"), List(EConstantNumber(n2)))) =>
            if(n2 < n1) {
              val intermediate_condition:Expression = newValue_ident <= n1
              minReachableInt filter (_ > n2) match {
                case None =>
                  IfCode(new_condition,
                      merge_code(shapes, new_code, codeIfTrue),
                      IfCode(intermediate_condition,
                          codeIfTrue,
                          codeIfFalse
                      )::Nil
                  )::Nil
                case _ =>
                  existing_code
              }
            } else if(n2 == n1) {
              IfCode(oldCondition, merge_code(shapes, new_code, codeIfTrue), codeIfFalse)::Nil
            } else {
              val intermediate_condition:Expression = newValue_ident <= n2
              IfCode(oldCondition,
                  merge_code(shapes, new_code, codeIfTrue),
                  merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n2 + 1))
              )::Nil
            }

        // existing code : if(newValue <= n1). New condition: if(newValue >= n2)
        case (IfCode(oldCondition@EApply(ESelect(EIdent("newValue"), "$less$eq"), List(EConstantNumber(n1))), codeIfTrue, codeIfFalse)::Nil,
              EApply(ESelect(EIdent("newValue"), "$greater$eq"), List(EConstantNumber(n2)))) =>
            if(n2 < n1) {
              val first_condition:Expression = newValue_ident <= n2 - 1
              IfCode(first_condition,
                  codeIfTrue,
                  IfCode(oldCondition,
                      merge_code(shapes, codeIfTrue, new_code),
                      merge_code_recursive(shapes, codeIfFalse, new_code, Expression.NONE, Some(n1 + 1))
                  )::Nil
               )::Nil
            } else if(n1 == n2) { // n1 == n2
             val firstCondition:Expression = newValue_ident <= n1 - 1
             minReachableInt filter (_ > n1 - 1) match {
                case None =>
                 IfCode(firstCondition,
                      codeIfTrue,
                      IfCode(oldCondition,
                          merge_code(shapes, codeIfTrue, new_code),
                          merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n1 + 1))
                      )::Nil
                   )::Nil
               case _ => 
                 IfCode(oldCondition,
                   merge_code(shapes, codeIfTrue, new_code),
                   merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n1 + 1))
                 )::Nil
             }
            } else {  // n2 >= n1 + 1
              IfCode(oldCondition,
                  codeIfTrue,
                  merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n1 + 1))
              )::Nil
            }
        // existing code : if(newValue <= n1). New condition: if(newValue == n2)
        case (IfCode(oldCondition@EApply(ESelect(EIdent("newValue"), "$less$eq"), List(EConstantNumber(n1))), codeIfTrue, codeIfFalse)::Nil,
              EApply(ESelect(EIdent("newValue"), "$eq$eq"), List(EConstantNumber(n2)))) =>
            if(n2 < n1) { // Todo : use MinReachableInt to get rid of duplicates
              val first_condition:Expression = newValue_ident <= n2 - 1
              val second_condition: Expression = newValue_ident <= n2
              minReachableInt filter (_ > n2 - 1) match {
                case None =>
                  IfCode(first_condition,
                    codeIfTrue,
                    IfCode(second_condition,
                        merge_code(shapes, codeIfTrue, new_code),
                        IfCode(oldCondition,
                          codeIfTrue,
                          codeIfFalse
                        )::Nil
                    )::Nil
                 )::Nil
                case _ =>
                  IfCode(second_condition,
                      merge_code(shapes, codeIfTrue, new_code),
                      IfCode(oldCondition,
                        codeIfTrue,
                        codeIfFalse
                      )::Nil
                  )::Nil                  
              }
              
            } else if(n2 == n1) { // n1 == n2
              val firstCondition:Expression = newValue_ident <= n2 - 1
              minReachableInt filter (_ > n2 - 1) match {
                case None =>
                  IfCode(firstCondition,
                      codeIfTrue,
                      IfCode(oldCondition,
                          merge_code(shapes, codeIfTrue, new_code),
                          codeIfFalse
                      )::Nil
                   )::Nil
                case _ => // minReachableInt >= n2
                  IfCode(oldCondition,
                      merge_code(shapes, codeIfTrue, new_code),
                      codeIfFalse
                  )::Nil
              }
            } else {  // n2 >= n1 + 1 We might have duplicates here.
              IfCode(oldCondition,
                  codeIfTrue,
                  merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n1 + 1))
               )::Nil
            }
        // existing code : if(newValue == n1). New condition: if(newValue <= n2)
        case (IfCode(oldCondition@EApply(ESelect(EIdent("newValue"), "$eq$eq"), List(EConstantNumber(n1))), codeIfTrue, codeIfFalse)::Nil,
              EApply(ESelect(EIdent("newValue"), "$less$eq"), List(EConstantNumber(n2)))) =>
            if(n2 < n1 - 1) {
              val second_condition:Expression = newValue_ident <= (n1 - 1)
              val third_condition:Expression = newValue_ident <= n1
              minReachableInt filter (_ > n2) match {
                case None =>
                  IfCode(new_condition,
                      merge_code(shapes, new_code, codeIfFalse),
                      IfCode(second_condition,
                          codeIfFalse,
                          IfCode(third_condition,
                              codeIfTrue,
                              codeIfFalse)::Nil
                      )::Nil
                  )::Nil
                case _ =>
                  existing_code
              }
            } else if(n2 == n1 - 1) {
              val second_condition:Expression = newValue_ident <= (n1 - 1)
              val third_condition:Expression = newValue_ident <= n1
              minReachableInt filter (_ > n2) match {
                case None =>
                  IfCode(new_condition,
                      merge_code(shapes, new_code, codeIfFalse),
                      IfCode(third_condition,
                          codeIfTrue,
                          codeIfFalse)::Nil
                  )::Nil
                case _ =>
                  existing_code
              }
            } else if(n2 == n1) {
              minReachableInt filter (_ >= n1) match {
                case None =>
                  val second_condition:Expression = newValue_ident <= (n1 - 1)
                  IfCode(second_condition,
                        merge_code(shapes, new_code, codeIfFalse),
                        IfCode(new_condition,
                            codeIfTrue,
                            codeIfFalse)::Nil
                    )::Nil
                case _ =>
                  existing_code
              }
            } else { // n2 > n1
              val first_condition:Expression = newValue_ident <= (n1 - 1)
              val second_condition:Expression = newValue_ident <= n1
              // New_condition is the third
              minReachableInt filter (_ > n1-1) match {
                case None =>
                  IfCode(first_condition,
                      merge_code(shapes, new_code, codeIfFalse),
                      IfCode(second_condition,
                          merge_code(shapes, new_code, codeIfTrue),
                          merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n1 + 1))
                      )::Nil
                  )::Nil
                case _ =>
                  IfCode(second_condition,
                      merge_code(shapes, new_code, codeIfTrue),
                      merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n1 + 1))
                  )::Nil
              }
            }
        // existing code : if(newValue == n1). New condition: if(newValue == n2)
        case (IfCode(oldCondition@EApply(ESelect(EIdent("newValue"), "$eq$eq"), List(EConstantNumber(n1))), codeIfTrue, codeIfFalse)::Nil,
              EApply(ESelect(EIdent("newValue"), "$eq$eq"), List(EConstantNumber(n2)))) =>
            if(n2 < n1 - 1) {
              val first_condition = newValue_ident <= (n2 - 1)
              val second_condition = newValue_ident <= n2
              val third_condition = newValue_ident <= n1 - 1
              val fourth_condition = newValue_ident <= n1
              minReachableInt filter (_ > n2 - 1) match {
                case None =>
                  IfCode(first_condition,
                      codeIfFalse,
                      IfCode(second_condition,
                          merge_code(shapes, new_code, codeIfFalse),
                          IfCode(third_condition,
                              codeIfFalse,
                              IfCode(fourth_condition,
                                  codeIfTrue,
                                  codeIfFalse)::Nil
                          )::Nil
                      )::Nil
                  )::Nil
                case _ =>
                  minReachableInt filter (_ > n2) match {
                    case None =>
                        IfCode(second_condition,
                            merge_code(shapes, new_code, codeIfFalse),
                            IfCode(third_condition,
                                codeIfFalse,
                                IfCode(fourth_condition,
                                    codeIfTrue,
                                    codeIfFalse)::Nil
                            )::Nil
                        )::Nil
                    case _ =>
                      minReachableInt filter (_ > n1 - 1) match {
                        case None =>
                          IfCode(third_condition,
                              codeIfFalse,
                              IfCode(fourth_condition,
                                  codeIfTrue,
                                  codeIfFalse)::Nil
                          )::Nil
                        case _ =>
                          existing_code
                      }
                  }
              }
            } else if(n2 == n1 - 1) {
              val first_condition = newValue_ident <= (n2 - 1)
              val second_condition = newValue_ident <= n2
              val third_condition = newValue_ident <= n1
              minReachableInt filter (_ > n2 - 1) match {
                case None =>
                  IfCode(first_condition,
                      codeIfFalse,
                      IfCode(second_condition,
                          merge_code(shapes, new_code, codeIfFalse),
                          IfCode(third_condition,
                              codeIfTrue,
                              codeIfFalse
                          )::Nil
                      )::Nil
                  )::Nil
                case _ =>
                  minReachableInt filter (_ > n2) match {
                    case None =>
                        IfCode(second_condition,
                            merge_code(shapes, new_code, codeIfFalse),
                            IfCode(third_condition,
                                codeIfTrue,
                                codeIfFalse
                            )::Nil
                        )::Nil
                    case _ =>
                      IfCode(third_condition,
                          codeIfTrue,
                          codeIfFalse
                      )::Nil
                  }
              }
            } else if(n2 == n1) {
              IfCode(oldCondition, merge_code(shapes, codeIfTrue, new_code), codeIfFalse)::Nil
            } else { // n2 > n1
              val first_condition = newValue_ident <= n1 - 1
              val second_condition = newValue_ident <= n1
              minReachableInt filter (_ > n1 - 1) match {
                case None =>
                  IfCode(first_condition,
                      codeIfFalse,
                      IfCode(second_condition,
                          codeIfTrue,
                          merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n1 + 1))
                      )::Nil
                  )::Nil
                case _ =>
                  IfCode(second_condition,
                      codeIfTrue,
                      merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n1 + 1))
                  )::Nil
              }
            }
        // existing code : newValue == n1, new condition : newValue >= n2
        case (IfCode(oldCondition@EApply(ESelect(EIdent("newValue"), "$eq$eq"), List(EConstantNumber(n1))), codeIfTrue, codeIfFalse)::Nil,
              EApply(ESelect(EIdent("newValue"), "$greater$eq"), List(EConstantNumber(n2)))) =>
            if(n2 < n1) {
              val first_condition = newValue_ident <= n2 - 1
              val second_condition = newValue_ident <= n1 - 1
              val third_condition = newValue_ident <= n1
              minReachableInt filter (_ > n2 - 1) match {
                case None =>
                  IfCode(first_condition,
                      codeIfFalse,
                      IfCode(second_condition,
                          merge_code(shapes, codeIfFalse, new_code),
                          IfCode(third_condition,
                              merge_code(shapes, codeIfTrue, new_code),
                              merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n1 + 1))
                          )::Nil
                      )::Nil
                  )::Nil
                case _ =>
                  minReachableInt filter (_ > n1 - 1) match {
                    case None =>
                      IfCode(second_condition,
                          merge_code(shapes, codeIfFalse, new_code),
                          IfCode(third_condition,
                              merge_code(shapes, codeIfTrue, new_code),
                              merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n1 + 1))
                          )::Nil
                      )::Nil
                    case _ =>
                      IfCode(third_condition,
                          merge_code(shapes, codeIfTrue, new_code),
                          merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n1 + 1))
                      )::Nil
                  }
              }
            } else if(n2 == n1) {
              val first_condition = newValue_ident <= n1 - 1
              val second_condition = newValue_ident <= n1
              minReachableInt filter (_ > n1 - 1) match {
                case None =>
                  IfCode(first_condition,
                      codeIfFalse,
                      IfCode(second_condition,
                          merge_code(shapes, codeIfTrue, new_code),
                          merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n1 + 1))
                      )::Nil
                  )::Nil
                case _ =>
                  IfCode(second_condition,
                      merge_code(shapes, codeIfTrue, new_code),
                      merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n1 + 1))
                  )::Nil
              }
            } else { // n2 > n1
              val first_condition = newValue_ident <= n1 - 1
              val second_condition = newValue_ident <= n1
              
              minReachableInt filter (_ > n1 - 1) match {
                case None =>
                  IfCode(first_condition,
                      codeIfFalse,
                      IfCode(second_condition,
                          codeIfTrue,
                          merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n1 + 1))
                      )::Nil
                  )::Nil
                case _ =>
                  IfCode(second_condition,
                     codeIfTrue,
                      merge_code_recursive(shapes, codeIfFalse, new_code, new_condition, Some(n1 + 1))
                  )::Nil
              }
            }
        // existing code : IfThenElse, new condition : true
        case (IfCode(condition, codeIfTrue, codeIfFalse)::Nil, Expression.NONE) =>
          IfCode(condition, merge_code(shapes, codeIfTrue, new_code), merge_code_recursive(shapes, codeIfFalse, new_code, Expression.NONE))::Nil
        case (existing_code, Expression.NONE) =>
          merge_code(shapes, new_code, existing_code)
        case (existing_code, EConstantBoolean(true)) =>
          merge_code(shapes, new_code, existing_code)
        case (existing_code, EApply(ESelect(EIdent("newValue"), "$greater$eq"), List(EConstantNumber(n2)))) =>
          minReachableInt filter (_ >= n2) match {
            case None =>
              IfCode(newValue_ident <= (n2 - 1), existing_code, merge_code_recursive(shapes, existing_code, new_code, new_condition, Some(n2)))::Nil
            case _ =>
              merge_code(shapes, existing_code, new_code)
          }
        case (existing_code, new_condition) =>
          IfCode(new_condition, merge_code(shapes, existing_code, new_code), existing_code)::Nil
      }
    }
  
  /** Creates a code that correspond to the current transform of the game and according to the causal event. */
  def recoverCodeFromWorldModification(game: Game, causeEvent: TriggerEvent, new_condition: Expression, existing_code: List[Expression]): List[Expression] = {
    var new_code: List[Expression] = Nil
    val causeEventCode = if(causeEvent != null) causeEvent.code else -1

    val xFrom = causeEvent.x1
    val yFrom = causeEvent.y1
    val xTo = causeEvent.x2
    val yTo = causeEvent.y2
    val shapeEvent = causeEvent.shape1
    val eventOldValue = xTo.toInt
    val eventNewValue = yTo.toInt
    val movementIsHorizontal = Math.abs(xTo - xFrom) > 10 * Math.abs(yTo - yFrom)
    val movementIsVertical = Math.abs(yTo - yFrom) > 10 * Math.abs(xTo - xFrom)

    var variablesToStore = new HashMap[Expression, Int]()
    var currentVariableCounter:Int = 1
    
    def variableName(counter: Int) = "$" + counter
    def getVariableFor(e: Expression): String = {
      val res = variablesToStore.getOrElse(e, currentVariableCounter)
      if(res == currentVariableCounter) {
        currentVariableCounter = currentVariableCounter + 1
        variablesToStore(e) = res
      }
      variableName(res)
    }
    def getAssignmentFor(e: Expression) = {
      
    }
    
    // Goes through each shape and each attribute to check for potential change.
    game.getArena foreach { shape: GameShapes.Shape =>
      val shape_ident = EIdentShape(shape)
      if(shape.prev_x != shape.x) {
        var possibilities:List[Expression] = Nil
        if(causeEventCode == TOUCHMOVE_EVENT) {
          if(almostTheSame(shape.x - shape.prev_x, yFrom - yTo) && !movementIsHorizontal) {
            possibilities = (shape_ident.x += yFrom_ident - yTo_ident)::possibilities
          }
          if(almostTheSame(shape.x - shape.prev_x, yTo - yFrom) && !movementIsHorizontal) {
            possibilities = (shape_ident.x += yTo_ident - yFrom_ident)::possibilities
          }
        }
        possibilities = (shape_ident.x += (shape.x.toInt - shape.prev_x.toInt))::possibilities
        possibilities = (shape_ident.x = shape.x.toInt)::possibilities
        if(causeEventCode == TOUCHMOVE_EVENT) {
          if(almostTheSame(shape.x - shape.prev_x, xFrom - xTo) && !movementIsVertical) {
            possibilities = (shape_ident.x += xFrom_ident - xTo_ident)::possibilities
          }
          if(almostTheSame(shape.x - shape.prev_x, xTo - xFrom) && !movementIsVertical) {
            possibilities = (shape_ident.x += xTo_ident - xFrom_ident)::possibilities
          }
        }
        new_code = ParallelExpressions(possibilities)::new_code
      }
      if(shape.prev_y != shape.y) {
        var possibilities:List[Expression] = Nil
        if(causeEventCode == TOUCHMOVE_EVENT) {
          if(almostTheSame(shape.y - shape.prev_y, xFrom - xTo) && !movementIsVertical) {
            possibilities = (shape_ident.y += xFrom_ident - xTo_ident)::possibilities
          }
          if(almostTheSame(shape.y - shape.prev_y, xTo - xFrom) && !movementIsVertical) {
            possibilities =(shape_ident.y += xTo_ident - xFrom_ident)::possibilities
          }
        }
        possibilities = (shape_ident.y += (shape.y.toInt - shape.prev_y.toInt))::possibilities
        possibilities = (shape_ident.y = (shape.y.toInt))::possibilities
        if(causeEventCode == TOUCHMOVE_EVENT) {
          if(almostTheSame(shape.y - shape.prev_y, yFrom - yTo) && !movementIsHorizontal) {
            possibilities = (shape_ident.y += yFrom_ident - yTo_ident)::possibilities
          }
          if(almostTheSame(shape.y - shape.prev_y, yTo - yFrom) && !movementIsHorizontal) {
            possibilities = (shape_ident.y += yTo_ident - yFrom_ident)::possibilities
          }
        }
        new_code = ParallelExpressions(possibilities)::new_code
      }
      // If the angle changed for a great amount.
      if(shape.prev_angle != shape.angle && Math.abs(shape.angle - shape.prev_angle) > 10) {
        val shiftAngle = Math.round((shape.angle - shape.prev_angle)/15)*15
        val roundedAngle = Math.round(shape.angle/15)*15
        var possibilities:List[Expression] = Nil
        //if(roughlyProportional)
        possibilities = (shape_ident.angle += shiftAngle)::possibilities
        possibilities = (shape_ident.angle = roundedAngle)::possibilities
        new_code = ParallelExpressions(possibilities)::new_code
      }
      // Change in velocity recorded only if the angle did not change or the initial velocity was null
      if(shape.prev_velocity != shape.velocity && (Math.round(Math.abs(shape.angle - shape.prev_angle)/15)*15 == 0 || shape.prev_velocity == 0 || shape.velocity == 0 || shape.velocity / shape.prev_velocity > 2 || shape.prev_velocity / shape.velocity > 2)) {
        var possibilities:List[Expression] = Nil
        possibilities = (shape_ident.velocity = shape.velocity)::possibilities
        if(shape.prev_velocity != 0 && shape.velocity != 0) {
          possibilities = (shape_ident.velocity *= (shape.velocity / shape.prev_velocity))::possibilities
        }
        new_code = ParallelExpressions(possibilities)::new_code
      }
      if(shape.prev_color != shape.color) {
        new_code = (shape_ident.color = (shape.color))::new_code
      }
      if(shape.prev_visible != shape.visible) {
        new_code = (shape_ident.visible = (EConstant(shape.visible)))::new_code
      }
      shape match {
        case shape:Rectangular =>
          if(shape.prev_width != shape.width) {
            var possibilities:List[Expression] = Nil
            possibilities = (shape_ident.width += (shape.width - shape.prev_width))::possibilities
            possibilities = (shape_ident.width *= (shape.width.toFloat / shape.prev_width))::possibilities
            possibilities = (shape_ident.width = shape.width)::possibilities
            if(causeEventCode == TOUCHMOVE_EVENT) {
              if(almostTheSame(shape.width - shape.prev_width, xTo - xFrom) && !movementIsVertical) {
                possibilities = (shape_ident.width += (xTo_ident - xFrom_ident))::possibilities
              }
            }
            new_code = ParallelExpressions(possibilities)::new_code
          }
          if(shape.prev_height != shape.height) {
            var possibilities:List[Expression] = Nil
            possibilities = (shape_ident.height += (shape.height - shape.prev_height))::possibilities
            possibilities = (shape_ident.height *= (shape.height.toFloat / shape.prev_height))::possibilities
            possibilities = (shape_ident.height = shape.height)::possibilities
            if(causeEventCode == TOUCHMOVE_EVENT) {
              if(almostTheSame(shape.height - shape.prev_height, yTo - yFrom) && !movementIsHorizontal) {
                possibilities = (shape_ident.height += yTo_ident - yFrom_ident)::possibilities
              }
            }
            new_code = ParallelExpressions(possibilities)::new_code
          }
          // Special case of integer that need to change.
          shape match {
            case d: IntegerBox =>
              if(d.prev_value != d.value && d != shapeEvent) {
                val dOldValue = d.prev_value
                val dNewValue = d.value
                var possibleExpressions:List[Expression] = Nil
                possibleExpressions = (shape_ident.value = d.value)::possibleExpressions
                if(Math.abs(d.value - d.prev_value) > 1) {
                  possibleExpressions = (shape_ident.value += (d.value - d.prev_value))::possibleExpressions
                }
                if(dNewValue == dOldValue / 2) {
                   possibleExpressions = (shape_ident.value /= 2)::possibleExpressions
                }
                //var d_encountered = false
                game.getArena foreach {
                  case (d2:IntegerBox) =>
                    if(d2 != d) {
                      var dValue = d2.prev_value //if(d_encountered) d2.prev_value else d2.value
                      if(dNewValue == dValue) {
                        possibleExpressions = (shape_ident.value = EIdentShape(d2).prev_value)::possibleExpressions
                      }
                      if(dNewValue == dOldValue + dValue) {
                        possibleExpressions = (shape_ident.value += EIdentShape(d2).prev_value)::possibleExpressions
                      }
                      if(dNewValue == dOldValue - dValue) {
                        possibleExpressions = (shape_ident.value -= EIdentShape(d2).prev_value)::possibleExpressions
                      }
                      if(dNewValue == dOldValue * dValue) {
                        possibleExpressions = (shape_ident.value *= EIdentShape(d2).prev_value)::possibleExpressions
                      }
                      if(dValue != 0 && dNewValue == dOldValue / dValue) {
                        possibleExpressions = (shape_ident.value /= EIdentShape(d2).prev_value)::possibleExpressions
                      }
                      if(dNewValue == 2*dValue) {
                        possibleExpressions = (shape_ident.value = (EIdentShape(d2).prev_value * 2))::possibleExpressions
                      }
                    }/* else {
                      d_encountered = true
                    }*/
                  case _ =>
                }
                //var d1_before_d = true
                //var d2_before_d = true
                //var previous_d1:IntegerBox = null
                game.getArena foreachPair {
                  case (d1:IntegerBox, d2:IntegerBox) =>
                    var d1Value = d1.prev_value
                    var d2Value = d2.prev_value
                    /*if(previous_d1 != d1) {
                      d2_before_d = true
                      if(d1 == d) {
                        d1_before_d = false
                        d2_before_d = false
                      }
                      if(!d1_before_d) d2_before_d = false
                      previous_d1 = d1
                    }
                    if(d1 == d) d1_before_d = false
                    if(d2 == d) d2_before_d = false
                    if(d1_before_d) d1Value = d1.value
                    if(d2_before_d) d2Value = d2.value*/
                    // d1Value contains the value of d1 at the current state of the program.
                    // d2Value contains the value of d2 at the current state of the program.
                    if(d1 != d || d2 != d) {
                      if(dNewValue == d1Value + d2Value) {
                        possibleExpressions = (shape_ident.value = EIdentShape(d1).prev_value + EIdentShape(d2).prev_value)::possibleExpressions
                      }
                      if(dNewValue == d1Value - d2Value) {
                        possibleExpressions = (shape_ident.value = EIdentShape(d1).prev_value - EIdentShape(d2).prev_value)::possibleExpressions
                      }
                      if(dNewValue == d2Value - d1Value) {
                        possibleExpressions = (shape_ident.value = EIdentShape(d2).prev_value - EIdentShape(d1).prev_value)::possibleExpressions
                      }
                      if(dNewValue == d1Value * d2Value) {
                        possibleExpressions = (shape_ident.value = EIdentShape(d1).prev_value * EIdentShape(d2).prev_value)::possibleExpressions
                      }
                      if(d2Value != 0 && dNewValue == d1Value / d2Value) {
                        possibleExpressions = (shape_ident.value = EIdentShape(d1).prev_value / EIdentShape(d2).prev_value)::possibleExpressions
                      }
                    }
                  case _ =>
                }
                if(Math.abs(d.value - d.prev_value) <= 1 && d.value != d.prev_value) {
                  possibleExpressions = (shape_ident.value += (d.value - d.prev_value))::possibleExpressions
                }
                causeEventCode match { case INTEGER_CHANGE_EVENT | INTEGER_EQUAL_EVENT | INTEGER_GREATER_EQUAL_EVENT | INTEGER_LESS_EQUAL_EVENT | INTEGER_POSITIVE_EVENT | INTEGER_NEGATIVE_EVENT =>
                  //var d_encountered = false
                  game.getArena foreach {
                  case (d2:IntegerBox) =>
                    if(d2 != d) {
                      var dValue = d2.prev_value // if(d_encountered) d2.prev_value else d2.value
                      if(dNewValue == dValue - eventNewValue) {
                        possibleExpressions = (shape_ident.value = EIdentShape(d2).prev_value - newValue_ident)::possibleExpressions
                      }
                      if(dNewValue == eventNewValue - dValue) {
                        possibleExpressions = (shape_ident.value = newValue_ident - EIdentShape(d2).prev_value)::possibleExpressions
                      }
                      if(dNewValue == eventNewValue + dValue) {
                        possibleExpressions = (shape_ident.value = newValue_ident + EIdentShape(d2).prev_value)::possibleExpressions
                      }
                      if(dNewValue == eventNewValue * dValue) {
                        possibleExpressions = (shape_ident.value = newValue_ident * EIdentShape(d2).prev_value)::possibleExpressions
                      }
                      if(dValue != 0 && dNewValue == eventNewValue / dValue) {
                        possibleExpressions = (shape_ident.value = newValue_ident / EIdentShape(d2).prev_value)::possibleExpressions
                      }
                    }// else d_encountered = true
                    
                  case _ =>
                  }
                  
                  if(dNewValue == eventOldValue) {
                     possibleExpressions = (shape_ident.value = oldValue_ident)::possibleExpressions
                  }
                  if(dNewValue == eventOldValue - eventNewValue) {
                     possibleExpressions = (shape_ident.value = (oldValue_ident - newValue_ident))::possibleExpressions
                  }
                  if(dNewValue == eventNewValue - eventOldValue) {
                     possibleExpressions = (shape_ident.value = (newValue_ident - oldValue_ident))::possibleExpressions
                  }
                  
                  if(dNewValue == eventNewValue * 2) {
                     possibleExpressions = (shape_ident.value = (newValue_ident * 2))::possibleExpressions
                  }
                  if(dNewValue == eventNewValue / 2 && eventNewValue % 2 == 0) {
                     possibleExpressions = (shape_ident.value = (newValue_ident / 2))::possibleExpressions
                  }
                  
                  if(dNewValue == dOldValue + eventOldValue - eventNewValue) {
                     possibleExpressions = (shape_ident.value += (oldValue_ident - newValue_ident))::possibleExpressions
                  }
                  if(dNewValue == dOldValue + eventNewValue - eventOldValue) {
                     possibleExpressions = (shape_ident.value += (newValue_ident - oldValue_ident))::possibleExpressions
                  }
                  if(dNewValue == eventNewValue) {
                     possibleExpressions = (shape_ident.value = newValue_ident)::possibleExpressions
                  }
                case _ =>
                }
                /*if(d.value == d.prev_value) {
                  possibleExpressions = (shape_ident.value = shape_ident.value)::possibleExpressions
                }*/
                new_code = ParallelExpressions(possibleExpressions)::new_code
              } else {
                // We store a context for this value in case this rule should change it later.
                /*game.getArena foreach {
                  case (d2:IntegerBox) =>
                    
                }*/
              }
            case d:TextBox =>
              if(d.prev_text != d.text) {
                val dOldText = d.prev_text
                val dNewText = d.text
                var possibleExpressions:List[Expression] = Nil
                game.getArena foreach {
                  case (d2:TextBox) if d2.prev_text == d.text =>
                    possibleExpressions = (shape_ident.text = EIdentShape(d2).prev_text)::possibleExpressions
                  case _ =>
                }
                //var d1_before_d = true
                //var d2_before_d = true
                //var previous_d1:TextBox = null
                game.getArena foreachPair {
                  case (d1:TextBox, d2:TextBox) =>
                    var d1Value = d1.prev_text
                    var d2Value = d2.prev_text
                    /*if(previous_d1 != d1) {
                      d2_before_d = true
                      if(d1 == d) {
                        d1_before_d = false
                        d2_before_d = false
                      }
                      if(!d1_before_d) d2_before_d = false
                      previous_d1 = d1
                    }
                    if(d1 == d) d1_before_d = false
                    if(d2 == d) d2_before_d = false
                    if(d1_before_d) d1Value = d1.text
                    if(d2_before_d) d2Value = d2.text*/
                    // d1Value contains the value of d1 at the current state of the program.
                    // d2Value contains the value of d2 at the current state of the program.
                    if(dNewText == d1Value + d2Value) {
                      possibleExpressions = (shape_ident.text = EIdentShape(d1).prev_text + EIdentShape(d2).prev_text)::possibleExpressions
                    }
                    if(dNewText == d2Value + d1Value) {
                      possibleExpressions = (shape_ident.text = EIdentShape(d2).prev_text + EIdentShape(d1).prev_text)::possibleExpressions
                    }
                  case _ =>
                }
                possibleExpressions = (shape_ident.text = d.text)::possibleExpressions
                new_code = ParallelExpressions(possibleExpressions)::new_code
              }
            case _ =>
              
          }
        case shape:Circle =>
          if(shape.prev_radius != shape.radius) {
            var possibilities:List[Expression] = Nil
            possibilities = (shape_ident.radius += (shape.radius - shape.prev_radius))::possibilities
            possibilities = (shape_ident.radius *= (shape.radius.toFloat / shape.prev_radius))::possibilities
            possibilities = (shape_ident.radius = (shape.radius))::possibilities
            if(causeEventCode == TOUCHMOVE_EVENT) {
              if(almostTheSame(shape.radius - shape.prev_radius, xTo - xFrom) && !movementIsVertical) {
                possibilities = (shape_ident.radius += (xTo_ident - xFrom_ident))::possibilities
              }
              if(almostTheSame(shape.radius - shape.prev_radius, xFrom - xTo) && !movementIsVertical) {
                possibilities = (shape_ident.radius += (xFrom_ident - xTo_ident))::possibilities
              }
              if(almostTheSame(shape.radius - shape.prev_radius, yTo - yFrom) && !movementIsHorizontal) {
                possibilities = (shape_ident.radius += (yTo_ident - yFrom_ident))::possibilities
              }
              if(almostTheSame(shape.radius - shape.prev_radius, yFrom - yTo) && !movementIsHorizontal) {
                possibilities = (shape_ident.radius += (yFrom_ident - yTo_ident))::possibilities
              }
            }
            new_code = ParallelExpressions(possibilities)::new_code
          }
        case _ =>
      }
    }
    //}
    new_code = new_code.reverse
    
    // We merge the existing code and the generated code.
    // Check for existing conditions.
    merge_code_recursive(game.getArena, existing_code, new_code, new_condition)
  }
  
  /**
   * Modify an existing rule and calls the ruleHandler when all the changes are made.
   */
  def modifyAndInsertRule(context: Context, game: Game, r: ReactiveRule, causeTimestamp: Long, ruleHandler: ReactiveRule => Unit = null): Unit = {
    if(r.code != Nil) {
      r.removeCompiledBlocks()
      // Build the equivalence classes if no if-then-else.
      var equivalences: List[Int] = Nil
      var codeFragments: List[Expression] = Nil
      var codeFragmentsStrings: List[String] = Nil
      
      /**
       * Expands the syntax tree to a list and launch a dialog box for user interaction,
       * to let him choose the code portion he wants
       */
      def expandCode(existing_code: List[Expression], currentEquivalenceClass: Int, indent: String):Int = existing_code match {
        case Nil =>
          currentEquivalenceClass + 1
        case ParallelExpressions(a::q)::remaining =>
          codeFragments = a::codeFragments
          codeFragmentsStrings = a.toScalaString( currentEquivalenceClass + ": " + indent, game.context)::codeFragmentsStrings
          equivalences = currentEquivalenceClass :: equivalences
          expandCode(ParallelExpressions(q)::remaining, currentEquivalenceClass, indent)
        case ParallelExpressions(Nil)::remaining =>
          expandCode(remaining, currentEquivalenceClass + 1, indent)
        case (i@IfCode(condition, codeIfTrue, codeIfFalse))::remaining_code =>
          codeFragments = IfCode(condition, Nil, Nil)::codeFragments
          codeFragmentsStrings = " : " + indent + i.headerToScalaString("", game.context)::codeFragmentsStrings
          equivalences = currentEquivalenceClass :: equivalences
          val newEquivalenceClass = expandCode(codeIfTrue, currentEquivalenceClass + 1, indent + "  ")
          if(codeIfFalse != Nil) {
            codeFragments = Expression.NONE::codeFragments
            codeFragmentsStrings = (" : " + indent + "} else {")::codeFragmentsStrings
            equivalences = (newEquivalenceClass + 1) :: equivalences
            val remaining_e2 = expandCode(codeIfFalse, newEquivalenceClass + 2, indent + "  ")
            codeFragments = Expression.NONE::codeFragments
            codeFragmentsStrings = (" : " + indent + "}")::codeFragmentsStrings
            equivalences = (remaining_e2) :: equivalences
            expandCode(remaining_code, remaining_e2, indent)
          } else {
            codeFragments = Expression.NONE::codeFragments
            codeFragmentsStrings = (" : " + indent + "}")::codeFragmentsStrings
            equivalences = (newEquivalenceClass + 1) :: equivalences
            expandCode(remaining_code, newEquivalenceClass + 2, indent)
          }
        case a::remaining =>
          codeFragments = a::codeFragments
          codeFragmentsStrings = a.toScalaString( currentEquivalenceClass + ": " + indent, game.context)::codeFragmentsStrings
          equivalences = currentEquivalenceClass :: equivalences
          expandCode(remaining, currentEquivalenceClass + 1, indent)
      }
      expandCode(r.code, 1, "")
      equivalences = equivalences.reverse
      codeFragments = codeFragments.reverse
      codeFragmentsStrings = codeFragmentsStrings.reverse
      
      CustomDialogs.launchRuleMakerDialog(context, context.getResources().getString(R.string.choose_rule), r.headerToScalaString("", game.context), r.footerToScalaString(""), r,
          codeFragmentsStrings, codeFragments, equivalences, 
          {
            rule => game.insertRule(rule, causeTimestamp)
               if(ruleHandler != null) ruleHandler(rule)
          }, {() => })
    }
  }
  
  /**
   * Recovers an existing rule from an event or creates a new one.
   */
  def getRuleFromEvent(game: Game, causeEvent: TriggerEvent): Option[ReactiveRule] = {
    causeEvent.code match {
      case COLLISION_EVENT =>
          val lookupPair = GameShapes.ShapePair(causeEvent.shape1, causeEvent.shape2)
          Some(game.added_whenCollisionRules.getOrElse(lookupPair, WhenCollisionBetweenRule(EIdentShape(causeEvent.shape1), EIdentShape(causeEvent.shape2), Nil)))
      case INTEGER_CHANGE_EVENT | INTEGER_EQUAL_EVENT | INTEGER_GREATER_EQUAL_EVENT | INTEGER_LESS_EQUAL_EVENT | INTEGER_POSITIVE_EVENT | INTEGER_NEGATIVE_EVENT =>
        val d = causeEvent.shape1.asInstanceOf[IntegerBox]
        val shape_ident = EIdentShape(causeEvent.shape1)
        Some(game.added_whenIntegerChangesRules.getOrElse(causeEvent.shape1.asInstanceOf[IntegerBox],
            WhenIntegerChangesRule(EIdentShape(causeEvent.shape1), List(oldValue_ident, newValue_ident), Nil)
        ))
      case TOUCHDOWN_EVENT | TOUCHUP_EVENT => 
          val down_event = causeEvent
          var shapeBelow: GameShapes.Shape = null
          var minDistance = -1f
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
                conditions = (shape_ident.x + shape_ident.width < 0)::conditions
              }
              if(causeEvent.x1 == game.screenWidth || causeEvent.code == BEYOND_SCREEN_EVENT) {
                conditions = (shape_ident.x > EScreenWidth())::conditions
              }
              if(causeEvent.y1 == 0 || causeEvent.code == BEYOND_SCREEN_EVENT) {
                conditions = (shape_ident.y + shape_ident.height < 0)::conditions
              }
              if(causeEvent.y1 == game.screenHeight || causeEvent.code == BEYOND_SCREEN_EVENT) {
                conditions = (shape_ident.y > EScreenHeight())::conditions
              }
            case c:Circle =>
              if(causeEvent.x1 == 0 || causeEvent.code == BEYOND_SCREEN_EVENT) {
                conditions = (shape_ident.x + shape_ident.radius < 0)::conditions
              }
              if(causeEvent.x1 == game.screenWidth || causeEvent.code == BEYOND_SCREEN_EVENT) {
                conditions = (shape_ident.x - shape_ident.radius > EScreenWidth())::conditions
              }
              if(causeEvent.y1 == 0 || causeEvent.code == BEYOND_SCREEN_EVENT) {
                conditions = (shape_ident.y + shape_ident.radius < 0)::conditions
              }
              if(causeEvent.y1 == game.screenHeight || causeEvent.code == BEYOND_SCREEN_EVENT) {
                conditions = (shape_ident.y - shape_ident.radius > EScreenHeight())::conditions
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
                case None => Some(WhenEverRule(a, Nil))
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
          var shapeBelow: GameShapes.Shape = null
          var minDistance = -1f
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
          if(shapeBelow != null) {
            Some(game.added_whenFingerMovesOnRules.getOrElse(shapeBelow, WhenFingerMovesOnRule(EIdentShape(shapeBelow), List(EIdent("xFrom"), EIdent("yFrom"), EIdent("xTo"), EIdent("yTo")), Nil)))
          } else None
    }
  }
  
  /**
   * Creates a rule based on the selected event and the status of the game.
   * This function generates the condition to which the code has been modified.
   **/
  def createRule(context: Context, game: Game, causeEvent: TriggerEvent, causeTimestamp: Long, ruleHandler : ReactiveRule => Unit) = {
    val res = context.getResources()
    def askChoicesIfNeeded(r: ReactiveRule) = {
      modifyAndInsertRule(context, game, r, causeTimestamp, ruleHandler)
    }

    if(causeEvent != null) {
      val ruleToModify = getRuleFromEvent(game, causeEvent)
      ruleToModify match {
        case Some(w) =>
          val condition:Expression = causeEvent.code match {
            case INTEGER_CHANGE_EVENT =>
              EConstantBoolean(true)
            case INTEGER_EQUAL_EVENT =>
              newValue_ident equals causeEvent.y2.toInt
            case INTEGER_GREATER_EQUAL_EVENT =>
              newValue_ident >= causeEvent.y2.toInt
            case INTEGER_LESS_EQUAL_EVENT =>
              newValue_ident <= causeEvent.y2.toInt
            case INTEGER_POSITIVE_EVENT =>
              newValue_ident >= 1
            case INTEGER_NEGATIVE_EVENT =>
              newValue_ident <= -1
            case BEYOND_SCREEN_EVENT | BEYOND_SIDE_EVENT => 
              var conditions:List[Expression] = Nil
              val shape_ident = EIdentShape(causeEvent.shape1)
              causeEvent.shape1 match {
                case r:Rectangular =>
                  if(causeEvent.x1 == 0 || causeEvent.code == BEYOND_SCREEN_EVENT) {
                    conditions = (shape_ident.x + shape_ident.width < 0)::conditions
                  }
                  if(causeEvent.x1 == game.screenWidth || causeEvent.code == BEYOND_SCREEN_EVENT) {
                    conditions = (shape_ident.x > EScreenWidth())::conditions
                  }
                  if(causeEvent.y1 == 0 || causeEvent.code == BEYOND_SCREEN_EVENT) {
                    conditions = (shape_ident.y + shape_ident.height < 0)::conditions
                  }
                  if(causeEvent.y1 == game.screenHeight || causeEvent.code == BEYOND_SCREEN_EVENT) {
                    conditions = (shape_ident.y > EScreenHeight())::conditions
                  }
                case c:Circle =>
                  if(causeEvent.x1 == 0 || causeEvent.code == BEYOND_SCREEN_EVENT) {
                    conditions = (shape_ident.x + shape_ident.radius < 0)::conditions
                  }
                  if(causeEvent.x1 == game.screenWidth || causeEvent.code == BEYOND_SCREEN_EVENT) {
                    conditions = (shape_ident.x - shape_ident.radius > EScreenWidth())::conditions
                  }
                  if(causeEvent.y1 == 0 || causeEvent.code == BEYOND_SCREEN_EVENT) {
                    conditions = (shape_ident.y + shape_ident.radius < 0)::conditions
                  }
                  if(causeEvent.y1 == game.screenHeight || causeEvent.code == BEYOND_SCREEN_EVENT) {
                    conditions = (shape_ident.y - shape_ident.radius > EScreenHeight())::conditions
                  }
                case _ =>
              }
              val condition: Expression = conditions match {
                case Nil =>
                  EConstantBoolean(true)
                case a::Nil =>
                  a
                case l =>
                  conditions.reduceLeft(_ or _)
              }
              condition
            case _ =>
              Expression.NONE
          }
          w.removeCompiledBlocks()
          w.code = CodeGenerator.recoverCodeFromWorldModification(game, causeEvent, condition, w.code)
          askChoicesIfNeeded(w)
        case None =>
          //CodeGenerator.recoverCodeFromWorldModification(game, causeEvent, Nil)
      }
    }
  }
  
  /**
   * Duplicates all rules concerning shape1 for shape2
   * This function should be deprecated once the categories are used.
   **/
  def duplicateRuleContaining(game: Game, shape1: GameShapes.Shape, shape2: GameShapes.Shape) = {
    game.init_rules foreach {
      rule =>
        if(rule.contains(shape1.mName)) {
          val new_rule = rule.replaceShape(shape1, shape2)
          game.insertRule(new_rule, game.currentTime + 1) // Add the rule in the future
        }
    }
  }
}