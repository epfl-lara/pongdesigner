package ch.epfl.lara.synthesis.kingpong.common

import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.menus._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import scala.collection.mutable.ArrayBuffer
import ch.epfl.lara.synthesis.kingpong.RulesManager

/**
 * Actions which can be undone.
 */
object UndoRedo {
  
  /**
   * Undo actions
   * @param i The number of actions to undo
   */
  def undo(i: Int = 1): Unit = {
    if(pointer != -1 && pointer < undoRedoBuffer.length) {
      val elem = undoRedoBuffer(pointer)
      pointer -= 1
      elem.undo()
      if(i > 1) undo(i-1)
    }
  }
  
  /**
   * Redo actions based on the current pointer
   * @param i The number of actions to redo
   */
  def redo(i: Int = 1): Unit = {
    if(pointer < undoRedoBuffer.length - 1) {
      pointer += 1
      val elem = undoRedoBuffer(pointer)
      elem.redo()
      if(i > 1) redo(i-1)
    }
  }
  
  /**
   * @return An ordered list of undos that can be done from that point.
   */
  def getUndos(): List[String] = {
    undoRedoBuffer.take(pointer+1).map(_.comment).toList.reverse
  }
  
  /**
   * @return An ordered list of redos that can be done from that point.
   */
  def getRedos(): List[String] = {
    undoRedoBuffer.drop(pointer+1).map(_.comment).toList
  }
  
  private val undoRedoBuffer = ArrayBuffer[Action]()
  private var pointer = -1
  
  /**
   * Actions to undo or redo
   */
  private trait Action {
    def undo(): Unit
    def redo(): Unit
    def comment: String
  }
  
  /**
   * A change of property current or next value
   * We keep the policy but it is not needed in practice.
   */
  private case class PropertySetPrevNext[@specialized T](property: RWProperty[T], prev_current_value: T, prev_next_value: T, new_value: T, implicit val modify_policy: MenuOptions.Policy) extends Action {
    def undo() = {
      property.set(prev_current_value)
      property.setNext(prev_next_value)
    }
    def redo() = { property.setPrevNext(new_value) }
    def comment = "Change " + property.name
  }
  
  /**
   * A change of a rule in the game at a given index
   */
  private case class SetRuleByIndex(game: RulesManager, index: Int, prevRule: Expr, newRule: Expr) extends Action {
    def undo() = { game.setRuleByIndex(prevRule, index) }
    def redo() = { game.setRuleByIndex(newRule, index) }
    def comment = "Replace rule"
  }
  
  /**
   * An addition of a rule in the game at a given index
   */
  private case class AddRule(game: RulesManager, index: Int, newRule: Expr) extends Action {
    def undo() = { game.removeRule(newRule) }
    def redo() = { game.addRule(newRule) }
    def comment = "Create rule"
  }

  /**
   * Clears the undo/redo buffer
   */
  def clear() = {
    undoRedoBuffer.clear()
    pointer = -1
  }
  
  /**
   * Clears the undo buffer except for added or replaced rules.
   */
  def clearButKeepRules() = {
    var i = 0
    while(i < undoRedoBuffer.length) {
      undoRedoBuffer(i) match {
        case e:SetRuleByIndex => // Keep it
        case e:AddRule =>
        case _ =>
          undoRedoBuffer.remove(i)
          if(pointer >= i) {
            pointer -= 1
          }
          i -= 1
      }
      i += 1
    }
    assert(pointer <= undoRedoBuffer.length - 1 && pointer >= -1) 
  }
  
  /**
   * Add an action.
   * If a similar action has just been performed, merge them in one.
   */
  def addAction(a: Action) = {
    if(undoRedoBuffer.length -1 > pointer) { // Make sure there are no redo possibles afterwards
      undoRedoBuffer.reduceToSize(pointer + 1)
    }
    val action_to_add = if(pointer > -1) {
      val last = undoRedoBuffer(pointer)
      (last, a) match {
        case (PropertySetPrevNext(p1, pv1, nv1, n1, m1), PropertySetPrevNext(p2, pv2, nv2, n2, m2))
          if((p1 eq p2) && m1 == m2)
        =>
          undoRedoBuffer.remove(pointer) 
          PropertySetPrevNext(p1, pv1, nv1, n2, m2)
        case _ =>
          a
      }
    } else a
    undoRedoBuffer += action_to_add
    pointer = undoRedoBuffer.length - 1
  }
  
  def recordSetPrevNext[T](property: RWProperty[T], value: T)(implicit modify_policy: MenuOptions.Policy) = {
    if((modify_policy.modifiesCurrent && value != property.get) || (modify_policy.modifiesNext && value != property.next))
      addAction(PropertySetPrevNext(property, property.get, property.next, value, modify_policy.withNotUndoable))
  }
  def recordRuleUpdate(game: RulesManager, index: Int, prevRule: Expr, newRule: Expr) = {
    addAction(SetRuleByIndex(game, index, prevRule, newRule))
  }
  def recordRuleAdd(game: RulesManager, index: Int, newRule: Expr) = {
    addAction(AddRule(game, index, newRule))
  }
}