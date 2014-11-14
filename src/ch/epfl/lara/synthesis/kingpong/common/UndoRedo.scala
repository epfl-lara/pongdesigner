package ch.epfl.lara.synthesis.kingpong.common

import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.menus._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import scala.collection.mutable.ArrayBuffer
import ch.epfl.lara.synthesis.kingpong.RulesManager
import ch.epfl.lara.synthesis.kingpong.Game

/**
 * Actions which can be undone.
 */
object UndoRedo {
  
  var undoRedoCallback: () => Unit = null;
  
  /**
   * Undo actions
   * @param i The number of actions to undo
   */
  def undo(i: Int = 1): Unit = {
    if(pointer != -1 && pointer < undoRedoBuffer.length) {
      val elems = undoRedoBuffer(pointer)
      pointer -= 1
      elems.foreach(_.undo())
      if(i > 1) undo(i-1)
    }
    if(i == 1 && undoRedoCallback != null) undoRedoCallback()
  }
  
  /**
   * Redo actions based on the current pointer
   * @param i The number of actions to redo
   */
  def redo(i: Int = 1): Unit = {
    if(pointer < undoRedoBuffer.length - 1) {
      pointer += 1
      val elems = undoRedoBuffer(pointer)
      elems.foreach(_.redo())
      if(i > 1) redo(i-1)
    }
    if(i == 1 && undoRedoCallback != null) undoRedoCallback()
  }
  
  /**
   * @return An ordered list of undos that can be done from that point.
   */
  def getUndos(): List[String] = {
    undoRedoBuffer.take(pointer+1).map(_.map(_.comment).mkString(" & ")).toList.reverse
  }
  
  /**
   * @return An ordered list of redos that can be done from that point.
   */
  def getRedos(): List[String] = {
    undoRedoBuffer.drop(pointer+1).map(_.map(_.comment).mkString(" & ")).toList
  }
  
  private val undoRedoBuffer = ArrayBuffer[List[Action]]()
  private var pointer = -1
  
  /**
   * Actions to undo or redo
   */
  private trait Action {
    def undo(): Unit
    def redo(): Unit
    def comment: String
    def isAboutRule: Boolean
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
    def isAboutRule: Boolean = false
  }
  
  /**
   * A change of a rule in the game at a given index
   */
  private case class SetRuleByIndex(game: RulesManager, index: Int, prevRule: Expr, newRule: Expr) extends Action {
    def undo() = { game.setRuleByIndex(prevRule, index, undoable = false) }
    def redo() = { game.setRuleByIndex(newRule, index, undoable = false) }
    def comment = "Replace rule"
    def isAboutRule: Boolean = true
  }
  
  /**
   * An addition of a rule in the game
   */
  private case class InsertRule(game: RulesManager, index: Int, newRule: Expr) extends Action {
    def undo() = { game.removeRuleByIndex(index, undoable = false) }
    def redo() = { game.insertRule(index, newRule, undoable = false) }
    def comment = "Create rule"
    def isAboutRule: Boolean = true
  }
  
  /**
   * An removal of a rule in the game
   */
  private case class RemoveRule(game: RulesManager, index: Int, rule: Expr) extends Action {
    def undo() = { game.insertRule(index, rule, undoable = false) }
    def redo() = { game.removeRuleByIndex(index, undoable = false) }
    def comment = "Delete rule"
    def isAboutRule: Boolean = true
  }
  
   /**
    * An addition of an object in the game
    */
	  private case class AddObject(game: Game, obj: GameObject) extends Action {
	    def undo() = { game.remove(obj, undoable = false) }
	    def redo() = { game.add(obj, undoable = false) }
	    def comment = "Add "+obj.name.get
	    def isAboutRule: Boolean = false
	  }
	  
	  /**
    * A removal of an object in the game
    */
	  private case class RemoveObject(game: Game, obj: GameObject) extends Action {
	    def undo() = { game.add(obj, undoable = false) }
	    def redo() = { game.remove(obj, undoable = false) }
	    def comment = "Delete "+obj.name.get 
	    def isAboutRule: Boolean = false
	  }
	  
	  /**
    * A renaming of an object in the game
    */
	  private case class RenameObject(game: Game, obj: GameObject, oldName: String, newName: String) extends Action {
	    def undo() = { obj.name set oldName }
	    def redo() = { obj.name set newName }
	    def comment = "Rename "+obj.name.get 
	    def isAboutRule: Boolean = false
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
      if(undoRedoBuffer(i).exists(e => e.isAboutRule)) {
        undoRedoBuffer(i) = undoRedoBuffer(i).collect{case e if e.isAboutRule => e}
      } else {
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
   * If an action is set to be groupped, group them together.
   */
  def addAction(newAction: Action) = {
    if(undoRedoBuffer.length -1 > pointer) { // Make sure there are no redo possibles afterwards
      undoRedoBuffer.reduceToSize(pointer + 1)
    }
    val actions_to_add: List[Action] = if(pointer > -1) {
      val lastActions = undoRedoBuffer(pointer)
      newAction match {
        case PropertySetPrevNext(p2, pv2, nv2, n2, m2) =>
          lastActions.find{
		        case e@PropertySetPrevNext(p1, pv1, nv1, n1, m1) => (p1 eq p2) && m1 == m2
		        case _ => false
          } match {
            case Some(e@PropertySetPrevNext(p1, pv1, nv1, n1, m1)) =>
              val i = lastActions.indexWhere(_ eq e)
              val (a, b) = lastActions.splitAt(i)
              b match {
		            case b::q =>
		              undoRedoBuffer.remove(pointer) 
		              a ++ (PropertySetPrevNext(p1, pv1, nv1, n2, m2) +: q)
		            case _ => List(newAction)
		          }
		        case _ =>
		          newAction match {
				        case e@PropertySetPrevNext(p1, pv1, nv1, n1, m1) if m1.groupsModifications =>
				          undoRedoBuffer.remove(pointer) 
				          lastActions ++ List(newAction)
				        case _ =>
				          List(newAction)
		          }
		      }
        case _ =>
          List(newAction)
      }
    } else List(newAction)
    undoRedoBuffer += actions_to_add
    pointer = undoRedoBuffer.length - 1
  }
  
  def recordSetPrevNext[T](property: RWProperty[T], value: T)(implicit modify_policy: MenuOptions.Policy) = {
    if((modify_policy.modifiesCurrent && value != property.get) || (modify_policy.modifiesNext && value != property.next))
      addAction(PropertySetPrevNext(property, property.get, property.next, value, modify_policy.withNotUndoable))
  }
  def recordRuleUpdate(game: RulesManager, index: Int, prevRule: Expr, newRule: Expr) = {
    addAction(SetRuleByIndex(game, index, prevRule, newRule))
  }
  def recordRuleInsert(game: RulesManager, index: Int, newRule: Expr) = {
    addAction(InsertRule(game, index, newRule))
  }
  def recordRuleRemove(game: RulesManager, index: Int, oldRule: Expr) = {
    addAction(RemoveRule(game, index, oldRule))
  }
  def recordObjectAdd(game: Game, obj: GameObject) = {
    addAction(AddObject(game, obj))
  }
  def recordObjectRemove(game: Game, obj: GameObject) = {
    addAction(RemoveObject(game, obj))
  }
  def recordObjectRename(game: Game, obj: GameObject, oldName: String, newName: String) = {
    addAction(RenameObject(game, obj, oldName: String, newName: String))
  }
}