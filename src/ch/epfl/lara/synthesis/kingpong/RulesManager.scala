package ch.epfl.lara.synthesis.kingpong

import scala.collection.mutable.{HashMap => MMap, Set => MSet}
import scala.collection.mutable.ArrayBuffer

import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeOps
import ch.epfl.lara.synthesis.kingpong.objects._

/**
 * Manage rules for the game.
 */
trait RulesManager {

  // Use an ArrayBuffer for performance reasons when using `foreach`.
  // An ArrayBuffer will use a simple while loop.
  private val _rules = ArrayBuffer.empty[Expr]
  private val _rulesByObject = MMap.empty[GameObject, MSet[Expr]]
  
  /** All the rules in this game. */
  def rules: Traversable[Expr] = _rules
  
  def setRuleByIndex(newRule: Expr, index: Int): Unit = {
    val oldRule = _rules(index)
    _rules(index) = newRule
    for((obj, rules) <- _rulesByObject) {
      if(rules contains oldRule) {
        rules -= oldRule
        rules += newRule
      }
    }
  }
  
  def getRuleByIndex(index: Int): Expr = _rules(index)
  def findRuleIndex(ruleMatcher: Expr => Boolean): Int = _rules.indexWhere(ruleMatcher)
  
  def getRulesbyObject(o: GameObject): Traversable[Expr] = _rulesByObject.getOrElse(o, List())
  def getRulesbyObject(o: Iterable[GameObject]): Iterable[Expr] = (o flatMap getRulesbyObject)
  
  def addRule(r: Expr): Unit = {
    def addToCategory(category: Category) {
      for(o<-category.objects) _rulesByObject.getOrElseUpdate(o, MSet()) += r
    }
    TreeOps.preTraversal(_ match {
      case ObjectLiteral(o) => _rulesByObject.getOrElseUpdate(o, MSet()) += r
      case Foreach(category, id, body) => addToCategory(category)
      case Forall(category, id, body) => addToCategory(category)
      case Find(category, id, body) => addToCategory(category)
      case c =>
    })(r)
    _rules += r
  }
}