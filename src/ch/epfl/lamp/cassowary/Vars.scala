package ch.epfl.lamp.cassowary
import scala.collection.mutable.FastSet
import scala.collection.mutable.ArrayBuffer

object AbstractVar {
  var iVariableNumber = 0

  def newName(): String = newName("v")
  def newName(prefix: String): String = if(debug) prefix + AbstractVar.iVariableNumber else prefix
}

abstract class AbstractVar(val name: String) {
  private[cassowary] var row: LinearExpression = null
  private[cassowary] var column: ArrayBuffer[AbstractVar] = null
  val index = AbstractVar.iVariableNumber
  AbstractVar.iVariableNumber += 1

  def this() = this(AbstractVar.newName())

  def isDummy: Boolean = false
  def isPivotable: Boolean = false
  def isRestricted: Boolean
}


private[cassowary] final class DummyVar(name: String) extends AbstractVar(name) {
  def this() = this(AbstractVar.newName())

  override def isDummy: Boolean = true
  def isRestricted: Boolean = true

  override def toString: String = "[" + name + ":dummy]"
}


/**
 * Artificial variable that is used to identify the objective row in the tableau.
 */
private[cassowary] final class ObjectiveVar(name: String) extends AbstractVar(name) {
  def isRestricted: Boolean = false

  override def toString: String = "[" + name + ":obj]"
}


private[cassowary] final class SlackVar(name: String) extends AbstractVar(name) {
  def this() = this(AbstractVar.newName())

  override def isPivotable: Boolean = true
  def isRestricted: Boolean = true

  override def toString: String = "[" + name + ":slack]"
}

object CVar {
  def apply(name: String, value: Double): CVar = new StdCVar(name, value)
  def apply(value: Double): CVar = apply(AbstractVar.newName(), value)
  def apply(name: String): CVar = apply(name, 0d)
}

abstract class CVar(name: String) extends AbstractVar(name) {
  def value: Double
  def value_=(v: Double): Unit

  /**
   * Called by solver internally.
   */
  def changeValue(v: Double) = value = v

  def isRestricted: Boolean = false
  def toExpr = new LinearExpression(this, 1, 0)

  def +(d: Double) = this.toExpr += d
  def +(v: CVar) = this.toExpr += v
  def -(d: Double) = this.toExpr -= d
  def -(v: CVar) = this.toExpr -= v
  def *(d: Double) = new LinearExpression(this, d, 0)

  def unary_-(): LinearExpression = this * -1d
  def :<=(rhs: Double) = new LinearInequality(this.toExpr *= -1 += rhs)
  def :>=(rhs: Double) = new LinearInequality(this.toExpr -= rhs)
  def :<=(rhs: CVar) = new LinearInequality(rhs.toExpr -= this)
  def :>=(rhs: CVar) = new LinearInequality(this.toExpr -= rhs)
  def :==(rhs: Double) = new LinearEquation(-this += rhs)
  def :==(rhs: CVar) = new LinearEquation(-this += rhs)
  def :==(rhs: LinearExpression) = new LinearEquation(-this += rhs)

  override def toString: String = "[" + name + "=" + value + "]"
}

class StdCVar(name: String, private var _value: Double) extends CVar(name) {
  def value = _value
  def value_=(v: Double) = _value = v
}