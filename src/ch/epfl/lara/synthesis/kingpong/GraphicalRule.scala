package ch.epfl.lara.synthesis.kingpong

import android.graphics.Canvas
import android.graphics.Paint
import android.content.Context

/**
 * ObjLayout contains static global variables
 */
object ObjLayout {
  final val margin = 2
  var colorBackground = 0
  
  def initialize(context: Context) { // Called by the main activity
    colorBackground = context.getResources().getColor(R.color.rule_background)
  }
}

/**
 * 
 */
trait ObjLayout {
  var parent: ObjLayout = null
  var x: Float = 0
  var y: Float = 0
  var width: Float = 0
  var height: Float = 0
  def draw(t: Canvas, originx: Float, originy: Float): Unit
}

trait ConditionAction extends ObjLayout {
  def draw(c: Canvas, originx: Float, originy: Float): Unit = {}
  def attach(new_parent: ObjLayout) {
    
  }
  def detach() {
    
  }
}
trait GameObject
class GameObjectLayout extends ObjLayout {
  var gameObject: GameObject = null
  width = 30
  height = 20
  def draw(t: Canvas, originx: Float, originy: Float): Unit = {}
}

trait Function

class CollisionCondition(otherClass: GameObjectLayout) extends ConditionAction
class IntegerCondition(code: Function) extends ConditionAction
class FingerMoveCondition(code: Function) extends ConditionAction
class FingerDownCondition(code: Function) extends ConditionAction
class FingerUpCondition(code: Function) extends ConditionAction
class CustomCondition(condition: Expression, code: Expression) extends ConditionAction

object ConditionActionEmpty extends ConditionAction {
  width = 30
  height = 20
}

class GraphicalRule extends ObjLayout {
  import ObjLayout._
  private var mObjectConcerned: GameObjectLayout = new GameObjectLayout()
  private var mCondition: ConditionAction = ConditionActionEmpty
  private var mCode: Expression = Expression.NONE
  var widthCondition = 0f
  var heightCondition = 0f
  var backgroundPaint = new Paint()
  
  def computeBounds() = {
    widthCondition += margin
    widthCondition += mObjectConcerned.width
    widthCondition += margin
    widthCondition += mCondition.width
    widthCondition += margin
    heightCondition = 2*margin + Math.max(mObjectConcerned.height, mCondition.height)
    backgroundPaint.setColor(colorBackground)
    mObjectConcerned.x = x + margin
    mObjectConcerned.y = y + margin
  }
  
  def setObjectConcerned(c: GameObject) = {
    
  }

  def draw(c: Canvas, originx: Float, originy: Float):Unit = {
    c.drawRect(x+originx, y+originy, x+originx+widthCondition, y+originy+heightCondition, backgroundPaint)
    mObjectConcerned.draw(c: Canvas, x+margin+originx, y+originy)
    mCondition.draw(c: Canvas, x + 3*margin + originx + mObjectConcerned.width, y + margin)
  }
}

