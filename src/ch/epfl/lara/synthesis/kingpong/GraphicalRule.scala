package ch.epfl.lara.synthesis.kingpong

import android.graphics.Canvas
import android.graphics.Paint
import android.content.Context
import ch.epfl.lara.synthesis.kingpong.ast._

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
  var xmin: Float = 0
  var ymin: Float = 0
  var xmax: Float = 0
  var ymax: Float = 0
  //def draw(t: Canvas, parent_x: Float, parent_y: Float): Unit
}

/**
 * Uses to update a layout when it is drawn.
 */
class Layout extends ObjLayout {
  var xmin1: Float = 0
  var ymin1: Float = 0
  var xmax1: Float = 0
  var ymax1: Float = 0
  def set(x1: Float, y1: Float, x2: Float, y2: Float) = {
    xmin1 = x1
    ymin1 = y1
    xmax1 = x2
    ymax1 = y2
  }
  def augment(x1: Float, y1: Float): Unit = {
    xmin1 = Math.min(x1, xmin1)
    ymin1 = Math.min(y1, ymin1)
    xmax1 = Math.max(x1, xmax1)
    ymax1 = Math.max(y1, ymax1)
  }
  def augment(x1: Float, y1: Float, x2: Float, y2: Float): Unit = {
    augment(x1, y1)
    augment(x2, y2)
  }
  def push(): Unit = {
    this.synchronized {
      xmin = xmin1
      ymin = ymin1
      xmax = xmax1
      ymax = ymax1
    }
  }
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
}

/**
 * The dynamic an graphical representation of a rule AST
 */
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
    //widthCondition += mObjectConcerned.width
    widthCondition += margin
    //widthCondition += mCondition.width
    widthCondition += margin
    //heightCondition = 2*margin + Math.max(mObjectConcerned.height, mCondition.height)
    backgroundPaint.setColor(colorBackground)
    //mObjectConcerned.x = x + margin
    //mObjectConcerned.y = y + margin
  }
  
  def setObjectConcerned(c: GameObject) = {
    
  }

  def draw(c: Canvas, originx: Float, originy: Float):Unit = {
    //c.drawRect(x+originx, y+originy, x+originx+widthCondition, y+originy+heightCondition, backgroundPaint)
    //mObjectConcerned.draw(c: Canvas, x+margin+originx, y+originy)
    //mCondition.draw(c: Canvas, x + 3*margin + originx + mObjectConcerned.width, y + margin)
  }
}

