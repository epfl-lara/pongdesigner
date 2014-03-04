package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.objects._
import scala.collection.mutable.{Set => MSet}
import ch.epfl.lara.synthesis.kingpong.expression.Trees._

object CodeTemplates extends CodeHandler {
  /*import GameShapes._
  import Stat._
  import Stat.Subtype._*/
  import ch.epfl.lara.synthesis.kingpong.rules.Events._
  
  //private var causeEventCode: Int = 0
  private var xFrom: Float = 0
  private var xTo: Float = 0
  private var yFrom: Float = 0
  private var yTo: Float = 0
  private var dx: Float = 0
  private var dy: Float = 0
  private var shapeEvent: MSet[GameObject] = null
  private var eventNewValue: Int = 0
  private var movementIsHorizontal: Boolean = false
  private var movementIsVertical: Boolean = false
  private var shapes: Traversable[GameObject] = null
  
  private def integers: Traversable[Box[Int]] = shapes.collect{ case b: Box[_] if b.className == "Box[Int]" => b.asInstanceOf[Box[Int]] }
  private def rectangulars: Traversable[Rectangular] = shapes.collect{ case b: Rectangular => b }
  private def texts: Traversable[Box[String]] = shapes.collect{ case b: Box[_] if b.className == "Box[String]" => b.asInstanceOf[Box[String]] }
  private def booleans: Traversable[Box[Boolean]] = shapes.collect{ case b: Box[_] if b.className == "Box[Boolean]" => b.asInstanceOf[Box[Boolean]] }
  private def circles: Traversable[Circle] = shapes.collect{ case b: Circle => b }
  
  private var TOUCHMOVE_EVENT = false
  /**
   * Initializes constants for the templates
   */
  def initialize(causeEvent: Event, s: Traversable[GameObject]) = {
    TOUCHMOVE_EVENT = false
    //causeEventCode = if(causeEvent != null) causeEvent.code else -1
    causeEvent match {
      case FingerDown(v, objs) =>
        xFrom = v.x
        yFrom = v.y
        shapeEvent = objs
      case FingerUp(v, objs) =>
        xFrom = v.x
        yFrom = v.y
        shapeEvent = objs
      case FingerMove(from, to, objs) =>
        xFrom = from.x
        yFrom = from.y
        xTo = to.x
        yTo = to.y
        dx = xTo -  xFrom
        dy = yTo -  yFrom
        shapeEvent = objs
        TOUCHMOVE_EVENT = true
      case BeginContact(contact) =>
        
      case CurrentContact(contact) =>
        
      case EndContact(contact) =>
        
      case AccelerometerChanged(v) =>
        
      case GameObjectCreated(c) =>
        
      case GameObjectDeleted(d) =>
        
    }
    //eventNewValue = yTo.toInt
    movementIsHorizontal = Math.abs(dx) > 10 * Math.abs(dy)
    movementIsVertical = Math.abs(dy) > 10 * Math.abs(dx)
    shapes = s
  }
  
  /**
   * Checks if the event is of a particular type,
   * namely to access special variables such as xFrom or eventNewValue
   */
  /*def ofType(i: Int): Boolean = {
    causeEventCode == i
  }*/
  
  
  /**
   * A template converts a modification of the game to a line of code if possible
   */
  trait Template[T <: GameObject] {
    private var mShape: T = _
    def shape: T = mShape
    def shape_=(other: T) = mShape = other
    var mShapeIdent: GameObjectRef = _
    def shape_ident: GameObjectRef = mShapeIdent
    def shape_ident_=(other: GameObjectRef) = mShapeIdent = other

    protected def condition: Boolean  // Method to express the condition under which this template applies.
    protected def result: Stat  // Method to define the expression this template can return.

    /**
     * Main method to compute an expression if any that matches the template.
     */
    def resultForShape(s: T, si: GameObjectRef): Option[Stat] = {
      shape = s
      if(condition) {
        shape_ident = si
        val res = result.setPriority(priority)
        res match {
          case Block(Nil) => None
          case ParExpr(Nil) => None
          case NOP => None
          case _ => Some(res)
        }
      } else None
    }
    def comment: String
    def priority: Int
  }

  /**
   * A TemplateParallel combines multiple templates to produce a ParExpr
   */
  trait TemplateParallel[T <: GameObject] extends Template[T] {
    protected def condition: Boolean // Verified outside of this scope
    def templates: Traversable[Template[T]]
    protected def result: Stat = {
      var exprs = List[Stat]()
      for(template <- templates) {
        template.resultForShape(shape, shape_ident) match {
          case Some(expr) => exprs = expr::exprs
          case None => 
        }
      }
      exprs = exprs.reverse.sortWith{(a: Stat, b:Stat) => a.priority > b.priority}
      ParExpr(exprs)
    }
  }
  
  /**
   * A TemplateBlock combines multiple templates to produce a block of code.
   * 
   * Type T is the base type, and U is the subtype that is used when the test on the shape of type T passed.
   */
  trait TemplateBlock[U <: GameObject] extends Template[U] {
    protected def condition: Boolean
    def templates: Traversable[Template[U]]
    final protected def result: Stat = {
      var exprs = List[Stat]()
      for(template <- templates) {
        template.resultForShape(shape, shape_ident) match {
          case Some(expr) =>
            exprs = expr::exprs
          case None => 
        }
      }
      exprs = exprs.reverse
      Block(exprs)
    }
  }
  
  /**
   * A template that overcomes the problem of template contravariance.
   */
  abstract class TemplateSubtype[U <: T, T <: GameObject](template: Template[U]) extends Template[T] {
    protected def condition_subtype: Boolean
    final protected def condition: Boolean = condition_subtype
    def comment = template.comment
    def priority = template.priority
    override def resultForShape(s: T, si: GameObjectRef): Option[Stat] = {
      shape = s
      shape_ident = si
      if(condition) {
        template.resultForShape(s.asInstanceOf[U], si).map(_.setPriority(priority))
      } else None
    }
    def result: Stat = NOP // Not used because we override resultForShape
  }
  /*case class IfWidth(template: Template[Rectangular]) extends TemplateSubtype[Rectangular, GameObject](template) {
    def condition_subtype = shape.isInstanceOf[Rectangular]
  }
  case class IfHeight(template: Template[Rectangular]) extends TemplateSubtype[Rectangular, GameObject](template) {
    def condition_subtype = shape.isInstanceOf[Rectangular]
  }*/
  case class IfValue(template: Template[Box[Int]]) extends TemplateSubtype[Box[Int], GameObject](template) {
    def condition_subtype = shape.className == "Box[Int]"
  }
  case class IfText(template: Template[Box[String]]) extends TemplateSubtype[Box[String], GameObject](template) {
    def condition_subtype = shape.className == "Box[String]"
  }
  case class IfRadius(template: Template[Circle]) extends TemplateSubtype[Circle, GameObject](template) {
    def condition_subtype = shape.isInstanceOf[Circle]
  }
  
  /**
   * A TemplateOther Shapes produces the combination of a template for type T against shapes of type T
   */
  trait TemplateOther[T <: GameObject, U <: GameObject] extends Template[T] {
    private var mOtherShape: U = _
    def other_shape: U = mOtherShape
    def other_shape_=(other: U) = mOtherShape = other
    
    var mOtherShapeIdent: GameObjectRef = _
    def other_shape_ident: GameObjectRef = mOtherShapeIdent
    def other_shape_ident_=(other: GameObjectRef) = mOtherShapeIdent = other
    
    def others: ()=>Traversable[U]
    
    override def resultForShape(s: T, si: GameObjectRef): Option[Stat] = {
      shape = s
      shape_ident = si 
      val expressions = others() flatMap { o =>
        other_shape = o
        if(other_shape != shape && condition) {
          other_shape_ident = GameObjectRef(o)
          result.setPriority(priority).toList
        } else Nil
      }
      expressions match {
        case Nil => None
        case list => Some(ParExpr(list.toList))
      }
    }
    def result: Stat
  }
  
  /**
   * A TemplateOtherTwo Shapes produces the combination of a template for type T against two shapes of type U
   * The two shapes of type U are provided in order and are different.
   */
  trait TemplatePair[T <: GameObject, U <: GameObject] extends TemplateOther[T, U] {
    def order = true
    
    private var mOtherShape2: U = _
    def other_shape2: U = mOtherShape2
    def other_shape2_=(other: U) = mOtherShape2 = other
    var mOtherShape2Ident: GameObjectRef = _
    def other_shape2_ident: GameObjectRef = mOtherShape2Ident
    def other_shape2_ident_=(other: GameObjectRef) = mOtherShape2Ident = other
    
    override def resultForShape(s: T, si: GameObjectRef): Option[Stat] = {
      shape = s
      shape_ident = si
      val expressions = others() flatMap { o =>
        other_shape = o
        others() flatMap { o2 =>
          other_shape2 = o2
          if(((order && other_shape.## < other_shape2.##) || (!order && other_shape.## != other_shape2.##)) && condition) {
            other_shape2 = o2
            other_shape_ident = GameObjectRef(o)
            other_shape2_ident = GameObjectRef(o2)
            result.setPriority(priority).toList
          } else Nil
        }
      }
      expressions match {
        case Nil => None
        case list => Some(ParExpr(list.toList))
      }
    }
    def conditionForShape(s: T, o1: U, o2: U) = { shape = s; other_shape = o1; other_shape2 = o2; condition}
    def result: Stat
  }
  trait TemplateOtherShape extends TemplateOther[GameObject, GameObject] {
    def others = () => shapes
  }
  /*trait TemplateOtherRectangular extends TemplateOther[Rectangular, Rectangular] {
    def others = () => rectangulars
  }
  trait TemplateRectangularOtherShape extends TemplateOther[Rectangular, GameObject] {
    def others = () => shapes
  }*/
  trait TemplateOtherValue extends TemplateOther[Box[Int], Box[Int]] {
    def others = () => integers
  }
  trait TemplateOtherValue2 extends TemplatePair[Box[Int], Box[Int]] {
    def others = () => integers
  }
  trait TemplateOtherText extends TemplateOther[Box[String], Box[String]] {
    def others = () => texts
  }
  trait TemplateOtherShape2 extends TemplatePair[GameObject, GameObject] {
    def others = () => shapes
  }
  trait TemplateOtherText2 extends TemplatePair[Box[String], Box[String]] {
    def others = () => texts
  }
  trait TemplateTextFromInteger extends TemplatePair[Box[String], Box[Int]] {
    def others = () => integers
  }
  trait TemplateShapeOtherCircle extends TemplateOther[GameObject, Circle] {
    def others = () => circles
  }
  trait TemplateOtherCircle extends TemplateOther[Circle, Circle] {
    def others = () => circles
  }
  trait TemplateOtherCircle2 extends TemplatePair[Circle, Circle] {
    def others = () => circles
  }

  object TX_DY1 extends Template[GameObject] {
    def condition = TOUCHMOVE_EVENT && almostTheSameDiff(shape.x.next - shape.x.get, -dy) && !movementIsHorizontal
    def result    = (shape_ident("x") := shape_ident("x") + (-dy_ident))
    def priority = 0
    def comment  = s"If the finger goes upwards, ${shape.name.next} moves horizontally to the right."
  }
  object TX_DY2 extends Template[GameObject] {
    def condition = TOUCHMOVE_EVENT && almostTheSameDiff(shape.x.next - shape.x.get, dy) && !movementIsHorizontal
    def result    = (shape_ident("x") := shape_ident("x") + (dy_ident))
    def priority = 0
    def comment  = s"If the finger goes downwards, ${shape.name.next} moves horizontally to the right."
  }
  object TX_relative extends Template[GameObject] {
    def condition = true
    def result    = (shape_ident("x") := shape_ident("x") + coord(shape.x.next.toInt - shape.x.get.toInt))
    def priority = 5
    def comment  = s"Relative movement of ${shape.name.next} by (" + (shape.x.next.toInt - shape.x.get.toInt) + ", 0)"
  }
  object TX_absolute extends Template[GameObject] {
    def condition = true
    def result    = (shape_ident("x") := coord(shape.x.next.toInt))
    def priority = 6
    def comment  = s"Absolute positionning of ${shape.name.next} to x = " + shape.x.next.toInt
  }
  object TX_DX1 extends Template[GameObject] {
    def condition = TOUCHMOVE_EVENT && almostTheSameDiff(shape.x.next - shape.x.get, -dx) && !movementIsVertical
    def result    = (shape_ident("x") := shape_ident("x") + (-dx_ident))
    def priority = 3
    def comment  = s"If the finger goes to the left, ${shape.name.next} moves horizontally to the right."
  }
  object TX_DX2 extends Template[GameObject] {
    def condition = TOUCHMOVE_EVENT && almostTheSameDiff(shape.x.next - shape.x.get, dx) && !movementIsVertical
    def result    = (shape_ident("x") := shape_ident("x") + (dx_ident))
    def priority = 15
    def comment  = shape.name.next + " moves horizontally in the same direction as the finger."
  }
  object TX_AlignLeft1 extends TemplateOtherShape {
    def condition = almostTheSame(shape.x.next, other_shape.x.get, 20)
    def result    = (shape_ident("x") := other_shape_ident("x"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its x side to the x side of ${other_shape.name.next}"
  }
  /*object TX_AlignLeft2 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.x.next, other_shape.prev_center_x, 20)  && other_shape.prev_center_x != other_shape.prev_x
    def result    = (shape_ident("x") := other_shape_ident("center_x"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its x side to the center side of ${other_shape.name.next}"
  }
  object TX_AlignLeft3 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.x.next, other_shape.x.get + other_shape.width.get, 20)
    def result    = (shape_ident("x") := other_shape_ident("x") + other_shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its x side to the right side of ${other_shape.name.next}"
  }
  object TX_AlignRight1 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.x.next + shape.width.get, other_shape.x.get + other_shape.width.get, 20)
    def result    = (shape_ident("x") := other_shape_ident("x") + (other_shape_ident("width") - shape_ident("width")))
    def priority = 10
    def comment  = shape.name.next + s" aligns its right side to the right side of ${other_shape.name.next}"
  }
  object TX_AlignRight2 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.x.next + shape.width.get, other_shape.prev_center_x, 20) && other_shape.prev_center_x != other_shape.prev_x
    def result    = (shape_ident("x") := other_shape_ident("center_x") - shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its right side to the center of ${other_shape.name.next}"
  }
  object TX_AlignRight3 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.x.next + shape.width.get, other_shape.x.get, 20)
    def result    = (shape_ident("x") := other_shape_ident("x") - shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its right side to the left side of ${other_shape.name.next}"
  }
  object TX_AlignCenter1 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.center_x, other_shape.x.get + other_shape.width.get, 20) && shape.center_x != shape.x.next
    def result    = (shape_ident.center_x = other_shape_ident("x") + other_shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its center to the right side of ${other_shape.name.next}"
  }*/
  /*object TX_AlignCenter2 extends TemplateOtherShape {
    def condition = almostTheSame(shape.center_x, other_shape.prev_center_x, 20) && shape.center_x != shape.x.next && other_shape.prev_center_x != other_shape.prev_x
    def result    = (shape_ident.center_x = other_shape_ident.center_x)
    def priority = 10
    def comment  = shape.name.next + s" aligns its center to the center of ${other_shape.name.next}"
  }*/
  /*object TX_AlignCenter3 extends TemplateOtherShape {
    def condition = almostTheSame(shape.center_x, other_shape.x.get, 20) && shape.center_x != shape.x.next
    def result    = (shape_ident.center_x = other_shape_ident("x"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its center to the left of ${other_shape.name.next}"
  }*/
  
  object TX extends TemplateParallel[GameObject] {
    def condition = shape.x.get != shape.x.next
    val templates: Traversable[Template[GameObject]] = List(
      TX_DY1,
      TX_DY2,
      TX_relative,
      TX_absolute,
      TX_DX1,
      TX_DX2,
      TX_AlignLeft1
      //IfWidth(TX_AlignLeft2),
      //IfWidth(TX_AlignLeft3),
      //IfWidth(TX_AlignCenter1),
      //TX_AlignCenter2,
      //TX_AlignCenter3,
      //IfWidth(TX_AlignRight1),
      //IfWidth(TX_AlignRight2),
      //IfWidth(TX_AlignRight3)
      )
    def priority = 10
    def comment   = s"Possible x changes for ${shape.name.next}"
  }

  object TY_DX1 extends Template[GameObject] {
    def condition = TOUCHMOVE_EVENT && almostTheSameDiff(shape.y.next - shape.y.get, -dx) && !movementIsVertical
    def result    = (shape_ident("y") := shape_ident("y") + (-dx_ident))
    def priority = 0
    def comment  = s"If the finger goes to the left, ${shape.name.next} moves vertically to the bottom."
  }
  object TY_DX2 extends Template[GameObject] {
    def condition = TOUCHMOVE_EVENT && almostTheSameDiff(shape.y.next - shape.y.get, dx) && !movementIsVertical
    def result    = (shape_ident("y") := shape_ident("y") + (dx_ident))
    def priority = 0
    def comment  = s"If the finger goes to the left, ${shape.name.next} moves vertically to the top."
  }
  object TY_relative extends Template[GameObject] {
    def condition = true
    def result    = (shape_ident("y") := shape_ident("y") + coord(shape.y.next.toInt - shape.y.get.toInt))
    def priority = 5
    def comment  = s"Relative movement of ${shape.name.next} by (0, " + (shape.y.next.toInt - shape.y.get.toInt) + ")"
  }
  object TY_absolute extends Template[GameObject] {
    def condition = true
    def result    = (shape_ident("y") := coord(shape.y.next.toInt))
    def priority = 6
    def comment  = s"Absolute positionning of ${shape.name.next} to y = " + shape.y.next.toInt
  }
  object TY_DY1 extends Template[GameObject] {
    def condition = TOUCHMOVE_EVENT && almostTheSameDiff(shape.y.next - shape.y.get, -dy) && !movementIsHorizontal
    def result    = (shape_ident("y") := shape_ident("y") + (-dy_ident))
    def priority = 3
    def comment  = s"If the finger goes to the bottom, ${shape.name.next} moves vertically to the top."
  }
  object TY_DY2 extends Template[GameObject] {
    def condition = TOUCHMOVE_EVENT && almostTheSameDiff(shape.y.next - shape.y.get, dy) && !movementIsHorizontal
    def result    = (shape_ident("y") := shape_ident("y") + (dy_ident))
    def priority = 15
    def comment  = shape.name.next + " moves vertically in the same direction as the finger"
  }
  object TY_AlignLeft1 extends TemplateOtherShape {
    def condition = almostTheSame(shape.y.next, other_shape.y.get, 20)
    def result    = (shape_ident("y") := other_shape_ident("y"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its y side to the y side of ${other_shape.name.next}"
  }
  /*object TY_AlignLeft2 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.y.next, other_shape.prev_center_y, 20)  && other_shape.prev_center_y != other_shape.prev_y
    def result    = (shape_ident("y") := other_shape_ident("center_y"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its y side to the center side of ${other_shape.name.next}"
  }
  object TY_AlignLeft3 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.y.next, other_shape.y.get + other_shape.width.get, 20)
    def result    = (shape_ident("y") := other_shape_ident("y") + other_shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its y side to the bottom side of ${other_shape.name.next}"
  }
  object TY_AlignRight1 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.y.next + shape.width.get, other_shape.y.get + other_shape.width.get, 20)
    def result    = (shape_ident("y") := other_shape_ident("y") + (other_shape_ident("width") - shape_ident("width")))
    def priority = 10
    def comment  = shape.name.next + s" aligns its bottom side to the bottom side of ${other_shape.name.next}"
  }
  object TY_AlignRight2 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.y.next + shape.width.get, other_shape.prev_center_y, 20) && other_shape.prev_center_y != other_shape.prev_y
    def result    = (shape_ident("y") := other_shape_ident("center_y") - shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its bottom side to the center of ${other_shape.name.next}"
  }
  object TY_AlignRight3 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.y.next + shape.width.get, other_shape.y.get, 20)
    def result    = (shape_ident("y") := other_shape_ident("y") - shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its bottom side to the top side of ${other_shape.name.next}"
  }
  object TY_AlignCenter1 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.center_y, other_shape.y.get + other_shape.width.get, 20) && shape.center_y != shape.y.next
    def result    = (shape_ident.center_y = other_shape_ident("y") + other_shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its center to the bottom side of ${other_shape.name.next}"
  }
  object TY_AlignCenter2 extends TemplateOtherShape {
    def condition = almostTheSame(shape.center_y, other_shape.prev_center_y, 20) && shape.center_y != shape.y.next && other_shape.prev_center_y != other_shape.prev_y
    def result    = (shape_ident.center_y = other_shape_ident.center_y)
    def priority = 10
    def comment  = shape.name.next + s" aligns its center to the center of ${other_shape.name.next}"
  }
  object TY_AlignCenter3 extends TemplateOtherShape {
    def condition = almostTheSame(shape.center_y, other_shape.y.get, 20) && shape.center_y != shape.y.next
    def result    = (shape_ident.center_y = other_shape_ident("y"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its center to the top of ${other_shape.name.next}"
  }*/
  object TY extends TemplateParallel[GameObject] {
    def condition = shape.y.get != shape.y.next
    val templates: Traversable[Template[GameObject]] = List(
      TY_DX1,
      TY_DX2,
      TY_relative,
      TY_absolute,
      TY_DY1,
      TY_DY2,
      TY_AlignLeft1/*,
      IfHeight(TY_AlignLeft2),
      IfHeight(TY_AlignLeft3),
      IfHeight(TY_AlignCenter1),
      TY_AlignCenter2,
      TY_AlignCenter3,
      IfHeight(TY_AlignRight1),
      IfHeight(TY_AlignRight2),
      IfHeight(TY_AlignRight3)*/)
    def priority = 10
    def comment   = s"Possible y changes for ${shape.name.next}"
  }
  
  object TXY_Independent extends TemplateBlock[GameObject] {
    def condition = shape.x.get != shape.x.next || shape.y.get != shape.y.next
    val templates: Traversable[Template[GameObject]] = List(
        TX,
        TY)
    def priority = 10
    def comment = s"Independent x and y changes for ${shape.name.next}"
  }
  
  // TODO : Create meta-templates to capture the diversity of alignments between shapes.
  /*object TXY_CenterMirror extends TemplateOtherShape2 {
    override def order = false
    def condition = almostTheSame(shape.center_x - other_shape.center_x, other_shape.center_x - other_shape2.center_x, 40) &&
                    almostTheSame(shape.center_y - other_shape.center_y, other_shape.center_y - other_shape2.center_y, 40)
    def result = Block( List(
        shape_ident.center_x = other_shape_ident.center_x - other_shape2_ident.center_x + other_shape_ident.center_x,
        shape_ident.center_y = other_shape_ident.center_y - other_shape2_ident.center_y + other_shape_ident.center_y
    ))
    def priority = 11
    def comment = s"The position of ${shape.name.next} is the mirror of ${other_shape2.name} relative to ${other_shape.name.next}"
  }*/
  
  object TXY extends TemplateParallel[GameObject] {
    def condition = shape.x.get != shape.x.next || shape.y.get != shape.y.next
    val templates: Traversable[Template[GameObject]] = List(
        //TXY_CenterMirror,
        TXY_Independent
    )
    def priority = 10
    def comment = s"All x and y changes for ${shape.name.next}"
  }
  
  object TAngleRelative extends Template[GameObject] {
    def condition = true
    var shiftAngle: Float = 0
    def result    = { shiftAngle = Math.round((shape.angle.next - shape.angle.get)/15)*15; (shape_ident("angle") := shape_ident("angle") - angle(shiftAngle)) }
    def priority = 8
    def comment   = s"Change the speed direction of ${shape.name.next} by " + shiftAngle + "°"
  }
  object TAngleAbsolute extends Template[GameObject] {
    def condition = true
    var roundedAngle: Float = 0
    def result    = { roundedAngle = Math.round(shape.angle.next/15)*15; (shape_ident("angle") := angle(roundedAngle)) }
    def priority = 9
    def comment   = s"Change the speed direction of ${shape.name.next} to " + roundedAngle + "°"
  }
  /*object TAngleOnCircle extends TemplateShapeOtherCircle {
    def condition = TOUCHMOVE_EVENT && Math.abs(Game.angle(other_shape.x.next, other_shape.y.next, xTo, yTo) - shape.angle.next) < 20
    // TODO : Verify that such event is fired when finger down.
    def result    = shape_ident("angle") := Stat.angle(other_shape_ident.x, other_shape_ident.y, x_ident, y_ident)
    def priority = 10
    def comment   = s"Change the speed direction of ${shape.name.next} to equal the direction between the center of ${other_shape.name.next} and the finger touch"
  }*/
  object TAngleCopy extends TemplateOtherShape {
    def condition = almostTheSame(shape.angle.next, other_shape.angle.get, 15)
    def result    = shape_ident("angle") := other_shape_ident("angle")
    def priority = 10
    def comment   = s"Copy the speed direction of " +  other_shape.name.next + s" to ${shape.name.next}"
  }
  
  object TAngle extends TemplateParallel[GameObject] {
    def condition = shape.angle.get != shape.angle.next && Math.abs(shape.angle.next - shape.angle.get) > 10 && Math.abs(shape.angle.next - shape.angle.get) < 350
    val templates = List(
      TAngleRelative,
      TAngleAbsolute,
      //TAngleOnCircle,
      TAngleCopy)
    def priority = 10
    def comment = s"Possible direction changes for ${shape.name.next}"
  }
  
  object TVelocityAbsolute extends Template[PhysicalObject] {
    def condition = true
    def result    = (shape_ident("velocity") := speed(shape.velocity.next))
    def priority = 10
    def comment = s"Velocity of ${shape.name.next} is set absolutely to " + shape.velocity.next
  }
  /*object TVelocityRelative extends Template[PhysicalObject] {
    def condition = shape.velocity.get.x != 0 && shape.velocity.next != 0
    def result    = (shape_ident("velocity") := shape_ident("velocity") * factor(shape.velocity.next / shape.velocity.get))
    def priority = 8
    def comment = s"Velocity of ${shape.name.next} is multiplied by " + (shape.velocity.next / shape.velocity.get)
  }*/
  /*object TVelocityCopy extends TemplateOtherShapes {
    def condition = true
    def resultForShape(shape_ident: GameObjectRef, other_shape: GameObject) = 
      if()
  }*/
  
  /*object TVelocity extends TemplateParallel[PhysicalObject] {
    def condition = shape.velocity.get != shape.velocity.next && (Math.round(Math.abs(shape.angle.next - shape.angle.get)/15)*15 == 0 || shape.velocity.get == 0 || shape.velocity.next == 0 || shape.velocity.next / shape.velocity.get > 2 || shape.velocity.get / shape.velocity.next > 2)
    val templates: Traversable[Template[PhysicalObject]] = List(TVelocityAbsolute)
    def priority = 10
    def comment = s"Possible velocity changes for ${shape.name.next}"
  }*/

  object TColorAbsolute extends Template[GameObject] {
    def condition = true
    def result    = (shape_ident("color") := color(shape.color.next))
    def priority = 10
    def comment = s"The color of ${shape.name.next} is set to " + shape.color.next
  }
  object TColor extends TemplateParallel[GameObject] {
    def condition = shape.color.get != shape.color.next
    val templates = List(TColorAbsolute)
    def priority = 10
    def comment = s"Possible color changes for ${shape.name.next}"
  }
  
  object TVisibleAbsolute extends Template[GameObject] {
    def condition = true
    def result    = (shape_ident("visible") := BooleanLiteral(shape.visible.next))
    def priority = 10
    def comment = s"The visibility of ${shape.name.next} is set to " + shape.visible.next
  }
  object TVisibleToggle extends Template[GameObject] {
    def condition = true
    def result    = (shape_ident("visible") := !(shape_ident("visible")))
    def priority = 10
    def comment = s"The visibility of ${shape.name.next} is set to " + shape.color.next
  }
  object TVisible extends TemplateParallel[GameObject] {
    def condition = shape.visible.get != shape.visible.next
    val templates = List(TVisibleAbsolute)
    def priority = 10
    def comment = s"Possible visibility changes for ${shape.name.next}"
  }
  
  /*object TWidthRelative extends Template[Rectangular] {
    def condition = true
    def result = (shape_ident("width") := shape_ident("width") + coord(shape.width.next - shape.width.get))
    def priority = 10
    def comment = s"The width of ${shape.name.next} is set relatively to its previous value"
  }*/
  
  /*object TWidthFactor extends Template[Rectangular] {
    def condition = true
    def result = (shape_ident("width") := shape_ident("width") * factor(shape.width.toFloat / shape.width.get))
    def priority = 11
    def comment = s"The width of ${shape.name.next} is multiplied by a factor"
  }
  
  object TWidthAbsolute extends Template[Rectangular] {
    def condition = true
    def result = (shape_ident("width") := coord(shape.width.next))
    def priority = 12
    def comment = s"The width of ${shape.name.next} is set absolutely"
  }
  
  object TWidthMove extends Template[Rectangular] {
    def condition = TOUCHMOVE_EVENT && almostTheSameDiff(shape.width.next - shape.width.get, dx) && !movementIsVertical
    def result = (shape_ident("width") := shape_ident.width + (dx_ident))
    def priority = 13
    def comment = s"The width of ${shape.name.next} increases with the finger going to the right "
  }*/
  // TODO : More width templates based on expressions (like sum or diff of widths)
  
  /*object TWidth extends TemplateParallel[Rectangular] {
    def condition = shape.width.get != shape.width.next
    val templates = List(
      TWidthRelative,
      TWidthFactor,
      TWidthAbsolute,
      TWidthMove)
    def priority = 10
    def comment = s"Possible width changes for ${shape.name.next}"
  }*/
  
  /*object THeightRelative extends Template[Rectangular] {
    def condition = true
    def result = (shape_ident("height") := shape_ident("height") + coord(shape.height.next - shape.height.get))
    def priority = 10
    def comment = s"The height of ${shape.name.next} is set relatively to its previous value"
  }
  
  object THeightFactor extends Template[Rectangular] {
    def condition = true
    def result = (shape_ident("height") := shape_ident("height") * factor(shape.height.toFloat / shape.height.get))
    def priority = 11
    def comment = s"The height of ${shape.name.next} is multiplied by a factor"
  }
  
  object THeightAbsolute extends Template[Rectangular] {
    def condition = true
    def result = (shape_ident("height") := coord(shape.height.next))
    def priority = 12
    def comment = s"The height of ${shape.name.next} is set absolutely"
  }
  
  object THeightMove extends Template[Rectangular] {
    def condition = TOUCHMOVE_EVENT && almostTheSameDiff(shape.height.next - shape.height.get, dy) && !movementIsHorizontal
    def result = (shape_ident("height") := shape_ident.height + (dy_ident))
    def priority = 13
    def comment = s"The height of ${shape.name.next} increases with the finger going to the bottom "
  }*/
  // TODO : More height templates based on expressions (like sum or diff of heights)
  
  /*object THeight extends TemplateParallel[Rectangular] {
    def condition = shape.height.get != shape.height.next
    def templates = List(
      THeightRelative,
      THeightFactor,
      THeightAbsolute,
      THeightMove
    )
    def priority = 10
    def comment = s"Possible height changes for ${shape.name.next}"
  }*/
  /*class T[A <: GameObject](c: A => Boolean) {
    def |-(exp: (A, GameObjectRef) => Stat): Template[A] = new Template[A] {
      def condition = c(shape)
      def result    = exp(shape, GameObjectRef(shape))
    }
  }
  implicit def gg[A <: GameObject](g: A => Boolean): T[A] = new T(g)
  
  val TemplateIntegerBffoxMore1 = ((
         (shape: Box[Int]) => Math.abs(shape.value.next - shape.value.get) > 1)
    |- ((shape, shape_ident) => (shape_ident("value") := shape_ident("value") + number(shape.value.next - shape.value.get)))
  )*/
  
  object TValueAbsolute extends Template[Box[Int]] {
    def condition = true
    def result    = (shape_ident("value") := number(shape.value.next))
    def priority  = 5
    def comment   = s"Always change the value  of ${shape.name.next} to " + shape.value.next
  }
  object TValueRelative extends Template[Box[Int]] {
    def condition = Math.abs(shape.value.next - shape.value.get) > 1
    def result    = (shape_ident("value") := shape_ident("value") + number(shape.value.next - shape.value.get))
    def priority  = 6
    def comment   = s"Change the value of ${shape.name.next} by " + shape.value.next
  }
  object TValueDiv2 extends Template[Box[Int]] {
    def condition = shape.value.next == shape.value.get / 2
    def result    = (shape_ident("value") := shape_ident("value") / number(2))
    def priority  = 7
    def comment   = s"Divides the value of ${shape.name.next} by 2"
  }
  object TValueTimes extends Template[Box[Int]] {
    def condition = shape.value.get != 0 && shape.value.next % shape.value.get == 0 && shape.value.next / shape.value.get != 0
    def result    = (shape_ident("value") := (shape_ident("value") * number(shape.value.next / shape.value.get)))
    def priority  = 8
    def comment   = s"Multiplies the value of ${shape.name.next} by " + (shape.value.next / shape.value.get)
  }
  object TValueCombine1_absolute extends TemplateOtherValue {
    def condition = shape.value.next == other_shape.value.get
    def result    = (shape_ident("value") := other_shape_ident("value"))
    def priority  = if(other_shape.value.get == 0) 5 else 10
    def comment   = s"Copies the value of ${other_shape.name.next} to ${shape.name.next}"
  }
  object TValueCombine1_relativeMultCopy extends TemplateOtherValue {
    def condition = other_shape.value.get != 0 && shape.value.next % other_shape.value.get == 0 && shape.value.next !=  other_shape.value.get && shape.value.next != 0
    def result    = (shape_ident("value") := (other_shape_ident("value") * number(shape.value.next / other_shape.value.get)))
    def priority  = if(shape.value.next / other_shape.value.get == 2) 8 else 10
    def comment   = s"Stores in ${shape.name.next} the value of ${other_shape.name.next} multiplied by" + (shape.value.next / other_shape.value.get)
  }
  object TValueCombine1_relativeModulo extends TemplateOtherValue {
    def condition = other_shape.value.get > 1 && shape.value.next == shape.value.get % other_shape.value.get
    def result    = (shape_ident("value") := (shape_ident("value") % other_shape_ident("value")))
    def priority  = 10
    def comment   = s"Stores in ${shape.name.next} its previous value modulo ${other_shape.name.next}"
  }
  object TValueCombine2_plus extends TemplateOtherValue2 {
    def condition = shape.value.next == other_shape.value.get + other_shape2.value.get && other_shape != other_shape2
    def result = (shape_ident("value") := other_shape_ident("value") + other_shape2_ident("value"))
    def priority = if(other_shape.value.get == 0 || other_shape2.value.get == 0) 0 else 10
    def comment   = s"Stores in ${shape.name.next} the sum of ${other_shape.name.next} and ${other_shape2.name}"
  }
  object TValueCombine2_minus extends TemplateOtherValue2 {
    override def order = false
    def condition = shape.value.next == other_shape.value.get - other_shape2.value.get
    def result = (shape_ident("value") := other_shape_ident("value") - other_shape2_ident("value"))
    def priority = if(other_shape.value.get == 0 || other_shape2.value.get == 0) 0 else if(shape.value.next == 0) 1 else 10
    def comment   = s"Stores in ${shape.name.next} the difference between ${other_shape.name.next} and ${other_shape2.name}"
  }
  object TValueCombine2_times extends TemplateOtherValue2 {
    def condition = shape.value.next == other_shape.value.get * other_shape2.value.get
    def result = (shape_ident("value") := other_shape_ident("value") * other_shape2_ident("value"))
    def priority = if(other_shape.value.get == 0 || other_shape2.value.get == 0) 0 else if(other_shape.value.get == 1 || other_shape2.value.get == 1) 2 else 10
    def comment   = s"Stores in ${shape.name.next} the multiplication between ${other_shape.name.next} and ${other_shape2.name}"
  }
  object TValueCombine2_div extends TemplateOtherValue2 {
    override def order = false
    def condition = other_shape2.value.get != 0 && shape.value.next == other_shape.value.get / other_shape2.value.get
    def result = (shape_ident("value") := other_shape_ident("value") / other_shape2_ident("value"))
    def priority = if(other_shape2.value.get == 0) 0 else if(other_shape.value.get % other_shape2.value.get != 0) 2 else 10
    def comment   = s"Stores in ${shape.name.next} the division between ${other_shape.name.next} and ${other_shape2.name}"
  }
  object TValueRelative2 extends Template[Box[Int]] {
    def condition = Math.abs(shape.value.next - shape.value.get) == 1
    def result    = (shape_ident("value") := shape_ident("value") + number(shape.value.next - shape.value.get))
    def priority = 12
    def comment   = s"Adds to ${shape.name.next} the number ${shape.value.next - shape.value.get}"
  }

  object TValue extends TemplateParallel[Box[Int]] {
    def condition = shape.value.get != shape.value.next
    val templates = List(TValueAbsolute,
      TValueRelative,
      TValueDiv2,
      TValueTimes,
      TValueCombine1_absolute,
      TValueCombine1_relativeMultCopy,
      TValueCombine1_relativeModulo,
      TValueCombine2_plus,
      TValueCombine2_minus,
      TValueCombine2_times,
      TValueCombine2_div,
      TValueRelative2
    )
    def priority = 10
    def comment   = s"Possible value changes for ${shape.name.next}"
  }
  object TTextCopy extends TemplateOtherText {
    def condition = shape.value.next == other_shape.value.get 
    def result    = (shape_ident("value") := other_shape_ident("value"))
    def priority  = if(other_shape.value.get != "") 10 else 0
    def comment   = s"Copy the text of ${other_shape.name.next} to ${shape.name.next}"
  }
  object TTextConcatenate extends TemplateOtherText2 {
    def condition = shape.value.next == other_shape.value.get + other_shape2.value.get
    def result    = (shape_ident("value") := other_shape_ident("value") + other_shape2_ident("value"))
    def priority  = if(other_shape.value.get == "" || other_shape2.value.get == "") 0 else 10
    def comment   = s"Concatenate the texts of ${other_shape.name.next} and ${other_shape2.name} to ${shape.name.next}"
  }
  object TTextConcatenate2 extends TemplateOtherText2 {
    def condition = shape.value.next == other_shape2.value.get + other_shape.value.get
    def result    = (shape_ident("value") := other_shape2_ident("value") + other_shape_ident("value"))
    def priority  = if(other_shape.value.get == "" || other_shape2.value.get == "") 0 else 10
    def comment   = s"Concatenate the texts of ${other_shape2.name} and ${other_shape.name.next} to ${shape.name.next}"
  }
  // TODO: convert integers to text boxes if detected.
  
  object TText extends TemplateParallel[Box[String]] {
    def condition = shape.value.get != shape.value.next
    val templates = List(
        TTextCopy,
        TTextConcatenate,
        TTextConcatenate2
    )
    def priority = 10
    def comment   = s"Possible text changes for ${shape.name.next}"
  }
  
  object TRadiusRelativePlus extends TemplateOtherCircle {
    def condition = true
    def result    = (shape_ident("radius") := shape_ident("radius") + coord(shape.radius.next - shape.radius.get))
    def priority  = 10
    def comment   = s"Add a constant to the radius of ${shape.name.next}"
  }
  object TRadiusRelativeTimes extends TemplateOtherCircle {
    def condition = shape.radius.get != 0
    def result    = (shape_ident("radius") := shape_ident("radius") * coord(shape.radius.next / shape.radius.get))
    def priority  = if(shape.radius.next / shape.radius.get < 1) 8 else 0
    def comment   = s"Multiply the radius of ${shape.name.next} by a factor"
  }
  object TRadiusAbsolute extends TemplateOtherCircle {
    def condition = true
    def result    = (shape_ident("radius") := coord(shape.radius.next))
    def priority  = 10
    def comment   = s"Change the radius of ${shape.name.next} absolutely"
  }
  object TRadiusSwitch extends TemplateOtherCircle {
    def condition = true
    def result    = (shape_ident("radius") := (coord(shape.radius.get + shape.radius.next) - shape_ident("radius")))
    def priority  = 6
    def comment   = s"Add a constant to the radius of ${shape.name.next}"
  }
  object TRadiusMoveX extends Template[Circle] {
    def condition = TOUCHMOVE_EVENT && almostTheSameDiff(shape.radius.next - shape.radius.get, dx) && !movementIsVertical
    def result    = (shape_ident("radius") := shape_ident("radius") + (dx_ident))
    def priority  = 10
    def comment   = s"Augment the radius of ${shape.name.next} when the finger moves to the right"
  }
  object TRadiusMoveX_rev extends Template[Circle] {
    def condition = TOUCHMOVE_EVENT && almostTheSameDiff(shape.radius.next - shape.radius.get, -dx) && !movementIsVertical
    def result    = (shape_ident("radius") := shape_ident("radius") + (-dx_ident))
    def priority  = 8
    def comment   = s"Augment the radius of ${shape.name.next} when the finger moves to the left"
  }
  object TRadiusMoveY extends Template[Circle] {
    def condition = TOUCHMOVE_EVENT && almostTheSameDiff(shape.radius.next - shape.radius.get, dy) && !movementIsHorizontal
    def result    = (shape_ident("radius") := shape_ident("radius") + (dy_ident))
    def priority  = 10
    def comment   = s"Augment the radius of ${shape.name.next} when the finger moves to the bottom"
  }
  object TRadiusMoveY_rev extends Template[Circle] {
    def condition = TOUCHMOVE_EVENT && almostTheSameDiff(shape.radius.next - shape.radius.get, -dy) && !movementIsHorizontal
    def result    = (shape_ident("radius") := shape_ident("radius") + (-dy_ident))
    def priority  = 8
    def comment   = s"Augment the radius of ${shape.name.next} when the finger moves to the top"
  }
  
  object TRadius extends TemplateParallel[Circle] {
    def condition = shape.radius.get != shape.radius.next
    val templates = List(
        TRadiusRelativePlus, 
        TRadiusRelativeTimes,
        TRadiusAbsolute,
        TRadiusSwitch,
        TRadiusMoveX,
        TRadiusMoveX_rev,
        TRadiusMoveY,
        TRadiusMoveY_rev
    )
    def priority = 10
    def comment   = s"Possible radius changes for ${shape.name.next}"
  }
  
  object TShape extends TemplateBlock[GameObject] {
    def condition = true
    val templates: Traversable[Template[GameObject]] = List(
        TXY,
        TAngle,
        //TVelocity,
        TColor,
        TVisible,
        //IfWidth(TWidth),
        //IfHeight(THeight),
        IfValue(TValue),
        IfText(TText),
        IfRadius(TRadius)
    )
    def priority = 10
    def comment   = s"The changes for shape ${shape.name.next}"
  }
  
  def recover(): List[Stat] = {
    var allcode: List[Stat] = Nil
    shapes.foreach({ shape =>
      TShape.resultForShape(shape, GameObjectRef(shape)) match {
        case Some(Block(Nil)) =>
        case Some(code) => allcode = code :: allcode
        case _ =>
      }
    })
    Stat.recursiveFlattenBlock(allcode.reverse)
  }
}