package ch.epfl.lara.synthesis.kingpong.expression

import scala.collection.mutable.{Set => MSet}

import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.expression.TreeOps._
import ch.epfl.lara.synthesis.kingpong.rules.Events._

object CodeTemplates extends CodeHandler {
  
  class TemplateContext(val events: Seq[(Event, Int)], val objects: Traversable[GameObject]) {
    val eventMove = (events.flatMap { case (e@FingerMove(from, to, objs), i) => List(e) case _=> Nil}).headOption
    val (dx, dy, eventObjects) = eventMove match {
      /*case Some(FingerDown(v, objs)) => (0, 0, objs)
      case Some(FingerUp(v, objs))=> (0, 0, objs)*/
      case Some(FingerMove(from, to, objs)) => (0, 0, objs)
      case _ => (0, 0, Set.empty[GameObject])
    }
    val isMovementHorizontal = Math.abs(dx) > 10 * Math.abs(dy)
    val isMovementVertical = Math.abs(dy) > 10 * Math.abs(dx)
    val isTouchMoveEvent = eventMove.nonEmpty
    
    lazy val integers: Traversable[Box[Int]] = objects.collect{ case b: IntBox => b}
    lazy val texts: Traversable[Box[String]] = objects.collect{ case b: StringBox => b}
    lazy val booleans: Traversable[Box[Boolean]] = objects.collect{ case b: BooleanBox => b }
    lazy val rectangulars: Traversable[Rectangular] = objects.collect{ case b: Rectangular => b }
    lazy val circles: Traversable[Circular] = objects.collect{ case b: Circular => b }
    lazy val positionables: Traversable[Positionable] = objects.collect{ case b: Positionable => b }
    lazy val rotationables: Traversable[Rotationable] = objects.collect{ case b: Rotationable => b }
    lazy val cells: Traversable[Cell] = objects.collect{ case c: Cell => c }
  }
  
  def inferStatements(events: Seq[(Event, Int)], objects: Traversable[GameObject]): Seq[Expr] = {
    implicit val ctx = new TemplateContext(events, objects)
    val exprs = objects.flatMap(TShape.applyForObject).toList
    flattenNOP(exprs.map(flatten))
  }
  
  /**
   * A template converts a modification of the game to a line of code if possible
   */
  trait Template[T <: GameObject] {
    
    /**
     * Compute an expression if the given object applies to this template.
     */
    def applyForObject(obj: GameObject)(implicit ctx: TemplateContext): Option[Expr] = {
      if (typeCondition(obj)) 
        apply(obj.asInstanceOf[T])
      else 
        None 
    }
    
    /**
     * Compute an expression if the given object applies to this template.
     */
    def apply(obj: T)(implicit ctx: TemplateContext): Option[Expr]
    
    protected def typeCondition(obj: GameObject): Boolean
  }
  
  trait TemplateSimple[T <: GameObject] extends Template[T] {
    def comment(obj: T)(implicit ctx: TemplateContext): String
    
    /** 
     * The statement this template can return.
     * Return `None` if this template doesn't appy to the given object. 
     * Also set the priority on the expression, if any.
     */
    def result(obj: T)(implicit ctx: TemplateContext): Option[Expr]

    def apply(obj: T)(implicit ctx: TemplateContext): Option[Expr] = {
      result(obj)
    }
  }
  
  trait TemplateOther[T <: GameObject, U] extends Template[T] {
    def comment(obj: T, other: U)(implicit ctx: TemplateContext): String
    
    /** 
     * The statement this template can return.
     * Return `None` if this template doesn't appy to the given objects. 
     * Also set the priority on the expression, if any.
     */
    def result(obj: T, other: U)(implicit ctx: TemplateContext): Option[Expr]

    def others(implicit ctx: TemplateContext): Traversable[U]
    
    def apply(obj: T)(implicit ctx: TemplateContext): Option[Expr] = {
      val stats = others flatMap { other =>
        if (obj != other) {
          result(obj, other)
        } else {
          None
        }
      }
      
      if (stats.size == 0)
        None
      else 
        Some(ParExpr(stats.toList))
    }
  }
  
  /**
   * A TemplateOtherPair produces the combination of a template for type T against two objects of type U
   * The two shapes of type U are provided in order and are different.
   */
  trait TemplateOtherPair[T <: GameObject, U] extends Template[T] {
    def comment(obj: T, other1: U, other2: U)(implicit ctx: TemplateContext): String
    
    /** The condition under which this template applies. */
    def condition(obj: T, other1: U, other2: U)(implicit ctx: TemplateContext): Boolean
    
    /** The statement this template can return. */
    def result(obj: T, other1: U, other2: U)(implicit ctx: TemplateContext): Expr

    /** The priority of this template compare to others, given a game object. */
    def priority(obj: T, other1: U, other2: U)(implicit ctx: TemplateContext): Int
    
    def others(implicit ctx: TemplateContext): Traversable[U]
    
    def otherOrder = true 
    
    def othersFiltered(implicit ctx: TemplateContext) = for {
        other1 <- others
        other2 <- others
        if other1 != other2
        if !otherOrder || other1.## < other2.##
      } yield (other1, other2)
    
    def apply(obj: T)(implicit ctx: TemplateContext): Option[Expr] = {
      val stats = othersFiltered flatMap { case (other1, other2) =>
        if (condition(obj, other1, other2) && obj != other1 && obj != other2) {
          result(obj, other1, other2) match {
            case Block(Nil) => None
            case ParExpr(Nil) => None
            case NOP => None
            case stat => Some(stat.setPriority(priority(obj, other1, other2)))
          }
        } else None
      }
      
      if (stats.size == 0)
        None
      else 
        Some(ParExpr(stats.toList))
    }
  }


  /**
   * A TemplateParallel combines multiple templates to produce a ParExpr
   */
  trait TemplateParallel[T <: GameObject] extends Template[T] {
    /** All the templates that will be checked. */
    def templates: Traversable[Template[_]]
    
    /** The condition under which this template applies. */
    def condition(obj: T)(implicit ctx: TemplateContext): Boolean
    
    /** The priority of this template compare to others, given a game object. */
    def priority(obj: T)(implicit ctx: TemplateContext): Int
    
    def apply(obj: T)(implicit ctx: TemplateContext): Option[Expr] = {
      if (condition(obj)) {
        val results = templates.flatMap(_.applyForObject(obj)).toList
        results.sortWith(_.priority > _.priority) match {
          case Nil => None
          case sortedResults => Some(ParExpr(sortedResults).setPriority(priority(obj)))
        }
      } else None
    }
  }
  
  /**
   * A TemplateBlock combines multiple templates to produce a block of code.
   */
  trait TemplateBlock[T <: GameObject] extends Template[T] {
    /** All the templates that will be checked. */
    def templates: Traversable[Template[_]]
    
    /** The condition under which this template applies. */
    def condition(obj: T)(implicit ctx: TemplateContext): Boolean
    
    /** The priority of this template compare to others, given a game object. */
    def priority(obj: T)(implicit ctx: TemplateContext): Int
    
    def apply(obj: T)(implicit ctx: TemplateContext): Option[Expr] = {
      if (condition(obj)) {
        templates.flatMap(_.applyForObject(obj)).toSeq match {
          case Seq()   => None
          case results => Some(Block(results).setPriority(priority(obj)))
        }
      } else None
    }
  }
  
  trait TemplateObject extends Template[GameObject] {
    protected def typeCondition(obj: GameObject) = true
  }
  
  trait TemplateMovable extends Template[Movable] {
    protected def typeCondition(obj: GameObject) = obj.isInstanceOf[Movable]
  }
  
  trait TemplateRotationable extends Template[Rotationable] {
    protected def typeCondition(obj: GameObject) = obj.isInstanceOf[Rotationable]
  }
  
  trait TemplateColorable extends Template[Colorable] {
    protected def typeCondition(obj: GameObject) = obj.isInstanceOf[Colorable]
  }
  
  trait TemplateVisiblable extends Template[Visiblable] {
    protected def typeCondition(obj: GameObject) = obj.isInstanceOf[Visiblable]
  }
  
  trait TemplatePhysicalObject extends Template[PhysicalObject] {
    protected def typeCondition(obj: GameObject) = obj.isInstanceOf[PhysicalObject]
  }
  
  trait TemplateValue extends Template[Box[Int]] {
    protected def typeCondition(obj: GameObject) = obj.isInstanceOf[IntBox]
  }
  
  trait TemplateText extends Template[Box[String]] {
    protected def typeCondition(obj: GameObject) = obj.isInstanceOf[StringBox]
  }
  
  trait TemplateCircular extends Template[Circular] {
    protected def typeCondition(obj: GameObject) = obj.isInstanceOf[Circular]
  }
  
  trait TemplateResizableCircular extends Template[ResizableCircular] {
    protected def typeCondition(obj: GameObject) = obj.isInstanceOf[ResizableCircular]
  }
  
  trait TemplateRectangular extends Template[Rectangular] {
    protected def typeCondition(obj: GameObject) = obj.isInstanceOf[Rectangular]
  }
  
  trait TemplateOtherObject[T <: GameObject] extends TemplateOther[T, GameObject] {
    def others(implicit ctx: TemplateContext) = ctx.objects
  }
  
  trait TemplateOtherPositionable[T <: GameObject] extends TemplateOther[T, Positionable] {
    def others(implicit ctx: TemplateContext) = ctx.positionables
  }
  
  trait TemplateOtherRotationable[T <: GameObject] extends TemplateOther[T, Rotationable] {
    def others(implicit ctx: TemplateContext) = ctx.rotationables
  }
  
  trait TemplateOtherValue[T <: GameObject] extends TemplateOther[T, Box[Int]] {
    def others(implicit ctx: TemplateContext) = ctx.integers
  }
  
  trait TemplateOtherText[T <: GameObject] extends TemplateOther[T, Box[String]] {
    def others(implicit ctx: TemplateContext) = ctx.texts
  }

  trait TemplateOtherCircular[T <: GameObject] extends TemplateOther[T, Circular] {
    def others(implicit ctx: TemplateContext) = ctx.circles
  }
  
  trait TemplateOtherRectangular[T <: GameObject] extends TemplateOther[T, Rectangular] {
    def others(implicit ctx: TemplateContext) = ctx.rectangulars
  }
  
  trait TemplateOtherCell[T <: GameObject] extends TemplateOther[T, Cell] {
    def others(implicit ctx: TemplateContext) = ctx.cells
  }
  
  trait TemplateOtherPairObject[T <: GameObject] extends TemplateOtherPair[T, GameObject] {
    def others(implicit ctx: TemplateContext) = ctx.objects
  }
  
  trait TemplateOtherPairValue[T <: GameObject] extends TemplateOtherPair[T, Box[Int]] {
    def others(implicit ctx: TemplateContext) = ctx.integers
  }
  
  trait TemplateOtherPairText[T <: GameObject] extends TemplateOtherPair[T, Box[String]] {
    def others(implicit ctx: TemplateContext) = ctx.texts
  }
  
  trait TemplateOtherPairCircular[T <: GameObject] extends TemplateOtherPair[T, Circular] {
    def others(implicit ctx: TemplateContext) = ctx.circles
  }
  
  trait TemplateOtherPairRectangular[T <: GameObject] extends TemplateOtherPair[T, Rectangular] {
    def others(implicit ctx: TemplateContext) = ctx.rectangulars
  }
  
  trait TemplateOtherPairCell[T <: GameObject] extends TemplateOtherPair[T, Cell] {
    def others(implicit ctx: TemplateContext) = ctx.cells
  }

  object TX_DY1 extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.x.next - obj.x.get, -ctx.dy) && !ctx.isMovementHorizontal) {
        val expr = obj.x := obj.x - ctx.dy
        Some(expr.setPriority(0))
      } else {
        None
      }
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
      s"If the finger goes upwards, ${obj.name.next} moves horizontally to the right."
  }
  
  object TX_DY2 extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.x.next - obj.x.get, ctx.dy) && !ctx.isMovementHorizontal) {
        val expr = obj.x := obj.x + ctx.dy
        Some(expr.setPriority(0))
      } else {
        None
      }
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
      s"If the finger goes downwards, ${obj.name.next} moves horizontally to the right."
  }
  
  object TX_relative extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      val expr = obj.x := obj.x + coord(obj.x.next.toInt - obj.x.get.toInt)
      Some(expr.setPriority(5))
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
      s"Relative movement of ${obj.name.next} by (" + (obj.x.next.toInt - obj.x.get.toInt) + ", 0)"
  }
  
  object TX_absolute extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      val expr = obj.x := coord(obj.x.next.toInt)
      Some(expr.setPriority(6))
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
      s"Absolute positionning of ${obj.name.next} to x = ${obj.x.next.toInt}."
  }
  
  object TX_DX1 extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.x.next - obj.x.get, -ctx.dx) && !ctx.isMovementVertical) {
        val expr = obj.x := obj.x + (-ctx.dx)
        Some(expr.setPriority(3))
      } else {
        None
      }
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
      s"If the finger goes to the left, ${obj.name.next} moves horizontally to the right."
  }
  
  object TX_DX2 extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.x.next - obj.x.get, ctx.dx) && !ctx.isMovementVertical) {
        val expr = obj.x := obj.x + ctx.dx
        Some(expr.setPriority(15))
      } else {
        None
      }
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
      s"${obj.name.next} moves horizontally in the same direction as the finger."
  }
  
  object TX_AlignLeft1 extends TemplateOtherPositionable[Movable] with TemplateMovable {
    def result(obj: Movable, other: Positionable)(implicit ctx: TemplateContext) = {
      if (almostTheSame(obj.x.next, other.x.get, 20)) {
        val expr = obj.x := other.x
        Some(expr.setPriority(10))
      } else {
        None
      }
    }
    
    def comment(obj: Movable, other: Positionable)(implicit ctx: TemplateContext) = 
      s"${obj.name.next} aligns its x side to the x side of ${other.name.next}"
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
  
  object TX extends TemplateParallel[Movable] with TemplateMovable {
    def condition(obj: Movable)(implicit ctx: TemplateContext) = obj.x.get != obj.x.next
    def priority(obj: Movable)(implicit ctx: TemplateContext) = 10
    
    val templates = List(
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
      
    //def comment   = s"Possible x changes for ${shape.name.next}"
  }

  object TY_DX1 extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.y.next - obj.y.get, -ctx.dx) && !ctx.isMovementVertical) {
        val expr = obj.y := obj.y + (-ctx.dx)
        Some(expr.setPriority(0))
      } else {
        None
      }
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
      s"If the finger goes to the left, ${obj.name.next} moves vertically to the bottom."
  }
  
  object TY_DX2 extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.y.next - obj.y.get, ctx.dx) && !ctx.isMovementVertical) {
        val expr = obj.y := obj.y + ctx.dx
        Some(expr.setPriority(0))
      } else {
        None
      }
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
      s"If the finger goes to the left, ${obj.name.next} moves vertically to the top."
  }
  
  object TY_relative extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      val expr = obj.y += coord(obj.y.next.toInt - obj.y.get.toInt)
      Some(expr.setPriority(5))
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
      s"Relative movement of ${obj.name.next} by (0, " + (obj.y.next.toInt - obj.y.get.toInt) + ")"
  }
  
  object TY_absolute extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      val expr = obj.y := coord(obj.y.next.toInt)
      Some(expr.setPriority(6))
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
      s"Absolute positionning of ${obj.name.next} to y = " + obj.y.next.toInt
  }
  
  object TY_DY1 extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.y.next - obj.y.get, -ctx.dy) && !ctx.isMovementHorizontal) {
        val expr = obj.y += -ctx.dy
        Some(expr.setPriority(3))
      } else {
        None
      }
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
      s"If the finger goes to the bottom, ${obj.name.next} moves vertically to the top."
  }
  
  object TY_DY2 extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.y.next - obj.y.get, ctx.dy) && !ctx.isMovementHorizontal) {
        val expr = obj.y += ctx.dy
        Some(expr.setPriority(15))
      } else {
        None
      }
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
      obj.name.next + " moves vertically in the same direction as the finger"
  }
  
  object TY_AlignLeft1 extends TemplateOtherPositionable[Movable] with TemplateMovable {
    def result(obj: Movable, other: Positionable)(implicit ctx: TemplateContext) = {
      if (almostTheSame(obj.y.next, other.y.get, 20)) {
        val expr = obj.y := other.y
        Some(expr.setPriority(10))
      } else {
        None
      }
    }
    
    def comment(obj: Movable, other: Positionable)(implicit ctx: TemplateContext) = 
      s"${obj.name.next} aligns its y side to the y side of ${other.name.next}"
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
    
  object TY extends TemplateParallel[Movable] with TemplateMovable {
    def condition(obj: Movable)(implicit ctx: TemplateContext) = obj.y.get != obj.y.next
    def priority(obj: Movable)(implicit ctx: TemplateContext) = 10
    
    val templates = List(
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
      
    //def comment   = s"Possible y changes for ${shape.name.next}"
  }
  
  object TXY_Independent extends TemplateBlock[Movable] with TemplateMovable {
    def condition(obj: Movable)(implicit ctx: TemplateContext) = obj.x.get != obj.x.next || obj.y.get != obj.y.next
    def priority(obj: Movable)(implicit ctx: TemplateContext) = 10
    val templates = List(TX, TY)
    //def comment = s"Independent x and y changes for ${shape.name.next}"
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
  
  
  object TXY extends TemplateParallel[Movable] with TemplateMovable {
    def condition(obj: Movable)(implicit ctx: TemplateContext) = obj.x.get != obj.x.next || obj.y.get != obj.y.next
    def priority(obj: Movable)(implicit ctx: TemplateContext) = 10
    val templates = List(
        //TXY_CenterMirror,
        TXY_Independent
    )
    //def comment = s"All x and y changes for ${shape.name.next}"
  }
    
  object TAngleRelative extends TemplateSimple[Rotationable] with TemplateRotationable {
    def result(obj: Rotationable)(implicit ctx: TemplateContext) = {
      val expr = obj.angle -= angle(shiftAngle(obj)) 
      Some(expr.setPriority(8))
    }
    
    def comment(obj: Rotationable)(implicit ctx: TemplateContext) =
      s"Change the speed direction of ${obj.name.next} by " + shiftAngle(obj) + "°"
      
    def shiftAngle(obj: Rotationable) = Math.round((obj.angle.next - obj.angle.get) / 15) * 15
  }
  
  object TAngleAbsolute extends TemplateSimple[Rotationable] with TemplateRotationable {
    def result(obj: Rotationable)(implicit ctx: TemplateContext) = {
      val expr = obj.angle := angle(roundedAngle(obj))
      Some(expr.setPriority(9))
    }
    
    def comment(obj: Rotationable)(implicit ctx: TemplateContext) =
      s"Change the speed direction of ${obj.name.next} to " + roundedAngle(obj) + "°"
      
    def roundedAngle(obj: Rotationable) = Math.round(obj.angle.next / 15) * 15
  }
  
  /*object TAngleOnCircle extends TemplateShapeOtherCircle {
    def condition = TOUCHMOVE_EVENT && Math.abs(Game.angle(other_shape.x.next, other_shape.y.next, xTo, yTo) - shape.angle.next) < 20
    // TODO : Verify that such event is fired when finger down.
    def result    = shape_ident("angle") := Stat.angle(other_shape_ident.x, other_shape_ident.y, x_ident, y_ident)
    def priority = 10
    def comment   = s"Change the speed direction of ${shape.name.next} to equal the direction between the center of ${other_shape.name.next} and the finger touch"
  }*/
  
  object TAngleCopy extends TemplateOtherRotationable[Rotationable] with TemplateRotationable {
    def result(obj: Rotationable, other: Rotationable)(implicit ctx: TemplateContext) = {
      if (almostTheSame(obj.angle.next, other.angle.get, 15)) {
        val expr = obj.angle := other.angle
        Some(expr.setPriority(10))
      } else {
        None
      }
    }
    
    def comment(obj: Rotationable, other: Rotationable)(implicit ctx: TemplateContext) = 
      s"Copy the speed direction of " + other.name.next + s" to ${obj.name.next}"
  }
    
  object TAngle extends TemplateParallel[Rotationable] with TemplateRotationable {
    def condition(obj: Rotationable)(implicit ctx: TemplateContext) = 
      obj.angle.get != obj.angle.next && Math.abs(obj.angle.next - obj.angle.get) > 10 && Math.abs(obj.angle.next - obj.angle.get) < 350
      
    def priority(obj: Rotationable)(implicit ctx: TemplateContext) = 10
    
    val templates = List(
      TAngleRelative,
      TAngleAbsolute,
      //TAngleOnCircle,
      TAngleCopy)
      
    //def comment = s"Possible direction changes for ${shape.name.next}"
  }
  
  object TVelocityAbsolute extends TemplateSimple[PhysicalObject] with TemplatePhysicalObject {
    def result(obj: PhysicalObject)(implicit ctx: TemplateContext) = {
      val expr = obj.velocity := speed(obj.velocity.next)
      Some(expr.setPriority(10))
    }
    
    def comment(obj: PhysicalObject)(implicit ctx: TemplateContext) =
       s"Velocity of ${obj.name.next} is set absolutely to ${obj.velocity.next}."
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
  
  object TColorAbsolute extends TemplateSimple[Colorable] with TemplateColorable {
    def result(obj: Colorable)(implicit ctx: TemplateContext) = {
      val expr = obj.color := color(obj.color.next)
      Some(expr.setPriority(10))
    }
    
    def comment(obj: Colorable)(implicit ctx: TemplateContext) = 
      s"The color of ${obj.name.next} is set to " + obj.color.next
  }
  
  object TColor extends TemplateParallel[GameObject] with TemplateObject {
    def condition(obj: GameObject)(implicit ctx: TemplateContext) = obj.color.get != obj.color.next
    def priority(obj: GameObject)(implicit ctx: TemplateContext) = 10
    val templates = List(TColorAbsolute)

    // def comment = s"Possible color changes for ${shape.name.next}"
  }
  
  object TVisibleAbsolute extends TemplateSimple[Visiblable] with TemplateVisiblable {
    def result(obj: Visiblable)(implicit ctx: TemplateContext) = {
      val expr = obj.visible := BooleanLiteral(obj.visible.next)
      Some(expr.setPriority(10))
    }
    
    def comment(obj: Visiblable)(implicit ctx: TemplateContext) = 
      s"The visibility of ${obj.name.next} is set to " + obj.visible.next
  }
  
  object TVisibleToggle extends TemplateSimple[Visiblable] with TemplateVisiblable {
    def result(obj: Visiblable)(implicit ctx: TemplateContext) = {
      val expr = obj.visible := !obj.visible
      Some(expr.setPriority(10))
    }
    
    def comment(obj: Visiblable)(implicit ctx: TemplateContext) = 
      s"The visibility of ${obj.name.next} is toggled"
  }
  
  object TVisible extends TemplateParallel[GameObject] with TemplateObject {
    def condition(obj: GameObject)(implicit ctx: TemplateContext) = obj.visible.get != obj.visible.next
    def priority(obj: GameObject)(implicit ctx: TemplateContext) = 10
    val templates = List(TVisibleAbsolute)
    //def comment = s"Possible visibility changes for ${shape.name.next}"
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
  
  //TODO lomig translate these templates
//  object TValueAbsolute extends Template[Box[Int]] {
//    def condition = true
//    def result    = (shape_ident("value") := number(shape.value.next))
//    def priority  = 5
//    def comment   = s"Always change the value  of ${shape.name.next} to " + shape.value.next
//  }
//  object TValueRelative extends Template[Box[Int]] {
//    def condition = Math.abs(shape.value.next - shape.value.get) > 1
//    def result    = (shape_ident("value") := shape_ident("value") + number(shape.value.next - shape.value.get))
//    def priority  = 6
//    def comment   = s"Change the value of ${shape.name.next} by " + shape.value.next
//  }
//  object TValueDiv2 extends Template[Box[Int]] {
//    def condition = shape.value.next == shape.value.get / 2
//    def result    = (shape_ident("value") := shape_ident("value") / number(2))
//    def priority  = 7
//    def comment   = s"Divides the value of ${shape.name.next} by 2"
//  }
//  object TValueTimes extends Template[Box[Int]] {
//    def condition = shape.value.get != 0 && shape.value.next % shape.value.get == 0 && shape.value.next / shape.value.get != 0
//    def result    = (shape_ident("value") := (shape_ident("value") * number(shape.value.next / shape.value.get)))
//    def priority  = 8
//    def comment   = s"Multiplies the value of ${shape.name.next} by " + (shape.value.next / shape.value.get)
//  }
//  object TValueCombine1_absolute extends TemplateOtherValue {
//    def condition = shape.value.next == other_shape.value.get
//    def result    = (shape_ident("value") := other_shape_ident("value"))
//    def priority  = if(other_shape.value.get == 0) 5 else 10
//    def comment   = s"Copies the value of ${other_shape.name.next} to ${shape.name.next}"
//  }
//  object TValueCombine1_relativeMultCopy extends TemplateOtherValue {
//    def condition = other_shape.value.get != 0 && shape.value.next % other_shape.value.get == 0 && shape.value.next !=  other_shape.value.get && shape.value.next != 0
//    def result    = (shape_ident("value") := (other_shape_ident("value") * number(shape.value.next / other_shape.value.get)))
//    def priority  = if(shape.value.next / other_shape.value.get == 2) 8 else 10
//    def comment   = s"Stores in ${shape.name.next} the value of ${other_shape.name.next} multiplied by" + (shape.value.next / other_shape.value.get)
//  }
//  object TValueCombine1_relativeModulo extends TemplateOtherValue {
//    def condition = other_shape.value.get > 1 && shape.value.next == shape.value.get % other_shape.value.get
//    def result    = (shape_ident("value") := (shape_ident("value") % other_shape_ident("value")))
//    def priority  = 10
//    def comment   = s"Stores in ${shape.name.next} its previous value modulo ${other_shape.name.next}"
//  }
//  object TValueCombine2_plus extends TemplateOtherValue2 {
//    def condition = shape.value.next == other_shape.value.get + other_shape2.value.get && other_shape != other_shape2
//    def result = (shape_ident("value") := other_shape_ident("value") + other_shape2_ident("value"))
//    def priority = if(other_shape.value.get == 0 || other_shape2.value.get == 0) 0 else 10
//    def comment   = s"Stores in ${shape.name.next} the sum of ${other_shape.name.next} and ${other_shape2.name}"
//  }
//  object TValueCombine2_minus extends TemplateOtherValue2 {
//    override def order = false
//    def condition = shape.value.next == other_shape.value.get - other_shape2.value.get
//    def result = (shape_ident("value") := other_shape_ident("value") - other_shape2_ident("value"))
//    def priority = if(other_shape.value.get == 0 || other_shape2.value.get == 0) 0 else if(shape.value.next == 0) 1 else 10
//    def comment   = s"Stores in ${shape.name.next} the difference between ${other_shape.name.next} and ${other_shape2.name}"
//  }
//  object TValueCombine2_times extends TemplateOtherValue2 {
//    def condition = shape.value.next == other_shape.value.get * other_shape2.value.get
//    def result = (shape_ident("value") := other_shape_ident("value") * other_shape2_ident("value"))
//    def priority = if(other_shape.value.get == 0 || other_shape2.value.get == 0) 0 else if(other_shape.value.get == 1 || other_shape2.value.get == 1) 2 else 10
//    def comment   = s"Stores in ${shape.name.next} the multiplication between ${other_shape.name.next} and ${other_shape2.name}"
//  }
//  object TValueCombine2_div extends TemplateOtherValue2 {
//    override def order = false
//    def condition = other_shape2.value.get != 0 && shape.value.next == other_shape.value.get / other_shape2.value.get
//    def result = (shape_ident("value") := other_shape_ident("value") / other_shape2_ident("value"))
//    def priority = if(other_shape2.value.get == 0) 0 else if(other_shape.value.get % other_shape2.value.get != 0) 2 else 10
//    def comment   = s"Stores in ${shape.name.next} the division between ${other_shape.name.next} and ${other_shape2.name}"
//  }
//  object TValueRelative2 extends Template[Box[Int]] {
//    def condition = Math.abs(shape.value.next - shape.value.get) == 1
//    def result    = (shape_ident("value") := shape_ident("value") + number(shape.value.next - shape.value.get))
//    def priority = 12
//    def comment   = s"Adds to ${shape.name.next} the number ${shape.value.next - shape.value.get}"
//  }
//
//  object TValue extends TemplateParallel[Box[Int]] {
//    def condition = shape.value.get != shape.value.next
//    val templates = List(TValueAbsolute,
//      TValueRelative,
//      TValueDiv2,
//      TValueTimes,
//      TValueCombine1_absolute,
//      TValueCombine1_relativeMultCopy,
//      TValueCombine1_relativeModulo,
//      TValueCombine2_plus,
//      TValueCombine2_minus,
//      TValueCombine2_times,
//      TValueCombine2_div,
//      TValueRelative2
//    )
//    def priority = 10
//    def comment   = s"Possible value changes for ${shape.name.next}"
//  }
  
  object TTextCopy extends TemplateOtherText[Box[String]] with TemplateText {
    def result(obj: Box[String], other: Box[String])(implicit ctx: TemplateContext) = {
      if (obj.value.next == other.value.get) {
        val expr = obj.value := other.value
        val priority = if (other.value.get != "") 10 else 0
        Some(expr.setPriority(priority))
      } else {
        None
      }
    }
    
    def comment(obj: Box[String], other: Box[String])(implicit ctx: TemplateContext) = 
      s"Copy the text of " + other.name.next + s" to ${obj.name.next}"
  }
  
  object TTextConcatenate extends TemplateOtherPairText[Box[String]] with TemplateText {
    override def otherOrder = false
    
    def condition(obj: Box[String], other1: Box[String], other2: Box[String])(implicit ctx: TemplateContext) =
      obj.value.next == other1.value.get + other2.value.get
    
    def result(obj: Box[String], other1: Box[String], other2: Box[String])(implicit ctx: TemplateContext) =
      obj.value := other1.value + other2.value

    def priority(obj: Box[String], other1: Box[String], other2: Box[String])(implicit ctx: TemplateContext) =
      if(other1.value.get == "" || other2.value.get == "") 0 else 10
    
    def comment(obj: Box[String], other1: Box[String], other2: Box[String])(implicit ctx: TemplateContext) = 
      s"Concatenate the texts of ${other1.name.next} and ${other2.name} to ${obj.name.next}"
  }
  
  // TODO: convert integers to text boxes if detected.
  
  object TText extends TemplateParallel[Box[String]] with TemplateText {
    def condition(obj: Box[String])(implicit ctx: TemplateContext) = obj.value.get != obj.value.next
    def priority(obj: Box[String])(implicit ctx: TemplateContext) = 10
    val templates = List(
        TTextCopy,
        TTextConcatenate
    )
    //def comment   = s"Possible text changes for ${shape.name.next}"
  }
  
  object TRadiusRelativePlus extends TemplateSimple[ResizableCircular] with TemplateResizableCircular {
    def result(obj: ResizableCircular)(implicit ctx: TemplateContext) = {
      val expr = obj.radius += coord(obj.radius.next - obj.radius.get)
      Some(expr.setPriority(10))
    }

    def comment(obj: ResizableCircular)(implicit ctx: TemplateContext) = 
      s"Add a constant to the radius of ${obj.name.next}."
  }
  
  object TRadiusRelativeTimes extends TemplateSimple[ResizableCircular] with TemplateResizableCircular {
    def result(obj: ResizableCircular)(implicit ctx: TemplateContext) = {
      if (obj.radius.get != 0) {
        val expr = obj.radius *= coord(obj.radius.next / obj.radius.get)
        val priority = if (obj.radius.next / obj.radius.get < 1) 8 else 0
        Some(expr.setPriority(priority))
      } else {
        None
      }
    }

    def comment(obj: ResizableCircular)(implicit ctx: TemplateContext) = 
      s"Multiply the radius of ${obj.name.next} by a factor."
  }
  
  object TRadiusAbsolute extends TemplateSimple[ResizableCircular] with TemplateResizableCircular {
    def result(obj: ResizableCircular)(implicit ctx: TemplateContext) = {
      val expr = obj.radius := coord(obj.radius.next)
      Some(expr.setPriority(10))
    }

    def comment(obj: ResizableCircular)(implicit ctx: TemplateContext) = 
       s"Change the radius of ${obj.name.next} absolutely."
  }
  
  //TODO MIKAEL je ne comprend pas ce template
//  object TRadiusSwitch extends TemplateOtherCircle {
//    def condition = true
//    def result    = (shape_ident("radius") := (coord(shape.radius.get + shape.radius.next) - shape_ident("radius")))
//    def priority  = 6
//    def comment   = s"Add a constant to the radius of ${shape.name.next}"
//  }
  
  object TRadiusMoveX extends TemplateSimple[ResizableCircular] with TemplateResizableCircular {
    def result(obj: ResizableCircular)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.radius.next - obj.radius.get, ctx.dx) && !ctx.isMovementVertical) {
        val expr = obj.radius += ctx.dx
        Some(expr.setPriority(10))
      } else {
        None
      }
    }

    def comment(obj: ResizableCircular)(implicit ctx: TemplateContext) = 
       s"Augment the radius of ${obj.name.next} when the finger moves to the right."
  }
  
  object TRadiusMoveX_rev extends TemplateSimple[ResizableCircular] with TemplateResizableCircular {
    def result(obj: ResizableCircular)(implicit ctx: TemplateContext) = {
       if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.radius.next - obj.radius.get, -ctx.dx) && !ctx.isMovementVertical) {
        val expr = obj.radius += -ctx.dx
        Some(expr.setPriority(8))
      } else {
        None
      }
    }

    def comment(obj: ResizableCircular)(implicit ctx: TemplateContext) = 
       s"Augment the radius of ${obj.name.next} when the finger moves to the left."
  }
  
  object TRadiusMoveY extends TemplateSimple[ResizableCircular] with TemplateResizableCircular {
    def result(obj: ResizableCircular)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.radius.next - obj.radius.get, ctx.dy) && !ctx.isMovementHorizontal) {
        val expr = obj.radius += ctx.dy
        Some(expr.setPriority(10))
      } else {
        None
      }
    }

    def comment(obj: ResizableCircular)(implicit ctx: TemplateContext) = 
       s"Augment the radius of ${obj.name.next} when the finger moves to the bottom."
  }
  
  object TRadiusMoveY_rev extends TemplateSimple[ResizableCircular] with TemplateResizableCircular {
    def result(obj: ResizableCircular)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.radius.next - obj.radius.get, -ctx.dy) && !ctx.isMovementHorizontal) {
        val expr = obj.radius += -ctx.dy
        Some(expr.setPriority(8))
      } else {
        None
      }
    }

    def comment(obj: ResizableCircular)(implicit ctx: TemplateContext) = 
       s"Augment the radius of ${obj.name.next} when the finger moves to the top."
  }
  
  object TRadius extends TemplateParallel[Circular] with TemplateCircular {
    def condition(obj: Circular)(implicit ctx: TemplateContext) = obj.radius.get != obj.radius.next
    def priority(obj: Circular)(implicit ctx: TemplateContext) = 10
    val templates = List(
        TRadiusRelativePlus, 
        TRadiusRelativeTimes,
        TRadiusAbsolute,
        //TRadiusSwitch,
        TRadiusMoveX,
        TRadiusMoveX_rev,
        TRadiusMoveY,
        TRadiusMoveY_rev
    )
    // def comment   = s"Possible radius changes for ${shape.name.next}"
  }
  
  /* Array templates */
  
  object TArrayMoveUp extends TemplateOtherCell[Movable] with TemplateMovable {
    def result(obj: Movable, other: Cell)(implicit ctx: TemplateContext) = {
      if (other.row >= 1 &&
          other.contains(obj.center.get) &&
          other.array.cells(other.column)(other.row - 1).contains(obj.center.next)) {
        val upCellExpr = other.array.cell(Column(other), Row(other) - 1)
        val expr = If(Row(other) >= 1 && Contains(other, obj), Block(
          obj.x := upCellExpr.x,
          obj.y := upCellExpr.y
        )) 
        Some(expr.setPriority(5)) //TODO priority ? 
      } else {
        None
      }
    }
    
    def comment(obj: Movable, other: Cell)(implicit ctx: TemplateContext) = 
      s"Move ${obj.name.get} to the upper adjacent cell."
  }
  
  /** Top template */
  object TShape extends TemplateBlock[GameObject] with TemplateObject {
    def condition(obj: GameObject)(implicit ctx: TemplateContext) = true
    def priority(obj: GameObject)(implicit ctx: TemplateContext) = 10
    val templates = List(
        TXY,
        TAngle,
        //TVelocity,
        TColor,
        TVisible,
        //IfWidth(TWidth),
        //IfHeight(THeight),
        //TValue,
        TText,
        TRadius
    )
    //def comment   = s"The changes for shape ${shape.name.next}"
  }

}