package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong._
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.expression.TreeOps._
import ch.epfl.lara.synthesis.kingpong.rules.Events._
import PrettyPrinterExtendedTypical._

object CodeTemplates extends CodeHandler {
  
  class TemplateContext(val game: Game, val events: Seq[(Event, Int)], val objects: Traversable[GameObject], val minSnapDetect: Float) {
    val eventMove = events.collect{ case (e: FingerMove, _) => e }.headOption
    val (dx, dy, eventObjects) = eventMove match {
      case Some(FingerMove(from, to, objs)) => (to.x - from.x, to.y - from.y, objs)
      case None => (0f, 0f, Set.empty[GameObject])
    }
    val isMovementHorizontal = Math.abs(dx) > 10 * Math.abs(dy)
    val isMovementVertical = Math.abs(dy) > 10 * Math.abs(dx)
    val isTouchMoveEvent = eventMove.nonEmpty

    lazy val integers: Traversable[IntBox] = objects.collect{ case b: IntBox => b}
    lazy val texts: Traversable[ValueTextable] = objects.collect{ case b: ValueTextable => b}
    lazy val times: Traversable[Timeable] = objects.collect{ case b: Timeable => b}
    lazy val booleans: Traversable[Booleanable] = objects.collect{ case b: Booleanable => b }
    lazy val rectangulars: Traversable[Rectangular] = objects.collect{ case b: Rectangular => b }
    lazy val circles: Traversable[Circular] = objects.collect{ case b: Circular => b }
    lazy val positionables: Traversable[Positionable] = objects.collect{ case b: Positionable => b }
    lazy val rotationables: Traversable[Rotationable] = objects.collect{ case b: Rotationable => b }
    lazy val cells: Traversable[Cell] = objects.collect{ case c: Cell => c }
  }
  
  def inferStatements(game: Game, events: Seq[(Event, Int)], objects: Traversable[GameObject]): Seq[Expr] = {
    val minSnapAmount = 20/game.pixelsByUnit
    implicit val ctx = new TemplateContext(game, events, objects, minSnapAmount)
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
    //def comment(obj: T)(implicit ctx: TemplateContext): String
    
    /** 
     * The statement this template can return.
     * Return `None` if this template doesn't apply to the given object. 
     * Also set the priority on the expression, if any.
     */
    def result(obj: T)(implicit ctx: TemplateContext): Option[Expr]

    def apply(obj: T)(implicit ctx: TemplateContext): Option[Expr] = {
      result(obj)
    }
  }
  
  trait TemplateOther[T <: GameObject, U] extends Template[T] {
    //def comment(obj: T, other: U)(implicit ctx: TemplateContext): String
    
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
      } toList
      
      stats.sortWith(_.priority > _.priority) match {
        case Nil => None
        case sortedResults => Some(ParExpr(sortedResults).setPriority(sortedResults.head.priority))
      }
    }
  }
  
  /**
   * A TemplateOtherPair produces the combination of a template for type T against two objects of type U
   * The two shapes of type U are provided in order and are different.
   */
  trait TemplateOtherPair[T <: GameObject, U] extends Template[T] {
    //def comment(obj: T, other1: U, other2: U)(implicit ctx: TemplateContext): String
    
    /** 
     * The statement this template can return.
     * Return `None` if this template doesn't appy to the given objects. 
     * Also set the priority on the expression, if any.
     */
    def result(obj: T, other1: U, other2: U)(implicit ctx: TemplateContext): Option[Expr]

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
        if (obj != other1 && obj != other2) {
          result(obj, other1, other2)
        } else {
          None
        }
      } toList
      
      stats.sortWith(_.priority > _.priority) match {
        case Nil => None
        case sortedResults => Some(ParExpr(sortedResults).setPriority(sortedResults.head.priority))
      }
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
        var isCreated = false
        var initialObject: Expr = null
        var copyId: Identifier = null
        var isDeleted = false
        templates.flatMap(template => {
          if(!isDeleted) {
	          val res = template.applyForObject(obj);
	          if(template == TCreate) {
	            res match {
	              case Some(Copy(obj, id, _)) =>
			            // All remaining templates should abstract
			            isCreated = true
			            initialObject = obj
			            copyId = id
			            Nil
	              case _ => Nil
	            }
	          } else if(template == TDelete && res.nonEmpty)  {
	            // Stop reading other properties.
	            isDeleted = true
	            res;
	          } else {
	            if(isCreated && res.nonEmpty) { // Abstraction
	              var v = Variable(copyId: Identifier): Expr
	              Some(TreeOps.preMap(
	                node => (if(node == ObjectLiteral(obj)) Some(v) else None): Option[Expr]
	              )(res.get))
	            } else res
	          }
          } else Nil
        }).toSeq match {
          case Seq()   => None
          case results => 
            if(isCreated) {
              Some(Copy(initialObject, copyId, Block(results)).setPriority(priority(obj)))
            } else Some(Block(results).setPriority(priority(obj)))
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
  
  trait TemplateSpeedable extends Template[SpeedSettable with Rotationable] {
    protected def typeCondition(obj: GameObject) = obj.isInstanceOf[SpeedSettable] && obj.isInstanceOf[Rotationable]
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
  
  trait TemplateValue extends Template[IntBox] {
    protected def typeCondition(obj: GameObject) = obj.isInstanceOf[IntBox]
  }
  
  trait TemplateText extends Template[ValueTextable] {
    protected def typeCondition(obj: GameObject) = obj.isInstanceOf[ValueTextable]
  }
  
  trait TemplateTimeable extends Template[Timeable] {
    protected def typeCondition(obj: GameObject) = obj.isInstanceOf[Timeable]
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

  trait TemplateBooleanable extends Template[Booleanable] {
    protected def typeCondition(obj: GameObject) = obj.isInstanceOf[Booleanable]
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
  
  trait TemplateOtherInt[T <: GameObject] extends TemplateOther[T, IntBox] {
    def others(implicit ctx: TemplateContext) = ctx.integers
  }
  
  trait TemplateOtherText[T <: GameObject] extends TemplateOther[T, ValueTextable] {
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

  trait TemplateOtherBooleanable[T <: GameObject] extends TemplateOther[T, Booleanable] {
    def others(implicit ctx: TemplateContext) = ctx.booleans
  }
  
  trait TemplateOtherPairObject[T <: GameObject] extends TemplateOtherPair[T, GameObject] {
    def others(implicit ctx: TemplateContext) = ctx.objects
  }
  
  trait TemplateOtherPairValue[T <: GameObject] extends TemplateOtherPair[T, IntBox] {
    def others(implicit ctx: TemplateContext) = ctx.integers
  }
  
  trait TemplateOtherPairText[T <: GameObject] extends TemplateOtherPair[T, ValueTextable] {
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

//  object TX_DY1 extends TemplateSimple[Movable] with TemplateMovable {
//    def result(obj: Movable)(implicit ctx: TemplateContext) = {
//      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.x.next - obj.x.get, -ctx.dy) && !ctx.isMovementHorizontal) {
//        val expr = obj.x := obj.x - ctx.dy
//        Some(expr.setPriority(0).setComment(comment(obj)))
//      } else {
//        None
//      }
//    }
//    
//    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
//      s"If the finger goes upwards, " + obj + " moves horizontally to the right"
//  }
//  
//  object TX_DY2 extends TemplateSimple[Movable] with TemplateMovable {
//    def result(obj: Movable)(implicit ctx: TemplateContext) = {
//      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.x.next - obj.x.get, ctx.dy) && !ctx.isMovementHorizontal) {
//        val expr = obj.x := obj.x + ctx.dy
//        Some(expr.setPriority(0).setComment(comment(obj)))
//      } else {
//        None
//      }
//    }
//    
//    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
//      s"If the finger goes downwards, " + obj + " moves horizontally to the right"
//  }
  
  object TX_relative extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      val deltax: Expr = obj.x.next - obj.x.get
      val expr = obj.x += deltax
      Some(expr.setPriority(5).setComment(comment(obj, deltax)))
    }
    
    def comment(obj: Movable, deltax: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Move " + obj + " by (" + deltax + ", 0)"
  }
  
  object TX_absolute extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      val tox: Expr = obj.x.next
      val expr = obj.x := tox
      Some(expr.setPriority(6).setComment(comment(obj, tox)))
    }
    
    def comment(obj: Movable, tox: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Move " + obj + " to x = " + tox;
  }
  
  object TX_MoveInvertedDX extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.x.next - obj.x.get, -ctx.dx) && !ctx.isMovementVertical) {
        val expr = obj.x := obj.x + (-ctx.dx)
        Some(expr.setPriority(3).setComment(comment(obj)))
      } else {
        None
      }
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Move " + obj + " by the opposite of the horizontal finger movement"
  }
  
  object TX_MoveDX_Pos extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.x.next - obj.x.get, ctx.dx) && !ctx.isMovementVertical) {
        val expr = fingerMoveOver(obj) { move =>
          obj.x += move._2._1 - move._1._1
        }
        Some(expr.setPriority(14).setComment(comment(obj)))
      } else {
        None
      }
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "If finger on " + obj + ", move " + obj + " by the horizontal finger movement"
  }
  
//  object TX_AlignLeft1 extends TemplateOtherPositionable[Movable] with TemplateMovable {
//    def result(obj: Movable, other: Positionable)(implicit ctx: TemplateContext) = {
//      if (almostTheSame(obj.x.next, other.x.get, ctx.minSnapDetect)) {
//        val expr = obj.x := other.x
//        Some(expr.setPriority(10).setComment(comment(obj, other)))
//      } else {
//        None
//      }
//    }
//    
//    def comment(obj: Movable, other: Positionable)(implicit ctx: TemplateContext) = 
//      s"" + obj + " aligns its x side to the x side of " + other
//  }
  
  /*object TX_AlignLeft2 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.x.next, other_shape.prev_center_x, ctx.minSnapDetect)  && other_shape.prev_center_x != other_shape.prev_x
    def result    = (shape_ident("x") := other_shape_ident("center_x"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its x side to the center side of ${other_shape.name.next}"
  }
  object TX_AlignLeft3 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.x.next, other_shape.x.get + other_shape.width.get, ctx.minSnapDetect)
    def result    = (shape_ident("x") := other_shape_ident("x") + other_shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its x side to the right side of ${other_shape.name.next}"
  }
  object TX_AlignRight1 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.x.next + shape.width.get, other_shape.x.get + other_shape.width.get, ctx.minSnapDetect)
    def result    = (shape_ident("x") := other_shape_ident("x") + (other_shape_ident("width") - shape_ident("width")))
    def priority = 10
    def comment  = shape.name.next + s" aligns its right side to the right side of ${other_shape.name.next}"
  }
  object TX_AlignRight2 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.x.next + shape.width.get, other_shape.prev_center_x, ctx.minSnapDetect) && other_shape.prev_center_x != other_shape.prev_x
    def result    = (shape_ident("x") := other_shape_ident("center_x") - shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its right side to the center of ${other_shape.name.next}"
  }
  object TX_AlignRight3 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.x.next + shape.width.get, other_shape.x.get, ctx.minSnapDetect)
    def result    = (shape_ident("x") := other_shape_ident("x") - shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its right side to the left side of ${other_shape.name.next}"
  }
  object TX_AlignCenter1 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.center_x, other_shape.x.get + other_shape.width.get, ctx.minSnapDetect) && shape.center_x != shape.x.next
    def result    = (shape_ident.center_x = other_shape_ident("x") + other_shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its center to the right side of ${other_shape.name.next}"
  }*/
  /*object TX_AlignCenter2 extends TemplateOtherShape {
    def condition = almostTheSame(shape.center_x, other_shape.prev_center_x, ctx.minSnapDetect) && shape.center_x != shape.x.next && other_shape.prev_center_x != other_shape.prev_x
    def result    = (shape_ident.center_x = other_shape_ident.center_x)
    def priority = 10
    def comment  = shape.name.next + s" aligns its center to the center of ${other_shape.name.next}"
  }*/
  /*object TX_AlignCenter3 extends TemplateOtherShape {
    def condition = almostTheSame(shape.center_x, other_shape.x.get, ctx.minSnapDetect) && shape.center_x != shape.x.next
    def result    = (shape_ident.center_x = other_shape_ident("x"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its center to the left of ${other_shape.name.next}"
  }*/
  
  object TX extends TemplateParallel[Movable] with TemplateMovable {
    def condition(obj: Movable)(implicit ctx: TemplateContext) = obj.x.get != obj.x.next
    def priority(obj: Movable)(implicit ctx: TemplateContext) = 10
    
    val templates = List(
//      TX_DY1,
//      TX_DY2,
      TX_relative,
      TX_absolute,
      TX_MoveInvertedDX,
      TX_MoveDX_Pos
//      TX_AlignLeft1
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

//  object TY_DX1 extends TemplateSimple[Movable] with TemplateMovable {
//    def result(obj: Movable)(implicit ctx: TemplateContext) = {
//      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.y.next - obj.y.get, -ctx.dx) && !ctx.isMovementVertical) {
//        val expr = obj.y := obj.y + (-ctx.dx)
//        Some(expr.setPriority(0).setComment(comment(obj)))
//      } else {
//        None
//      }
//    }
//    
//    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
//      s"If the finger goes to the left, " + obj + " moves vertically to the bottom"
//  }
//  
//  object TY_DX2 extends TemplateSimple[Movable] with TemplateMovable {
//    def result(obj: Movable)(implicit ctx: TemplateContext) = {
//      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.y.next - obj.y.get, ctx.dx) && !ctx.isMovementVertical) {
//        val expr = obj.y := obj.y + ctx.dx
//        Some(expr.setPriority(0).setComment(comment(obj)))
//      } else {
//        None
//      }
//    }
//    
//    def comment(obj: Movable)(implicit ctx: TemplateContext) = 
//      s"If the finger goes to the left, " + obj + " moves vertically to the top"
//  }
  
  object TY_relative extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      val deltay: Expr = obj.y.next - obj.y.get
      val expr = obj.y += deltay
      Some(expr.setPriority(5).setComment(comment(obj, deltay)))
    }
    
    def comment(obj: Movable, deltay: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Move " + obj + " by (0, " + deltay + ")"
  }
  
  object TY_absolute extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      val toy: Expr = obj.y.next
      val expr = obj.y := toy
      Some(expr.setPriority(6).setComment(comment(obj, toy)))
    }
    
    def comment(obj: Movable, toy: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Move " + obj + " to y = " + toy
  }
  
  object TY_MoveInvertedDY extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.y.next - obj.y.get, -ctx.dy) && !ctx.isMovementHorizontal) {
        val expr = obj.y += -ctx.dy
        Some(expr.setPriority(3).setComment(comment(obj)))
      } else {
        None
      }
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Move " + obj + " by the opposite of the vertical finger movement"
  }
  
  object TY_MoveDY_Pos extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.y.next - obj.y.get, ctx.dy) && !ctx.isMovementHorizontal) {
        val expr = fingerMoveOver(obj) { move =>
          obj.y += move._2._2 - move._1._2
        }
        Some(expr.setPriority(14).setComment(comment(obj)))
      } else {
        None
      }
    }
    
    def comment(obj: Movable)(implicit ctx: TemplateContext) = (c: StringMaker) => 
     c + "Move " + obj + " by the vertical finger movement"
  }
  
//  object TY_AlignLeft1 extends TemplateOtherPositionable[Movable] with TemplateMovable {
//    def result(obj: Movable, other: Positionable)(implicit ctx: TemplateContext) = {
//      if (almostTheSame(obj.y.next, other.y.get, ctx.minSnapDetect)) {
//        val expr = obj.y := other.y
//        Some(expr.setPriority(10).setComment(comment(obj, other)))
//      } else {
//        None
//      }
//    }
//
//    def comment(obj: Movable, other: Positionable)(implicit ctx: TemplateContext) =
//      s"" + obj + " aligns its y side to the y side of " + other
//  }
  
  /*object TY_AlignLeft2 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.y.next, other_shape.prev_center_y, ctx.minSnapDetect)  && other_shape.prev_center_y != other_shape.prev_y
    def result    = (shape_ident("y") := other_shape_ident("center_y"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its y side to the center side of ${other_shape.name.next}"
  }
  object TY_AlignLeft3 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.y.next, other_shape.y.get + other_shape.width.get, ctx.minSnapDetect)
    def result    = (shape_ident("y") := other_shape_ident("y") + other_shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its y side to the bottom side of ${other_shape.name.next}"
  }
  object TY_AlignRight1 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.y.next + shape.width.get, other_shape.y.get + other_shape.width.get, ctx.minSnapDetect)
    def result    = (shape_ident("y") := other_shape_ident("y") + (other_shape_ident("width") - shape_ident("width")))
    def priority = 10
    def comment  = shape.name.next + s" aligns its bottom side to the bottom side of ${other_shape.name.next}"
  }
  object TY_AlignRight2 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.y.next + shape.width.get, other_shape.prev_center_y, ctx.minSnapDetect) && other_shape.prev_center_y != other_shape.prev_y
    def result    = (shape_ident("y") := other_shape_ident("center_y") - shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its bottom side to the center of ${other_shape.name.next}"
  }
  object TY_AlignRight3 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.y.next + shape.width.get, other_shape.y.get, ctx.minSnapDetect)
    def result    = (shape_ident("y") := other_shape_ident("y") - shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its bottom side to the top side of ${other_shape.name.next}"
  }
  object TY_AlignCenter1 extends TemplateOtherRectangular {
    def condition = almostTheSame(shape.center_y, other_shape.y.get + other_shape.width.get, ctx.minSnapDetect) && shape.center_y != shape.y.next
    def result    = (shape_ident.center_y = other_shape_ident("y") + other_shape_ident("width"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its center to the bottom side of ${other_shape.name.next}"
  }
  object TY_AlignCenter2 extends TemplateOtherShape {
    def condition = almostTheSame(shape.center_y, other_shape.prev_center_y, ctx.minSnapDetect) && shape.center_y != shape.y.next && other_shape.prev_center_y != other_shape.prev_y
    def result    = (shape_ident.center_y = other_shape_ident.center_y)
    def priority = 10
    def comment  = shape.name.next + s" aligns its center to the center of ${other_shape.name.next}"
  }
  object TY_AlignCenter3 extends TemplateOtherShape {
    def condition = almostTheSame(shape.center_y, other_shape.y.get, ctx.minSnapDetect) && shape.center_y != shape.y.next
    def result    = (shape_ident.center_y = other_shape_ident("y"))
    def priority = 10
    def comment  = shape.name.next + s" aligns its center to the top of ${other_shape.name.next}"
  }*/
    
  object TY extends TemplateParallel[Movable] with TemplateMovable {
    def condition(obj: Movable)(implicit ctx: TemplateContext) = obj.y.get != obj.y.next
    def priority(obj: Movable)(implicit ctx: TemplateContext) = 10
    
    val templates = List(
//      TY_DX1,
//      TY_DX2,
      TY_relative,
      TY_absolute,
      TY_MoveInvertedDY,
      TY_MoveDY_Pos
//      TY_AlignLeft1,

    /*IfHeight(TY_AlignLeft2),
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


  object TXY_Move_Force extends TemplateSimple[Movable] with TemplateMovable {
    def result(obj: Movable)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && !ctx.isMovementHorizontal && !ctx.isMovementVertical &&
          almostTheSameDiff(obj.x.next - obj.x.get, ctx.dx) && almostTheSameDiff(obj.y.next - obj.y.get, ctx.dy)) {
        val expr = fingerMoveOver(obj) { move =>
          ApplyForce(obj, (move._2 - move._1) * 80)
        }
        Some(expr.setPriority(15).setComment(comment(obj)))
      } else {
        None
      }
    }

    def comment(obj: Movable)(implicit ctx: TemplateContext) = (c: StringMaker) => 
     c + "Accelerate " + obj + " following the finger"
  }

  /** Move the object by a relative number of columns and rows inside a matrix. */
  object TXY_Cell_Move_Relative extends TemplateOtherCell[Movable] with TemplateMovable {
    def result(obj: Movable, other: Cell)(implicit ctx: TemplateContext) = {
      other.array.containingCell(obj.center.next) match {
        case Some(destCell) if other.contains(obj.center.get) && other != destCell =>
          val dx: Expr = destCell.column - other.column
          val dy: Expr = destCell.row - other.row
          val expr = whenever(Contains(other, obj)) (
            let("destCell", other.array.cell(other.columnProp + dx, other.rowProp + dy)) { destCellExpr =>
              whenever(notNull(destCellExpr)) (
                obj.x := destCellExpr.x,
                obj.y := destCellExpr.y
              )
            }
          )
          Some(expr.setPriority(16).setComment(comment(obj, other.array, dx, dy)))

        case _ =>
          None
      }
    }
    
    def comment(obj: Movable, array: Array2D, dx: Expr, dy: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Move " + obj + " on array  " + array + " by (" + dx + "," + dy + ")"
  }

  /** Move the object to an absolute cell. */
  object TXY_Cell_Move_Absolute extends TemplateOtherCell[Movable] with TemplateMovable {
    def result(obj: Movable, other: Cell)(implicit ctx: TemplateContext) = {
      other.array.containingCell(obj.center.next) match {
        case Some(destCell) if other.contains(obj.center.get) && other != destCell =>
          val expr = whenever(Contains(other, obj)) (
            obj.x := destCell.x,
            obj.y := destCell.y
          )
          Some(expr.setPriority(15).setComment(comment(obj, destCell)))

        case _ =>
          None
      }
    }

    def comment(obj: Movable, destCell: Cell)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Move " + obj + " to cell " + destCell
  }
  
  object TXY extends TemplateParallel[Movable] with TemplateMovable {
    def condition(obj: Movable)(implicit ctx: TemplateContext) = obj.x.get != obj.x.next || obj.y.get != obj.y.next
    def priority(obj: Movable)(implicit ctx: TemplateContext) = 10
    val templates = List(
      //TXY_CenterMirror,
      TXY_Move_Force,
      TArraySnapWithCoordinates,
      TArraySnapWithVelocity,
      TArraySnapWithForce,
      TXY_Cell_Move_Relative,
      TXY_Cell_Move_Absolute,
      TXY_Independent
    )
    //def comment = s"All x and y changes for ${shape.name.next}"
  }
    
  object TAngleRelative extends TemplateSimple[Rotationable] with TemplateRotationable {
    def result(obj: Rotationable)(implicit ctx: TemplateContext) = {
      val angle: Expr = shiftAngle(obj)
      val expr = obj.angle -= angle
      Some(expr.setPriority(8).setComment(comment(obj, angle)))
    }
    
    def comment(obj: Rotationable, angle: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Change the speed direction of " + obj + " by " + angle + "°"
      
    def shiftAngle(obj: Rotationable) = Math.round((obj.angle.next - obj.angle.get) / 15) * 15
  }
  
  object TAngleAbsolute extends TemplateSimple[Rotationable] with TemplateRotationable {
    def result(obj: Rotationable)(implicit ctx: TemplateContext) = {
      val angle: Expr = roundedAngle(obj)
      val expr = obj.angle := angle
      Some(expr.setPriority(9).setComment(comment(obj, angle)))
    }
    
    def comment(obj: Rotationable, angle: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Change the speed direction of " + obj + " to " + angle + "°"
      
    def roundedAngle(obj: Rotationable) = Math.round(obj.angle.next / 15) * 15
  }
  
  /*object TAngleOnCircle extends TemplateShapeOtherCircle {
    def condition = TOUCHMOVE_EVENT && Math.abs(Game.angle(other_shape.x.next, other_shape.y.next, xTo, yTo) - shape.angle.next) < ctx.minSnapDetect
    // TODO : Verify that such event is fired when finger down.
    def result    = shape_ident("angle") := Stat.angle(other_shape_ident.x, other_shape_ident.y, x_ident, y_ident)
    def priority = 10
    def comment   = s"Change the speed direction of ${shape.name.next} to equal the direction between the center of ${other_shape.name.next} and the finger touch"
  }*/
  
  object TAngleCopy extends TemplateOtherRotationable[Rotationable] with TemplateRotationable {
    def result(obj: Rotationable, other: Rotationable)(implicit ctx: TemplateContext) = {
      if (almostTheSame(obj.angle.next, other.angle.get, 15)) {
        val otherangle = other.angle.expr
        val expr = obj.angle := otherangle
        Some(expr.setPriority(10).setComment(comment(obj, other)))
      } else {
        None
      }
    }
    
    def comment(obj: Rotationable, other: Rotationable)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Copy the speed direction of " + other + " to " + obj
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
  
  object TVelocityAbsolute extends TemplateSimple[SpeedSettable with Rotationable] with TemplateSpeedable {
    def result(obj: SpeedSettable with Rotationable)(implicit ctx: TemplateContext) = {
      val newV: Expr = obj.velocity.next
      val expr = obj.velocity := newV
      Some(expr.setPriority(10).setComment(comment(obj, newV)))
    }
    
    def comment(obj: SpeedSettable with Rotationable, newV: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Set velocity of " + obj + " to " + newV
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
  
  object TVelocity extends TemplateParallel[SpeedSettable with Rotationable] with TemplateSpeedable {
    def condition(shape: SpeedSettable with Rotationable)(implicit ctx: TemplateContext) = {
      val vp = shape.velocity.get
      val vn = shape.velocity.next
      (vp.x != vn.x || vp.y != vn.y)
      //shape.velocity.get != shape.velocity.next && (Math.round(Math.abs(shape.angle.next - shape.angle.get)/15)*15 == 0 || shape.velocity.get == 0 || shape.velocity.next == 0 || shape.velocity.next / shape.velocity.get > 2 || shape.velocity.get / shape.velocity.next > 2)
    }
    def priority(obj: SpeedSettable with Rotationable)(implicit ctx: TemplateContext) = 10
    def comment(obj: SpeedSettable with Rotationable)(implicit ctx: TemplateContext) = s"Velocity changes for " + obj
    val templates: Traversable[Template[SpeedSettable with Rotationable]] = List(TVelocityAbsolute)
  }
  
  object TColorAbsolute extends TemplateSimple[Colorable] with TemplateColorable {
    def result(obj: Colorable)(implicit ctx: TemplateContext) = {
      val newColor: Expr = obj.color.next
      val expr = obj.color := newColor
      Some(expr.setPriority(10).setComment(comment(obj, newColor)))
    }
    
    def comment(obj: Colorable, newColor: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + s"Set the color of " + obj + " to " + newColor
  }
  
  object TColor extends TemplateParallel[GameObject] with TemplateObject {
    def condition(obj: GameObject)(implicit ctx: TemplateContext) = obj.color.get != obj.color.next
    def priority(obj: GameObject)(implicit ctx: TemplateContext) = 10
    val templates = List(TColorAbsolute)
  }
  
  object TVisibleAbsolute extends TemplateSimple[Visiblable] with TemplateVisiblable {
    def result(obj: Visiblable)(implicit ctx: TemplateContext) = {
      val boolb = BooleanLiteral(obj.visible.next)
      val expr = obj.visible := boolb
      Some(expr.setPriority(10).setComment(comment(obj, boolb)))
    }
    
    def comment(obj: Visiblable, boolb: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Set the visibility of " + obj + " to " + boolb
  }
  
  object TVisibleToggle extends TemplateSimple[Visiblable] with TemplateVisiblable {
    def result(obj: Visiblable)(implicit ctx: TemplateContext) = {
      val expr = obj.visible := !obj.visible
      Some(expr.setPriority(10).setComment(comment(obj)))
    }
    
    def comment(obj: Visiblable)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Toggle the visibility of " + obj
  }
  
  object TVisible extends TemplateParallel[GameObject] with TemplateObject {
    def condition(obj: GameObject)(implicit ctx: TemplateContext) = obj.visible.get != obj.visible.next
    def priority(obj: GameObject)(implicit ctx: TemplateContext) = 10
    val templates = List(TVisibleAbsolute, TVisibleToggle)
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
  
  
  object TValueAbsolute extends TemplateSimple[IntBox] with TemplateValue {
    def result(obj: IntBox)(implicit ctx: TemplateContext) = {
      val value: Expr = obj.value.next
      val expr = obj.value := value
      Some(expr.setPriority(5).setComment(comment(obj, value)))
    }
    
    def comment(obj: IntBox, value: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Change the value of " + obj + " to " + value
  }
  
  object TValueRelative extends TemplateSimple[IntBox] with TemplateValue {
    def result(obj: IntBox)(implicit ctx: TemplateContext) = {
      val diff = obj.value.next - obj.value.get
      if (Math.abs(diff) > 1) {
        val diffe: Expr = diff;
        val expr = obj.value += diffe
        Some(expr.setPriority(6).setComment(comment(obj, diffe)))
      } else {
        None
      }
    }
    
    def comment(obj: IntBox, value: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Change the value of " + obj + " by " + value
  }
  
  object TValueRelative2 extends TemplateSimple[IntBox] with TemplateValue {
    def result(obj: IntBox)(implicit ctx: TemplateContext) = {
      val diff = obj.value.next - obj.value.get
      if (Math.abs(diff) == 1) {
        val diffe: Expr = diff;
        val expr = obj.value += diff
        Some(expr.setPriority(12).setComment(comment(obj, diffe)))
      } else {
        None
      }
    }
    
    def comment(obj: IntBox, number: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Adds "+ number +" to the value of " + obj
  }
  
  object TValueDiv2 extends TemplateSimple[IntBox] with TemplateValue {
    def result(obj: IntBox)(implicit ctx: TemplateContext) = {
      if (obj.value.next == obj.value.get / 2) {
        val two: Expr = 2
        val expr = obj.value /= two
        Some(expr.setPriority(7).setComment(comment(obj, two)))
      } else {
        None
      }
    }
    
    def comment(obj: IntBox, two: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Divide the value of " + obj + " by " + two
  }
  
  object TValueTimes extends TemplateSimple[IntBox] with TemplateValue {
    def result(obj: IntBox)(implicit ctx: TemplateContext) = {
      if (obj.value.get != 0 && obj.value.next % obj.value.get == 0 && obj.value.next / obj.value.get != 0) {
        val factor: Expr = obj.value.next / obj.value.get
        val expr = obj.value *= factor
        Some(expr.setPriority(8).setComment(comment(obj, factor)))
      } else {
        None
      }
    }
    
    def comment(obj: IntBox, factor: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Multiply the value of " + obj + " by " + factor
  }
  
  
  object TValueCombine1_absolute extends TemplateOtherInt[IntBox] with TemplateValue {
    def result(obj: IntBox, other: IntBox)(implicit ctx: TemplateContext) = {
      if (obj.value.next == other.value.get) {
        val expr = obj.value := other.value
        val priority = if(other.value.get == 0) 5 else 10
        Some(expr.setPriority(priority))
      } else {
        None
      }
    }
    
    def comment(obj: IntBox, other: IntBox)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Copy the value of " + obj + " to " + obj
  }
  
  object TValueCombine1_relativeMultCopy extends TemplateOtherInt[IntBox] with TemplateValue {
    def result(obj: IntBox, other: IntBox)(implicit ctx: TemplateContext) = {
      if (other.value.get != 0 && obj.value.next % other.value.get == 0 && obj.value.next != other.value.get && obj.value.next != 0) {
        val factor: Expr = obj.value.next / other.value.get
        val expr = obj.value := other.value * factor
        val priority = if(obj.value.next / other.value.get == 2) 8 else 10
        Some(expr.setPriority(priority))
      } else {
        None
      }
    }
    
    def comment(obj: IntBox, other: IntBox, factor: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Stors in " + obj + " the value of " + other + " multiplied by" + factor
  }
  
  object TValueCombine1_relativeModulo extends TemplateOtherInt[IntBox] with TemplateValue {
    def result(obj: IntBox, other: IntBox)(implicit ctx: TemplateContext) = {
      if (other.value.get > 1 && obj.value.next == obj.value.get % other.value.get) {
        val expr = obj.value := obj.value % other.value
        Some(expr.setPriority(10).setComment(comment(obj, other)))
      } else {
        None
      }
    }
    
    def comment(obj: IntBox, other: IntBox)(implicit ctx: TemplateContext) =  (c: StringMaker) => 
      c + "Store in " + obj + " its previous value modulo " + other
  }
  
  object TValueCombine2_plus extends TemplateOtherPairValue[IntBox] with TemplateValue {
    def result(obj: IntBox, other1: IntBox, other2: IntBox)(implicit ctx: TemplateContext) = {
      if (obj.value.next == other1.value.get + other2.value.get) {
        val expr = obj.value := other1.value + other2.value
        val priority = if(other1.value.get == 0 || other2.value.get == 0) 0 else 10
        Some(expr.setPriority(priority).setComment(comment(obj, other1, other2)))
      } else {
        None
      }
    }
      
    def comment(obj: IntBox, other1: IntBox, other2: IntBox)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Store in " + obj + " the sum of "+other1+" and "+other2
  }
  
  object TValueCombine2_minus extends TemplateOtherPairValue[IntBox] with TemplateValue {
    override def otherOrder = false
    def result(obj: IntBox, other1: IntBox, other2: IntBox)(implicit ctx: TemplateContext) = {
      if (obj.value.next == other1.value.get - other2.value.get) {
        val expr = obj.value := other1.value - other2.value
        val priority = if(other1.value.get == 0 || other2.value.get == 0) 0 else if(obj.value.next == 0) 1 else 10
        Some(expr.setPriority(priority).setComment(comment(obj, other1, other2)))
      } else {
        None
      }
    }
      
    def comment(obj: IntBox, other1: IntBox, other2: IntBox)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Store in " + obj + " the difference between " + other1 + " and " + other2
  }
  
  object TValueCombine2_times extends TemplateOtherPairValue[IntBox] with TemplateValue {
    override def otherOrder = false
    def result(obj: IntBox, other1: IntBox, other2: IntBox)(implicit ctx: TemplateContext) = {
      if (obj.value.next == other1.value.get * other2.value.get) {
        val expr = obj.value := other1.value * other2.value
        val priority = if(other1.value.get == 0 || other2.value.get == 0) 0 else if(other1.value.get == 1 || other2.value.get == 1) 2 else 10
        Some(expr.setPriority(priority).setComment(comment(obj, other1, other2)))
      } else {
        None
      }
    }
      
    def comment(obj: IntBox, other1: IntBox, other2: IntBox)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Store in " + obj + " the multiplication between " + other1 + " and " + other2
  }
  
  object TValueCombine2_div extends TemplateOtherPairValue[IntBox] with TemplateValue {
    override def otherOrder = false
    def result(obj: IntBox, other1: IntBox, other2: IntBox)(implicit ctx: TemplateContext) = {
      if (other2.value.get != 0 && obj.value.next == other1.value.get / other2.value.get) {
        val expr = obj.value := other1.value / other2.value
        val priority = if(other2.value.get == 0) 0 else if(other1.value.get % other2.value.get != 0) 2 else 10
        Some(expr.setPriority(priority).setComment(comment(obj, other1, other2)))
      } else {
        None
      }
    }
      
    def comment(obj: IntBox, other1: IntBox, other2: IntBox)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Store in " + obj + " the division between " + other1 + " and " + other2
  }
  
  object TValueCombine2_mod extends TemplateOtherPairValue[IntBox] with TemplateValue {
    override def otherOrder = false
    def result(obj: IntBox, other1: IntBox, other2: IntBox)(implicit ctx: TemplateContext) = {
      if (other2.value.get != 0 && obj.value.next == other1.value.get % other2.value.get) {
        val expr = obj.value := other1.value % other2.value
        val priority = if(other2.value.get == 0) 0 else if(other1.value.get % other2.value.get == 0) 2 else 10
        Some(expr.setPriority(priority).setComment(comment(obj, other1, other2)))
      } else {
        None
      }
    }
      
    def comment(obj: IntBox, other1: IntBox, other2: IntBox)(implicit ctx: TemplateContext) =  (c: StringMaker) => 
      c + "Store in " + obj + " the remainder of the division between " + other1 + " and " + other2
  }

  object TValue extends TemplateParallel[IntBox] with TemplateValue {
    def condition(obj: IntBox)(implicit ctx: TemplateContext) = obj.value.get != obj.value.next
    def priority(obj: IntBox)(implicit ctx: TemplateContext) = 10
    val templates = List(
      TValueAbsolute,
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
      TValueCombine2_mod,
      TValueRelative2
    )
//    def comment   = s"Possible value changes for ${shape.name.next}"
  }
  
  object TTextAbsolute extends TemplateSimple[ValueTextable] with TemplateText {
    def result(obj: ValueTextable)(implicit ctx: TemplateContext) = {
      val texpr: Expr = obj.value.next
      val expr = obj.value := texpr
      Some(expr.setPriority(5).setComment(comment(obj, texpr)))
    }
    
    def comment(obj: ValueTextable, texpr: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Change the text of " + obj + " to \"" + texpr + "\""
  }
  
  object TTextCopy extends TemplateOtherText[ValueTextable] with TemplateText {
    def result(obj: ValueTextable, other: ValueTextable)(implicit ctx: TemplateContext) = {
      if (obj.value.next == other.value.get) {
        val expr = obj.value := other.value
        val priority = if (other.value.get != "") 10 else 0
        Some(expr.setPriority(priority).setComment(comment(obj, other)))
      } else {
        None
      }
    }
    
    def comment(obj: ValueTextable, other: ValueTextable)(implicit ctx: TemplateContext) =  (c: StringMaker) => 
      c + "Copy the text of " + other + s" to " + obj
  }
  
  object TTextConcatenate extends TemplateOtherPairText[ValueTextable] with TemplateText {
    override def otherOrder = false
    
    def result(obj: ValueTextable, other1: ValueTextable, other2: ValueTextable)(implicit ctx: TemplateContext) = {
      if (obj.value.next == other1.value.get + other2.value.get) {
        val texpr: Expr =  other1.value + other2.value
        val expr = obj.value := texpr
        val priority = if(other1.value.get == "" || other2.value.get == "") 0 else 10
        Some(expr.setPriority(priority))
      } else {
        None
      }
    }
      
    def comment(obj: ValueTextable, other1: ValueTextable, other2: ValueTextable)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Concatenate the texts of " + other1 + " and " + other2 + " to " + obj
  }
  
  // TODO: convert integers to text boxes if detected.
  
  object TText extends TemplateParallel[ValueTextable] with TemplateText {
    def condition(obj: ValueTextable)(implicit ctx: TemplateContext) = obj.value.get != obj.value.next
    def priority(obj: ValueTextable)(implicit ctx: TemplateContext) = 10
    val templates = List(
        TTextAbsolute,
        TTextCopy,
        TTextConcatenate
    )
    //def comment   = s"Possible text changes for ${shape.name.next}"
  }
  
  object TTimeAbsolute extends TemplateSimple[Timeable] with TemplateTimeable {
    def result(obj: Timeable)(implicit ctx: TemplateContext) = {
      val texpr: Expr = obj.time.next;
      val expr = obj.time := texpr
      Some(expr.setPriority(5).setComment(comment(obj, texpr)))
    }
    
    def comment(obj: Timeable, texpr: Expr)(implicit ctx: TemplateContext) =  (c: StringMaker) => 
      c + "Change the time of " + obj.asInstanceOf[GameObject] + " to " + texpr
  }

  object TTimeRepeat extends TemplateSimple[Timeable] with TemplateTimeable {
    def result(obj: Timeable)(implicit ctx: TemplateContext) = {
      val texpr: Expr = obj.time + (obj.time.next - obj.time.get)
      val expr = obj.time := texpr
      Some(expr.setPriority(5).setComment(comment(obj, texpr)))
    }
    
    def comment(obj: Timeable, texpr: Expr)(implicit ctx: TemplateContext) =  (c: StringMaker) => 
      c + "Change the time of " + obj  + " by " + texpr
  }
  
  object TTime extends TemplateParallel[Timeable] with TemplateTimeable {
    def condition(obj: Timeable)(implicit ctx: TemplateContext) = obj.time.get != obj.time.next
    def priority(obj: Timeable)(implicit ctx: TemplateContext) = 10
    val templates = List(
      TTimeAbsolute,
      TTimeRepeat
    )
  }
  
  object TRadiusRelativePlus extends TemplateSimple[ResizableCircular] with TemplateResizableCircular {
    def result(obj: ResizableCircular)(implicit ctx: TemplateContext) = {
      val constant: Expr = obj.radius.next - obj.radius.get
      val expr = obj.radius += constant
      Some(expr.setPriority(10).setComment(comment(obj, constant)))
    }

    def comment(obj: ResizableCircular, constant: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Add " + constant + " to the radius of " + obj.asInstanceOf[GameObject]
  }
  
  object TRadiusRelativeTimes extends TemplateSimple[ResizableCircular] with TemplateResizableCircular {
    def result(obj: ResizableCircular)(implicit ctx: TemplateContext) = {
      if (obj.radius.get != 0) {
        val constant: Expr = obj.radius.next / obj.radius.get
        val expr = obj.radius *= constant
        val priority = if (obj.radius.next / obj.radius.get < 1) 8 else 0
        Some(expr.setPriority(priority).setComment(comment(obj, constant)))
      } else {
        None
      }
    }

    def comment(obj: ResizableCircular, constant: Expr)(implicit ctx: TemplateContext) =  (c: StringMaker) => 
      c + "Multiply the radius of " + obj + " by " + constant
  }
  
  object TRadiusAbsolute extends TemplateSimple[ResizableCircular] with TemplateResizableCircular {
    def result(obj: ResizableCircular)(implicit ctx: TemplateContext) = {
      val constant: Expr = obj.radius.next
      val expr = obj.radius := constant
      Some(expr.setPriority(10).setComment(comment(obj, constant)))
    }

    def comment(obj: ResizableCircular, constant: Expr)(implicit ctx: TemplateContext) =  (c: StringMaker) => 
       c + "Change the radius of " + obj + " to " + constant
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
        Some(expr.setPriority(10).setComment(comment(obj)))
      } else {
        None
      }
    }

    def comment(obj: ResizableCircular)(implicit ctx: TemplateContext) =  (c: StringMaker) => 
       c + "Augment the radius of " + obj + " when the finger moves to the right"
  }
  
  object TRadiusMoveX_rev extends TemplateSimple[ResizableCircular] with TemplateResizableCircular {
    def result(obj: ResizableCircular)(implicit ctx: TemplateContext) = {
       if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.radius.next - obj.radius.get, -ctx.dx) && !ctx.isMovementVertical) {
        val expr = obj.radius += -ctx.dx
        Some(expr.setPriority(8).setComment(comment(obj)))
      } else {
        None
      }
    }

    def comment(obj: ResizableCircular)(implicit ctx: TemplateContext) =  (c: StringMaker) => 
       c + "Augment the radius of " + obj + " when the finger moves to the left"
  }
  
  object TRadiusMoveY extends TemplateSimple[ResizableCircular] with TemplateResizableCircular {
    def result(obj: ResizableCircular)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.radius.next - obj.radius.get, ctx.dy) && !ctx.isMovementHorizontal) {
        val expr = obj.radius += ctx.dy
        Some(expr.setPriority(10).setComment(comment(obj)))
      } else {
        None
      }
    }

    def comment(obj: ResizableCircular)(implicit ctx: TemplateContext) =  (c: StringMaker) => 
      c + "Augment the radius of " + obj + " when the finger moves to the bottom"
  }
  
  object TRadiusMoveY_rev extends TemplateSimple[ResizableCircular] with TemplateResizableCircular {
    def result(obj: ResizableCircular)(implicit ctx: TemplateContext) = {
      if (ctx.isTouchMoveEvent && almostTheSameDiff(obj.radius.next - obj.radius.get, -ctx.dy) && !ctx.isMovementHorizontal) {
        val expr = obj.radius += -ctx.dy
        Some(expr.setPriority(8).setComment(comment(obj)))
      } else {
        None
      }
    }

    def comment(obj: ResizableCircular)(implicit ctx: TemplateContext) =  (c: StringMaker) => 
      c + "Augment the radius of " + obj + " when the finger moves to the top"
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


  object TBooleanAbsolute extends TemplateSimple[Booleanable] with TemplateBooleanable {
    def result(obj: Booleanable)(implicit ctx: TemplateContext) = {
      val constant: Expr = obj.value.next
      val expr = obj.value := obj.value.next
      Some(expr.setPriority(10).setComment(comment(obj, constant)))
    }

    def comment(obj: Booleanable, constant: Expr)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Set the value of " + obj + " to " + constant
  }

  object TBooleanToggle extends TemplateSimple[Booleanable] with TemplateBooleanable {
    def result(obj: Booleanable)(implicit ctx: TemplateContext) = {
      val expr = obj.value := !obj.value
      Some(expr.setPriority(9).setComment(comment(obj)))
    }

    def comment(obj: Booleanable)(implicit ctx: TemplateContext) =(c: StringMaker) => 
      c + "Toggle the boolean value of " + obj
  }

  object TBooleanCopy extends TemplateOtherBooleanable[Booleanable] with TemplateBooleanable {
    def result(obj: Booleanable, other: Booleanable)(implicit ctx: TemplateContext) = {
      if (obj.value.next == other.value.get) {
        val expr = obj.value := other.value
        Some(expr.setPriority(8).setComment(comment(obj, other)))
      } else {
        None
      }
    }

    def comment(obj: Booleanable, other: Booleanable)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Copy the boolean value of " + other + " to " + obj
  }


  object TBoolean extends TemplateParallel[Booleanable] with TemplateBooleanable {
    def condition(obj: Booleanable)(implicit ctx: TemplateContext) = obj.value.get != obj.value.next
    def priority(obj: Booleanable)(implicit ctx: TemplateContext) = 10
    val templates = List(
      TBooleanAbsolute,
      TBooleanToggle,
      TBooleanCopy
    )
  }
  
  /* Snapping templates */

  object TArraySnapWithCoordinates extends TemplateOtherCell[Movable] with TemplateMovable {
    def result(obj: Movable, other: Cell)(implicit ctx: TemplateContext) = {
      if (other.contains(obj.center.get) &&
        almostTheSame(obj.x.next, other.x.get, other.width.get / 5) &&
        almostTheSame(obj.y.next, other.y.get, other.height.get / 5)) {
        val expr = Seq(
          obj.x := other.x,
          obj.y := other.y
        )
        Some(expr.setPriority(14).setComment(comment(obj, other)))
      } else {
        None
      }
    }

    def comment(obj: Movable, other: Cell)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Snap " + obj + " to the center of cell "+ other
  }

  object TArraySnapWithVelocity extends TemplateOtherCell[Movable] with TemplateMovable {
    def result(obj: Movable, other: Cell)(implicit ctx: TemplateContext) = {
      if (other.contains(obj.center.get) &&
          almostTheSame(obj.x.next, other.x.get, other.width.get / 5) &&
          almostTheSame(obj.y.next, other.y.get, other.height.get / 5)) {
        val expr = obj.velocity := (other.center - obj.center) * 10
        Some(expr.setPriority(13).setComment(comment(obj, other)))
      } else {
        None
      }
    }
    
    def comment(obj: Movable, other: Cell)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Snap " + obj + " to the center of cell " + other + " by modifying velocity"
  }

  object TArraySnapWithForce extends TemplateOtherCell[Movable] with TemplateMovable {
    def result(obj: Movable, other: Cell)(implicit ctx: TemplateContext) = {
      if (other.contains(obj.center.get) &&
          almostTheSame(obj.x.next, other.x.get, other.width.get / 5) &&
          almostTheSame(obj.y.next, other.y.get, other.height.get / 5)) {
        val expr = ApplyForce(obj, (other.center - obj.center) * 10)
        Some(expr.setPriority(15).setComment(comment(obj, other)))
      } else {
        None
      }
    }

    def comment(obj: Movable, other: Cell)(implicit ctx: TemplateContext) = (c: StringMaker) => 
      c + "Snap " + obj + " to the center of cell " + other + " my modifying acceleration"
  }
  
  object TCreate extends Template[GameObject] {
    /**
     * Compute an expression if the given object applies to this template.
     */
    def apply(obj: GameObject)(implicit ctx: TemplateContext): Option[Expr] = {
      if(obj.creationTime.next == ctx.game.time) {
        obj.plannedFromBeginning match { // TODO: Maybe take into account, if true, that creationTime was infinite before.
          case GameObject.PLANNED_COPY(original) =>
		        Some(copy(ObjectLiteral(original)) { copyId =>
		          copyId
		        });
          case _ => None
        }
      } else {
        None
      }
    }
    
    protected def typeCondition(obj: GameObject): Boolean = true
  }
  
  object TDelete extends Template[GameObject] {
    /**
     * Compute an expression if the given object applies to this template.
     */
    def apply(obj: GameObject)(implicit ctx: TemplateContext): Option[Expr] = {
      if(obj.deletionTime.next == ctx.game.time && obj.deletionTime.get != obj.deletionTime.next) {
        Some(delete(obj))
      } else {
        None
      }
    }
    
    protected def typeCondition(obj: GameObject): Boolean = true
  }

  /** Top template */
  object TShape extends TemplateBlock[GameObject] with TemplateObject {
    def condition(obj: GameObject)(implicit ctx: TemplateContext) = true
    def priority(obj: GameObject)(implicit ctx: TemplateContext) = 10
    val templates = List(
      TCreate,
      TDelete,
      TXY,
      TAngle,
      TVelocity,
      TColor,
      TVisible,
      //IfWidth(TWidth),
      //IfHeight(THeight),
      TValue,
      TText,
      TRadius,
      TBoolean,
      TTime
    )
    //def comment   = s"The changes for shape ${shape.name.next}"
  }

}