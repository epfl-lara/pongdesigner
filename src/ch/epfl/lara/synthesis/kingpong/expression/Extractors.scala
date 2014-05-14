package ch.epfl.lara.synthesis.kingpong.expression

object Extractors {
  import Trees._
  
  object UnaryOperator {
    def unapply(expr: Expr): Option[(Expr, (Expr) => Expr)] = expr match {
      case Not(e) => Some((e, Not))
      case Row(e) => Some((e, Row))
      case Column(e) => Some((e, Column))
      case IsFingerDownOver(e) => Some((e, IsFingerDownOver))
      case IsFingerUpOver(e)   => Some((e, IsFingerUpOver))
      case TupleSelect(e, i) => Some((e, TupleSelect(_, i)))
      case Choose(vars, e)   => Some((e, Choose(vars, _)))
      case Select(t1, p)   => Some((t1, Select(_, p)))
      
      case Foreach(cat, id, e) => Some((e, Foreach(cat, id, _)))
      case Forall(cat, id, e)  => Some((e, Forall(cat, id, _)))
      case Find(cat, id, e)    => Some((e, Find(cat, id, _)))
      case Delete(e) => Some((e, Delete))
      
      case _ => None
    }
  }
  
  object BinaryOperator {
    def unapply(expr: Expr): Option[(Expr, Expr, (Expr, Expr) => Expr)] = expr match {
      case Equals(t1, t2) => Some((t1, t2, Equals))
      case Plus(t1, t2)   => Some((t1, t2, Plus))
      case Minus(t1, t2)  => Some((t1, t2, Minus))
      case Times(t1, t2)  => Some((t1, t2, Times))
      case Div(t1, t2)    => Some((t1, t2, Div))
      case Mod(t1, t2)    => Some((t1, t2, Mod))
      
      case LessThan(t1, t2)    => Some((t1, t2, LessThan))
      case GreaterThan(t1, t2) => Some((t1, t2, GreaterThan))
      case LessEq(t1, t2)      => Some((t1, t2, LessEq))
      case GreaterEq(t1, t2)   => Some((t1, t2, GreaterEq))
      case And(t1, t2)         => Some((t1, t2, And))
      case Or(t1, t2)          => Some((t1, t2, Or))
      
      case Collision(t1, t2) => Some((t1, t2, Collision))
      case Contains(t1, t2)  => Some((t1, t2, Contains))
      case Let(id, t1, t2)   => Some((t1, t2, Let(id, _, _)))
      case Copy(t1, id, t2)  => Some((t1, t2, Copy(_, id, _)))
      case ApplyForce(t1, t2)     => Some((t1, t2, ApplyForce))
      case Assign((t1, prop), t2) => Some((t1, t2, (e1: Expr, e2: Expr) => Assign((e1, prop), e2)))
      case ContainingCell(t1, t2) => Some((t1, t2, ContainingCell))
      
      case FingerMoveOver(t1, id, t2) => Some((t1, t2, FingerMoveOver(_, id, _)))
      case FingerUpOver(t1, id, t2) => Some((t1, t2, FingerUpOver(_, id, _)))
      case FingerDownOver(t1, id, t2) => Some((t1, t2, FingerDownOver(_, id, _)))

      case _ => None
    }
  }
  
  trait Builder[T <: Expr] {
    def apply(as: Seq[Expr]): T
  }
  implicit val BuilderIf = new Builder[If] { def apply(as: Seq[Expr]) = If(as(0), as(1), as(2))}
  implicit val BuilderBlock = new Builder[Block] { def apply(as: Seq[Expr]) = Block(as) }
  
  object NAryOperator {
    def unapply(expr: Expr): Option[(Seq[Expr], (Seq[Expr]) => Expr)] = expr match {
      case Tuple(args) => Some((args, (as: Seq[Expr]) => Tuple(as)))
      case If(t1, t2, t3) => Some((Seq(t1, t2, t3), BuilderIf(_)))
      case Apply(t1, t2, t3) => Some((Seq(t1, t2, t3), (as: Seq[Expr]) => Apply(as(0), as(1), as(2))))
      
      case Block(exprs) => Some((exprs, (as: Seq[Expr]) => Block(as)))
      case ParExpr(exprs) => Some((exprs, (as: Seq[Expr]) => ParExpr(as.toList)))
      case MethodCall(n, l) => Some((l, MethodCall(n, _)))

      case _ => None
    }
  }
  
}