package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Types._

object TypeOps {

  def leastUpperBound(t1: Type, t2: Type): Option[Type] = (t1,t2) match {
    case (TTuple(args1), TTuple(args2)) =>
      val args = (args1 zip args2).map(p => leastUpperBound(p._1, p._2))
      if (args.forall(_.isDefined)) Some(TTuple(args.map(_.get))) else None
    case (o1, o2) if (o1 == o2) => Some(o1)
    
    // Accept integer as float
    case (TInt, TFloat) => Some(TFloat)
    case (TFloat, TInt) => Some(TFloat)
    case (o1,TAny) => Some(TAny)
    case (TAny,o2) => Some(TAny)

//    case (o1,BottomType) => Some(o1)
//    case (BottomType,o2) => Some(o2)
    
    case _ => None
  }

  def leastUpperBound(ts: Seq[Type]): Option[Type] = {
    def olub(ot1: Option[Type], t2: Option[Type]): Option[Type] = ot1 match {
      case Some(t1) => leastUpperBound(t1, t2.get)
      case None => None
    }

    if (ts.isEmpty) {
      None
    } else {
      ts.map(Some(_)).reduceLeft(olub)
    }
  }

  def isSubtypeOf(t1: Type, t2: Type): Boolean = {
    leastUpperBound(t1, t2) == Some(t2)
  }

  def typeCheck(obj: Expr, exps: Type*) {
    val res = exps.exists(e => isSubtypeOf(obj.getType, e))
    if (!res) {
      throw TypeErrorException(obj, exps.toList)
    }
  }

}