package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.objects.GameObject

/***
 * Objects used to disambiguate trees.
 * The following merge mode are supported:
 * - SequentialMerge: Rename the first variable written and replace any occ
 ***/
object Disambiguator {
  import Trees._
  import TreeOps._
  import TreeDSL._

  object MergeMode extends Enumeration {
     type MergeMode = Value
     val SequentialMerge, ForceSecond = Value
  }
  import MergeMode._
  
  type PA[T] = Property[T] with AssignableProperty[T]
  
  /**
   * Returns a tuple (PropertyHavingBeenAssigned, PropertyWithDuplicates)
   */
  def findDuplicates(stat: Expr): (Set[PropertyId], Set[PropertyId]) = {
    TreeOps.foldRight{(s: Expr, subs: Seq[(Set[PropertyId], Set[PropertyId])]) => 
      val (l1, l2) = subs.unzip
      val (assigned, duplicates) = (l1.foldLeft(Set.empty[PropertyId])(_ union _), l2.foldLeft(Set.empty[PropertyId])(_ union _))
      s match {
        case Assign((_, prop), _) =>
          (assigned union Set(prop), duplicates.union(assigned intersect Set(prop)))
        case _ => 
          (assigned, duplicates)
      }
    }(stat)
  }
  
  
  
  implicit class RichMap(m: Map[Property[_], List[Assign]]) {
    def union(other: Map[Property[_], List[Assign]]): Map[Property[_], List[Assign]] = {
      (m /: other) {
        case (m, (key, value)) =>
          if(m contains key) {
            m + (key -> (value ++ m(key)))
          } else {
            m + (key -> value)
          }
      }
    }
    def intersect(other: Map[Property[_], List[Assign]]): Map[Property[_], List[Assign]] = {
      (Map[Property[_], List[Assign]]() /: m) {
        case (m, (key, value)) =>
          if(other contains key) {
            m + (key -> (value ++ other(key)))
          } else {
            m
          }
      }
    }
  }
  
  /**
   * Returns (PropertyHavingBeenAssigned -> Assignments, PropertyDuplicated -> ConflictingAssignments )
   */
  def findDuplicatesMap(t: Expr): (Map[Property[_], List[Assign]], Map[Property[_], List[Assign]]) = {
    t match {
      case ParExpr(l) =>
        val (l1, l2) = (l map findDuplicatesMap).unzip
        (l1 reduceLeft (_ union _), l2 reduceLeft (_ union _))
      case Block(stats) =>
        ((Map[Property[_], List[Assign]](), Map[Property[_], List[Assign]]()) /: stats) {
          case ((assigned, duplicates), stat) =>
            val (newAssigned, newDuplicates) = findDuplicatesMap(stat)
            (assigned union newAssigned, duplicates union (assigned intersect newAssigned))
        }
      case If(cond, ifTrue, UnitLiteral) =>
        findDuplicatesMap(ifTrue)
      case If(cond, ifTrue, ifFalse) =>
        val (trueAssigned, trueDuplicates) = findDuplicatesMap(ifTrue)
        val (falseAssigned, falseDuplicates) = findDuplicatesMap(ifFalse)
        (trueAssigned union falseAssigned, trueDuplicates union falseDuplicates)
      case Foreach(_, _, body) =>
        findDuplicatesMap(body)
//        val duplicatesParallel = f.children.map(findDuplicatesMap)
//        ((Map[Property[_], List[Assign]](), Map[Property[_], List[Assign]]()) /: duplicatesParallel) {
//          case ((assigned, duplicates), (newAssigned, newDuplicates)) =>
//            (assigned union newAssigned, duplicates union newDuplicates)
//        }
        
      case Copy(name, obj, stat) =>
        (Map.empty, Map.empty)
      case a @ Assign(props, expr) =>
        //TODO 
        ???
//        (Map[Property[_], List[Assign]]() ++ (props.collect{ case m: MaybeAssignable if m.isProperty => m.getProperty.get -> List(a) }), Map[Property[_], List[Assign]]())
      
      case Delete(_) =>
        (Map.empty, Map.empty)
      case UnitLiteral =>
        (Map.empty, Map.empty)
      //case Reset(prop) =>
      //  (Map.empty, Map.empty)
    }
  }
  
   // TODO : Check the correctness of assignments
   implicit class RichMapNumber(m: Map[Property[_], List[(Assign, Int)]]) {
    def union(other: Map[Property[_], List[(Assign, Int)]]): Map[Property[_], List[(Assign, Int)]] = {
      (m /: other) {
        case (m, (key, value)) =>
          if(m contains key) {
            m + (key -> (value ++ m(key)))
          } else {
            m + (key -> value)
          }
      }
    }
    def intersect(other: Map[Property[_], List[(Assign, Int)]]): Map[Property[_], List[(Assign, Int)]] = {
      (Map[Property[_], List[(Assign, Int)]]() /: m) {
        case (m, (key, value)) =>
          if(other contains key) {
            m + (key -> (value ++ other(key)))
          } else {
            m
          }
      }
    }
  }
  
  /**
   * Returns (PropertyHavingBeenAssigned -> Assignments with # of conflict, PropertyDuplicated -> ConflictingAssignments with # of conflict (in disorder) )
   */
  /*def 
   * (t: Expr): (Map[Property[_], List[(Assign, Int)]], Map[Property[_], List[(Assign, Int)]]) = {
    t match {
      case Block(stats) =>
        ((Map[Property[_], List[(Assign, Int)]](), Map[Property[_], List[(Assign, Int)]]()) /: stats) {
          case ((assigned, duplicates), stat) =>
            val (newAssigned, newDuplicates) = findDuplicatesMap(stat)
            (assigned union newAssigned, duplicates union (assigned intersect newAssigned))
        }
      case If(cond, ifTrue, ifFalse) =>
        val (trueAssigned, trueDuplicates) = findDuplicatesMap(ifTrue)
        val (falseAssigned, falseDuplicates) = findDuplicatesMap(ifFalse)
        (trueAssigned union falseAssigned, trueDuplicates union falseDuplicates)
      case f@Foreach1(category, name, rule) =>
        val duplicatesParallel = f.children.map(findDuplicatesMap)
        ((Map[Property[_], List[(Assign, Int)]](), Map[Property[_], List[(Assign, Int)]]()) /: duplicatesParallel) {
          case ((assigned, duplicates), (newAssigned, newDuplicates)) =>
            (assigned union newAssigned, duplicates union newDuplicates)
        }
      case Copy(name, obj, stat) =>
        // Expr cannot be evaluated at this time
        (Map[Property[_], List[(Assign, Int)]](), Map[Property[_], List[(Assign, Int)]]())
      case a @ Assign(props, expr) =>
        (Map[Property[_], List[(Assign, Int)]]() ++ (props.collect{ case m: MaybeAssignable if m.isProperty => m.getProperty.get -> List(a) }), Map[Property[_], List[(Assign, Int)]]())
      case Delete(name, o) =>
        (Map[Property[_], List[(Assign, Int)]](), Map[Property[_], List[(Assign, Int)]]())
      case NOP =>
        (Map[Property[_], List[(Assign, Int)]](), Map[Property[_], List[(Assign, Int)]]())
      case Reset(prop) =>
        (Map[Property[_], List[(Assign, Int)]](), Map[Property[_], List[(Assign, Int)]]())
  //    (Map[Property[_], List[(Assign, Int)]]() ++ (prop.getProperty -> , Map[Property[_], List[(Assign, Int)]]())
    }
  }*/
  
  /**
   * Ambiguity if a path in the interpretation assigns two times the same variable (it means that there is a paradox)
   * For example:
   * x' = x + 6
   * x' = x * 6
   * or with nested if-then-else statements.
   * if A:
   *   x' = x + 5  =>  x1 = x + 5
   *   x' = x * 2  =>  x2 = x1 * 2
   * else            else
   *   x' = x*3      //x1 = x
   *                 //x2 = x1
   *                   x2 = x * 3
   * if B:
   *   x' = x + 10 =>  x' = x2 + 10
   *                 else
   *                   x' = x2
   */
  def apply(t: Expr)(implicit interface: PropertyId => MergeMode)= {
    val (_, duplicates) = findDuplicates(t)
    (t /: duplicates) { case (tree, prop) =>
      val mergeMode = interface(prop)
      modifyCode(tree, UnitLiteral, prop, mergeMode) // TODO : Replace UnitLiteral by the expression corresponding to prop
    }
  }
  
  /**
   * Interface returns a list of pair of assignments, where the first assign should replace the old one,
   * and the second assigned should be applied in the else section of the containing IF if it exists. 
   */
  def modifyCode(t: Expr, pe: Expr, p: PropertyId, mode: MergeMode): Expr = {
    mode match {
      case MergeMode.SequentialMerge =>
        modifyCodeSequential(t, (pe, p))
      case MergeMode.ForceSecond =>
        modifyCodeForceSecond(t, p)
    }
  }
  
  def modifyCodeForceSecond[T](t: Expr, p_original: PropertyId): Expr = {
    t // Not implemented yet/
  }
  
  /**
   * Sequential substitution
   */
  def modifyCodeSequential[T](t: Expr, p_original: (Expr, PropertyId)): Expr = {
    // Retrieve a property's number. x => 0, x1 => 1, x2 => 2 ...
    def numProperty(p: PropertyId): Int = {
      p match {
        case GameObject.EphemeralEndings(prefix, num) => num.toInt
        case _ => 0
      }
    }
    // Retrieve a property given a number and the original: 0 => x, 1 => x1, 2 => x2, ....
    def propertyNumGet(p: PropertyId, num: Int): PropertyId = {
      val prefixName = p match {
        case GameObject.EphemeralEndings(prefix, num) => prefix
        case _ => p
      }
      val requestedName = prefixName + num
      
      val currentProperty: PropertyId = ??? // TODO : Recover the object. p.parent.getEphemeralProperty(requestedName).asInstanceOf[Option[PropertyId]]
      currentProperty//.getOrElse(p.copyEphemeral(requestedName))
    }
    
    // Retrieve the successor of a property.
    def newProp(p: PropertyId): PropertyId = {
      propertyNumGet(p, numProperty(p) + 1)
    }
    
    // do it in reverse
    def rec(stat: Expr, p: PropertyId, replaceAssigned: (Expr, PropertyId), replaceEvaluated: (Expr, PropertyId)): (Expr, (Expr, PropertyId), (Expr, PropertyId)) = {
      stat match {
        case ParExpr(l) =>
          val pars = l map { a => rec(a, p, replaceAssigned, replaceEvaluated) } // Should all be the same
          pars match {
            case Nil => (UnitLiteral, replaceAssigned, replaceEvaluated)
            case a::Nil => a
            case a::q => (ParExpr(pars.map(_._1)), a._2, a._3)
          }
        case Block(stats) =>
          val (stats2, res1, res2) = ((List[Expr](), replaceAssigned, replaceEvaluated) /: stats.reverse) { case ((statList, r1, r2), newStat) =>
            val (s, rr1, rr2) = rec(newStat, p, r1, r2)
            (s::statList, rr1, rr2)
          }
          (Block(stats2), res1, res2)
        case If(cond, ifTrue, UnitLiteral) =>
          val (ifTrue2, itr, ite) = rec(ifTrue, p, replaceAssigned, replaceEvaluated)
          val (ifFalse2, ifr, ife) = (UnitLiteral, replaceAssigned, replaceEvaluated)
          if(itr == ifr && ite == ife) { // Ifs are balanced.
            (If(cond, ifTrue2, UnitLiteral), itr, ite)
          } else { // Need to introduce new variables
            val i = numProperty(itr._2)
            val j = numProperty(ifr._2)
            if(i < j) { // More variables in ifFalse2
              /* Case 
               * if A:
               *   itr  ite
               *   x1 = x2 + 3  (i == 1)   => need to pre-add the assignment  x2 = x3  (ite = ife)
               * else
               *   ifr  ife
               *   x2 = x3 + 6  (i == 2)
               *   x1 = x2 * 3
               */
              (If(cond, Assign(ite, ife._1) :: ifTrue2, ifFalse2), ifr, ife)
            } else if(i > j) {
              /* Case 
               * if A:
               *   itr  ite
               *   x2 = x3 + 6  (i == 2)
               *   x1 = x2 + 3  (i == 1)
               * else
               *   ifr  ife      => need to pre-add the assignment  x2 = x3  (ife = ite)
               *   x1 = x2 * 3
               */
              (If(cond, ifTrue2, Assign(ife, ite._1)), itr, ite)
            } else {
              throw new Exception("Should not arrive here: property $ifTrueReplaced is not equal to $ifFalseReplaced but have the same number.")
            }
          }
        case If(cond, ifTrue, ifFalse) =>
          val (ifTrue2, itr, ite) = rec(ifTrue, p, replaceAssigned, replaceEvaluated)
          val (ifFalse2, ifr, ife) = rec(ifFalse, p, replaceAssigned, replaceEvaluated)
          if(itr == ifr && ite == ife) { // Ifs are balanced.
            (If(cond, ifTrue2, ifFalse2), itr, ite)
          } else { // Need to introduce new variables
            val i = numProperty(itr._2)
            val j = numProperty(ifr._2)
            if(i < j) { // More variables in ifFalse2
              /* Case 
               * if A:
               *   itr  ite
               *   x1 = x2 + 3  (i == 1)   => need to pre-add the assignment  x2 = x3  (ite = ife)
               * else
               *   ifr  ife
               *   x2 = x3 + 6  (i == 2)
               *   x1 = x2 * 3
               */
              (If(cond, Assign(ite, ife._1) :: ifTrue2, ifFalse2), ifr, ife)
            } else if(i > j) {
              /* Case 
               * if A:
               *   itr  ite
               *   x2 = x3 + 6  (i == 2)
               *   x1 = x2 + 3  (i == 1)
               * else
               *   ifr  ife      => need to pre-add the assignment  x2 = x3  (ife = ite)
               *   x1 = x2 * 3
               */
              (If(cond, ifTrue2, Assign(ife, ite._1) :: ifFalse2), itr, ite)
            } else {
              throw new Exception("Should not arrive here: property $ifTrueReplaced is not equal to $ifFalseReplaced but have the same number.")
            }
          }
        case f @ Foreach(_, _, body) =>
          val (s, newAssigned, newEvaluated) = rec(stat, p, replaceAssigned, replaceEvaluated)
          if(newAssigned != replaceAssigned) // There has been a replacement with if-like structure. Keep it
            (f.copy(body = s), newAssigned, newEvaluated)
          else
            (t, replaceAssigned, replaceEvaluated) // No replacement
            
        case Copy(name, obj, stat) =>
          (t, replaceAssigned, replaceEvaluated) // No replacement for the moment.
        case a @ Assign(props, expr) =>
          //  x' = f(x, ...) => replaced by xReplacedAssigned = f(xReplaceEvaluated)
          // where the two variables are shifted from the previous assignments
          // If x' is hard-coded, we replace it normally
          // If x' is soft-coded, we add an If condition.
          
          //TODO
          ???
//          val (assign, npa, npe) = if(props exists { case p => p.getProperty == Some(p_original) }) {
//            val newPropAssigned = replaceEvaluated
//            val newPropEvaluated = newProp(replaceEvaluated)
//            val res = Assign(props.map{
//              case p if p.getProperty == Some(p_original) => newPropAssigned.expr
//              case p => p
//            }, expr.replace(p_original, newPropEvaluated))
//            (res, newPropAssigned, newPropEvaluated)
//          } else {
//            (Assign(props, expr.replace(p_original, replaceEvaluated)), replaceAssigned, replaceEvaluated)
//          }
//          props find {case p => p.getProperty == Some(p_original)} match {
//            case Some(p: PropertyIndirect) =>
//              //val exprReplaced = expr.replace(p_original, replaceEvaluated)
//              (If(p.indirectObject =:= p_original.parent.expr,
//                  assign,
//                  Some(Assign(props, expr))), npa, npe)
//            case _ =>
//              (assign, npa, npe)
//          }
        
        case Delete(_) =>
          (t, replaceAssigned, replaceEvaluated)
        case UnitLiteral =>
          (t, replaceAssigned, replaceEvaluated)
        //case Reset(_) =>
         // (t, replaceAssigned, replaceEvaluated)
      }
    }
    val (result, replaceAssigned, replaceEvaluated) = rec(t, p_original._2, p_original, p_original)
    Assign(replaceEvaluated, p_original._1)::result
  }
}