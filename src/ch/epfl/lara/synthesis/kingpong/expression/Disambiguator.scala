package ch.epfl.lara.synthesis.kingpong.expression

import scala.collection.mutable.{HashMap => MMap}
import ch.epfl.lara.synthesis.kingpong.objects.Property
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import ch.epfl.lara.synthesis.kingpong.objects.GameObject

/***
 * Objects used to disambiguate trees.
 * The following merge mode are supported:
 * - SequentialMerge: Rename the first variable written and replace any occ
 ***/
object Disambiguator {
  import Trees._
  import TraverseMode.{Value => _, _}
  object MergeMode extends Enumeration {
     type MergeMode = Value
     val SequentialMerge, ForceSecond = Value
  }
  import MergeMode._
  
  /**
   * Returns a tuple (PropertyHavingBeenAssigned, PropertyWithDuplicates)
   */
  def findDuplicates(t: Stat): (Set[Property[_]], Set[Property[_]]) = {
    t match {
      case ParExpr(l) =>
        val (l1, l2) = (l map findDuplicates).unzip
        (l1 reduceLeft (_ union _), l2 reduceLeft (_ union _))
      case Block(stats) =>
        ((Set[Property[_]](), Set[Property[_]]()) /: stats) {
          case ((assigned, duplicates), stat) =>
            val (newAssigned, newDuplicates) = findDuplicates(stat)
            (assigned union newAssigned, duplicates.union(assigned intersect newAssigned))
        }
      case If(cond, ifTrue, None) =>
        val (trueAssigned, trueDuplicates) = findDuplicates(ifTrue)
        //val (falseAssigned, falseDuplicates) = findDuplicates(ifFalse)
        (trueAssigned, trueDuplicates)
      case If(cond, ifTrue, Some(ifFalse)) =>
        val (trueAssigned, trueDuplicates) = findDuplicates(ifTrue)
        val (falseAssigned, falseDuplicates) = findDuplicates(ifFalse)
        (trueAssigned union falseAssigned, trueDuplicates union falseDuplicates)
      case f@Foreach1(category, name, rule) =>
        val duplicatesParallel = f.children.map(findDuplicates)
        ((Set[Property[_]](), Set[Property[_]]()) /: duplicatesParallel) {
          case ((assigned, duplicates), (newAssigned, newDuplicates)) =>
            (assigned union newAssigned, duplicates union newDuplicates)
        }
      case Copy(name, obj, stat) =>
        // Stat cannot be evaluated at this time
        (Set[Property[_]](), Set[Property[_]]())
      case Assign(props, expr) =>
        (Set[Property[_]]() ++ (props.collect{ case m: MaybeAssignable if m.isProperty => m.getProperty.get }), Set[Property[_]]())
      case Delete(name, o) =>
        (Set[Property[_]](), Set[Property[_]]())
      case NOP =>
        (Set[Property[_]](), Set[Property[_]]())
      case Reset(prop) =>
        (Set[Property[_]]() ++ prop.getProperty, Set[Property[_]]())
    }
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
  def findDuplicatesMap(t: Stat): (Map[Property[_], List[Assign]], Map[Property[_], List[Assign]]) = {
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
      case If(cond, ifTrue, None) =>
        val (trueAssigned, trueDuplicates) = findDuplicatesMap(ifTrue)
        //val (falseAssigned, falseDuplicates) = findDuplicatesMap(ifFalse)
        (trueAssigned, trueDuplicates)
      case If(cond, ifTrue, Some(ifFalse)) =>
        val (trueAssigned, trueDuplicates) = findDuplicatesMap(ifTrue)
        val (falseAssigned, falseDuplicates) = findDuplicatesMap(ifFalse)
        (trueAssigned union falseAssigned, trueDuplicates union falseDuplicates)
      case f@Foreach1(category, name, rule) =>
        val duplicatesParallel = f.children.map(findDuplicatesMap)
        ((Map[Property[_], List[Assign]](), Map[Property[_], List[Assign]]()) /: duplicatesParallel) {
          case ((assigned, duplicates), (newAssigned, newDuplicates)) =>
            (assigned union newAssigned, duplicates union newDuplicates)
        }
      case Copy(name, obj, stat) =>
        // Stat cannot be evaluated at this time
        (Map[Property[_], List[Assign]](), Map[Property[_], List[Assign]]())
      case a @ Assign(props, expr) =>
        (Map[Property[_], List[Assign]]() ++ (props.collect{ case m: MaybeAssignable if m.isProperty => m.getProperty.get -> List(a) }), Map[Property[_], List[Assign]]())
      case Delete(name, o) =>
        (Map[Property[_], List[Assign]](), Map[Property[_], List[Assign]]())
      case NOP =>
        (Map[Property[_], List[Assign]](), Map[Property[_], List[Assign]]())
      case Reset(prop) =>
        (Map[Property[_], List[Assign]](), Map[Property[_], List[Assign]]())
  //    (Map[Property[_], List[Assign]]() ++ (prop.getProperty -> , Map[Property[_], List[Assign]]())
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
   * (t: Stat): (Map[Property[_], List[(Assign, Int)]], Map[Property[_], List[(Assign, Int)]]) = {
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
        // Stat cannot be evaluated at this time
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
  def apply(t: Stat)(implicit interface: Property[_] => MergeMode)= {
    val (_, duplicates) = findDuplicates(t)
    (t /: duplicates) { case (tree, prop) =>
      val mergeMode = interface(prop)
      modifyCode(tree, prop, mergeMode)
    }
  }
  
  /**
   * Interface returns a list of pair of assignments, where the first assign should replace the old one,
   * and the second assigned should be applied in the else section of the containing IF if it exists. 
   */
  def modifyCode(t: Stat, p: Property[_], mode: MergeMode): Stat = {
    mode match {
      case MergeMode.SequentialMerge =>
        modifyCodeSequential(t, p)
      case MergeMode.ForceSecond =>
        modifyCodeForceSecond(t, p)
    }
  }
  
  def modifyCodeForceSecond[T](t: Stat, p_original: Property[T]): Stat = {
    t // Not implemented yet/
  }
  
  /**
   * Sequential substitution
   */
  def modifyCodeSequential[T](t: Stat, p_original: Property[T]): Stat = {
    // Retrieve a property's number. x => 0, x1 => 1, x2 => 2 ...
    def numProperty(p: Property[T]): Int = {
      p.name match {
        case GameObject.EphemeralEndings(prefix, num) => num.toInt
        case _ => 0
      }
    }
    // Retrieve a property given a number and the original: 0 => x, 1 => x1, 2 => x2, ....
    def propertyNumGet(p: Property[T], num: Int): Property[T] = {
      val prefixName = p.name match {
        case GameObject.EphemeralEndings(prefix, num) => prefix
        case _ => p.name
      }
      val requestedName = prefixName + num
      
      val currentProperty = p.parent.getEphemeralProperty(requestedName).asInstanceOf[Option[Property[T]]]
      currentProperty.getOrElse(p.copyEphemeral(requestedName))
    }
    
    // Retrieve the successor of a property.
    def newProp(p: Property[T]): Property[T] = {
      propertyNumGet(p, numProperty(p) + 1)
    }
    
    // do it in reverse
    def rec(stat: Stat, p: Property[T], replaceAssigned: Property[T], replaceEvaluated: Property[T]): (Stat, Property[T], Property[T]) = {
      stat match {
        case ParExpr(l) =>
          val pars = l map { a => rec(a, p, replaceAssigned, replaceEvaluated) } // Should all be the same
          pars match {
            case Nil => (NOP, replaceAssigned, replaceEvaluated)
            case a::Nil => a
            case a::q => (ParExpr(pars.map(_._1)), a._2, a._3)
          }
        case Block(stats) =>
          val (stats2, res1, res2) = ((List[Stat](), replaceAssigned, replaceEvaluated) /: stats.reverse) { case ((statList, r1, r2), newStat) =>
            val (s, rr1, rr2) = rec(newStat, p, r1, r2)
            (s::statList, rr1, rr2)
          }
          (Block(stats2), res1, res2)
        case If(cond, ifTrue, None) =>
          val (ifTrue2, itr, ite) = rec(ifTrue, p, replaceAssigned, replaceEvaluated)
          val (ifFalse2, ifr, ife) = (None, replaceAssigned, replaceEvaluated)
          if(itr == ifr && ite == ife) { // Ifs are balanced.
            (If(cond, ifTrue2, None), itr, ite)
          } else { // Need to introduce new variables
            val i = numProperty(itr)
            val j = numProperty(ifr)
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
              (If(cond, Assign(List(ite.expr), ife.expr) :: ifTrue2, ifFalse2), ifr, ife)
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
              (If(cond, ifTrue2, Some(Assign(List(ife.expr), ite.expr))), itr, ite)
            } else {
              throw new Exception("Should not arrive here: property $ifTrueReplaced is not equal to $ifFalseReplaced but have the same number.")
            }
          }
        case If(cond, ifTrue, Some(ifFalse)) =>
          val (ifTrue2, itr, ite) = rec(ifTrue, p, replaceAssigned, replaceEvaluated)
          val (ifFalse2, ifr, ife) = rec(ifFalse, p, replaceAssigned, replaceEvaluated)
          if(itr == ifr && ite == ife) { // Ifs are balanced.
            (If(cond, ifTrue2, Some(ifFalse2)), itr, ite)
          } else { // Need to introduce new variables
            val i = numProperty(itr)
            val j = numProperty(ifr)
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
              (If(cond, Assign(List(ite.expr), ife.expr) :: ifTrue2, Some(ifFalse2)), ifr, ife)
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
              (If(cond, ifTrue2, Some(Assign(List(ife.expr), ite.expr) :: ifFalse2)), itr, ite)
            } else {
              throw new Exception("Should not arrive here: property $ifTrueReplaced is not equal to $ifFalseReplaced but have the same number.")
            }
          }
        case f@Foreach1(category, name, rule) =>
          f.children.foreach { stat =>
            val (s, newAssigned, newEvaluated) = rec(stat, p, replaceAssigned, replaceEvaluated)
            if(newAssigned != replaceAssigned) // There has been a replacement with if-like structure. Keep it
              return (Foreach1(category, name, s), newAssigned, newEvaluated)
          }
          (t, replaceAssigned, replaceEvaluated) // No replacement
        case Copy(name, obj, stat) =>
          (t, replaceAssigned, replaceEvaluated) // No replacement for the moment.
        case a @ Assign(props, expr) =>
          //  x' = f(x, ...) => replaced by xReplacedAssigned = f(xReplaceEvaluated)
          // where the two variables are shifted from the previous assignments
          // If x' is hard-coded, we replace it normally
          // If x' is soft-coded, we add an If condition.
          val (assign, npa, npe) = if(props exists { case p => p.getProperty == Some(p_original) }) {
            val newPropAssigned = replaceEvaluated
            val newPropEvaluated = newProp(replaceEvaluated)
            val res = Assign(props.map{
              case p if p.getProperty == Some(p_original) => newPropAssigned.expr
              case p => p
            }, expr.replace(p_original, newPropEvaluated))
            (res, newPropAssigned, newPropEvaluated)
          } else {
            (Assign(props, expr.replace(p_original, replaceEvaluated)), replaceAssigned, replaceEvaluated)
          }
          props find {case p => p.getProperty == Some(p_original)} match {
            case Some(p: PropertyIndirect) =>
              //val exprReplaced = expr.replace(p_original, replaceEvaluated)
              (If(p.indirectObject =:= p_original.parent.expr,
                  assign,
                  Some(Assign(props, expr))), npa, npe)
            case _ =>
              (assign, npa, npe)
          }
        case Delete(name, o) =>
          (t, replaceAssigned, replaceEvaluated)
        case NOP =>
          (t, replaceAssigned, replaceEvaluated)
        case Reset(prop) =>
          (t, replaceAssigned, replaceEvaluated)
      }
    }
    val (result, replaceAssigned, replaceEvaluated) = rec(t, p_original, p_original, p_original)
    Assign(List(replaceEvaluated.expr), p_original.expr)::result
  }
}