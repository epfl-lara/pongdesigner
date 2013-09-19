package ch.epfl.lara.synthesis.kingpong.expression

import scala.collection.mutable.{HashMap => MMap}
import ch.epfl.lara.synthesis.kingpong.objects.Property
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

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
      case If(cond, ifTrue, ifFalse) =>
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
      case If(cond, ifTrue, ifFalse) =>
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
  /*def findDuplicatesMapNumber(t: Stat): (Map[Property[_], List[(Assign, Int)]], Map[Property[_], List[(Assign, Int)]]) = {
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
  def apply(t: Stat) = {
    // Assume the format For(if(...)else(...))
    val alreadyAssigned = MMap[Property[_], List[Assign]]()
    val duplicationProblems = MMap[Property[_], List[Assign]]() // subset of alreadyAssign where values are of size 2 or more.
    
    def addPropertyAssigned(prop: Property[_], newAssign: Assign) = {
      if(alreadyAssigned contains prop) {
        alreadyAssigned(prop) = newAssign::alreadyAssigned.getOrElse(prop, Nil)
        duplicationProblems(prop) = alreadyAssigned(prop)
      } else {
        alreadyAssigned(prop) = newAssign::alreadyAssigned.getOrElse(prop, Nil)
      }
    }
    // TODO : use the findDuplicatesMap to solve the ambiguities.
    
    // Find in order which properties are being written over others.
    t.traverse{ _ match {
      case a@Assign(propertyreflist, expr) => 
        propertyreflist foreach {
          case PropertyRef(prop) =>
            addPropertyAssigned(prop, a)
          case p@PropertyIndirect(name, obj, prop) =>
            p.expr match {
              case PropertyRef(prop) => 
                addPropertyAssigned(prop, a)
              case _ => // Nothing to handle
            }
          case _ =>
        }
        ContinueSiblings
      case _  => 
        ContinueWithChildren
    }}
    /*duplicationProblems.foreach {
      case (p, (a::b::Nil)) =>
        
      case (p, (a::b::q)) =>
        
      case _ =>
        
    }*/
  }
  
  /**
   * Interface returns a list of pair of assignments, where the first assign should replace the old one,
   * and the second assigned should be applied in the else section of the containing IF if it exists. 
   */
  /*def modifyCode(t: Stat, duplicationProblems: MMap[Property[_], List[Assign]],
      interface: (Property[_], List[Assign]) => MergeMode)
  ): Stat = {
    var tree = t
    duplicationProblems.foreach { problem =>
      val (prop, oldAssigns) = problem
      val (newAssigns, mergeMode) = interface.tupled(problem)
      val mapAssigns = (oldAssigns zip newAssigns).toMap
      /**
       * Returns the replaced tree, and a list of states to add to any branches which can be dependent.
       */
      /*def replace(t: Tree)(implicit map: Map[Assign, (Assign, Assign)]): (Tree, List[Stat]) = t match {
        case Block(stats) =>
          val res = stats.map(replace(_))
          
        case If(cond, ifTrue, ifFalse) =>
          // Cond does not change. ifTrue might.
          val (newIfTrue, toPutInIfFalse) = replace(ifTrue)
          val (newIfFalse, toPutInIfTrue) = replace(ifFalse)
          
          If(cond, newIfTrue, newIfFalse)
          
          (If(cond, ifTrueReplaced, ifFalseReplaced), listFalse, ContinueSiblings)
        case a@Assign(props, expr) => if(mapAssigns contains a) {
          val (ifTrueAssign, ifFalseAssign) = mapAssigns(a)
          
          (ifTrueAssign, List(ifFalseAssign), ContinueSiblings)
          //replace with ifTrueAssign
          //return ifFalseAssign to add to all parent 'else' statements.
          
        } else {
          (a, Nil, ContinueSiblings)
        }
        ContinueWithChildren
        case e =>
          (e, Nil, ContinueWithChildren) // Return this expression
          ContinueWithChildren
      }
      tree = tree.traverseReplace(replace _)*/
    }
    tree
  }*/
}