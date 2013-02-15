package ch.epfl.lara.synthesis.kingpong.ast

import scala.collection.mutable.Set
import scala.collection.Traversable
import ch.epfl.lara.synthesis.kingpong.GameShapes
import ch.epfl.lara.synthesis.kingpong.GameShapes._

/**
 * Defines a constructor to easily define categories.
 */
object Category {
  def apply(vargs : GameShapes.Shape*): Category[Shape] = {
    val res = new BasicCategory
    res.content.++=(vargs)
    res
  }
}

trait InstanceSpawner {
  
}


/**
 * Describes a traversable and named set of shapes.
 */
trait Category[T] extends Traversable[T] with InstanceSpawner with GameShapes.NamedObject {
  var content = Set[T]()
  def foreach[U](f: T => U) = {
    content foreach f
  }
   def turnOnOff(s: T) = {
    if(content contains s) {
      remove(s)
    } else {
      add(s)
    }
  }
   def remove(s: T): Unit = {
     content -= s
   }
   def add(s: T): Unit = {
     content += s
   }
   def +=(s: T): Unit = {
     add(s)
   }
   def reset(): Unit = {
     content = Set[T]()
   }
   override def size: Int = content.size
   var size_prev: Int = 0
   
   def copyToPrev() {
     size_prev = size
   }
}



class BasicCategory extends Category[GameShapes.Shape]