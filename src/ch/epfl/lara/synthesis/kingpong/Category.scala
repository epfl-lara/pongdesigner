package ch.epfl.lara.synthesis.kingpong

import scala.collection.mutable.Set
import scala.collection.Traversable

/**
 * Defines a constructor to easily define categories.
 */
object Category {
  def apply(vargs : GameShapes.Shape*): Category = {
    val res = new Category
    res.content.++=(vargs)
    res
  }
}

/**
 * Describes a traversable and named set of shapes.
 */
class Category extends Traversable[GameShapes.Shape] {
  var content = Set[GameShapes.Shape]()
  var name: String = null
  def foreach[U](f: GameShapes.Shape => U) = {
    content foreach f
  }
  def named(s: String): Category = {
    name = s
    this
  }
   def turnOnOff(s: GameShapes.Shape) = {
    if(content contains s) {
      remove(s)
    } else {
      add(s)
    }
  }
   def remove(s: GameShapes.Shape): Unit = {
     content -= s
   }
   def add(s: GameShapes.Shape): Unit = {
     content += s
   }
   def reset(): Unit = {
     content = Set[GameShapes.Shape]()
   }
}