package ch.epfl.lara.synthesis.kingpong.objects

import scala.collection.mutable.{Set => MSet, Map => MMap}

import ch.epfl.lara.synthesis.kingpong.expression.Trees._

class Category(name: String) { self =>

  private val _children = MSet.empty[GameObject]
  
  private[kingpong] val properties = MMap.empty[String, PropertyRef]
  
  def add(o: GameObject): self.type = {
    _children += o
    resolveProperties()
    this
  }

  def remove(o: GameObject): self.type = {
    _children -= o
    resolveProperties()
    this
  }

  def apply(property: String): PropertyRef = properties(property)

  private def resolveProperties() {
    properties.clear()
    val keys = _children.map(_.properties.keySet).foldLeft(Set.empty[String]) {_ intersect _}
    properties ++= keys.map { k =>
      val p = _children.map(_.properties).foldLeft(Set.empty[Property[_]]){_ + _(k)}
      k -> CategoryPropertyRef(p)
    }
  }
}