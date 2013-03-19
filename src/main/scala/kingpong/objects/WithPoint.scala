package ch.epfl.lara.synthesis.kingpong.objects

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._

trait WithPoint extends Any {
  
  /** The main point of this object. */
  protected def point: Vec2
 
  /** The squared Euclidean distance. */
  def squaredDistance(to: Vec2): Float = {
    val dx = to.x - point.x
    val dy = to.y - point.y
    dx*dx + dy*dy
  }
  
  /** The Euclidean distance. */
  def distance(to: Vec2): Float = math.sqrt(squaredDistance(to)).toFloat
  
}