package ch.epfl.lara.synthesis.kingpong.objects

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._

trait WithPoint extends Any {
  
  /** The main point of this object. */
  protected def point: Vec2
 
}