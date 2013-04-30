package ch.epfl.lara.synthesis.kingpong.common

trait Snap extends Any {  
    /** Do a snapshot. */
  def snapshot(): Unit
  
  /** Revert to the latest snapshot. */
  def revert(): Unit
}
