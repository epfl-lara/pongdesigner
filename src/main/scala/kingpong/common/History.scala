package ch.epfl.lara.synthesis.kingpong.common

trait History extends Any {  
  /** Save the curent value to the history with the specified 
   *  discrete time.
   */
  def save(t: Long): Unit

  /** Restore the value from the specified discrete time. */
  def restore(t: Long): Unit

  /** Destroy the history. */
  def clear(): Unit
}