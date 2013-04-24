package ch.epfl.lara.synthesis.kingpong.objects

trait Timed extends Any {

  /** Do a snapshot. */
  def snapshot(): Unit
  
  /** Revert to the latest snapshot. */
  def revert(): Unit
  
  /** Save the curent value to the history with the specified 
   *  discrete time.
   */
  def save(t: Long): Unit

  /** Restore the value from the specified discrete time. */
  def restore(t: Long): Unit

  /** Destroy the history. */
  def clear(): Unit
}