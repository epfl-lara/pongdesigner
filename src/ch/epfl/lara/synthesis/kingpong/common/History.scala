package ch.epfl.lara.synthesis.kingpong.common

object History {
  val MAX_HISTORY_SIZE = 300
}

trait History extends Any {  
  /** Save the current value to the history with the specified 
   *  discrete time.
   */
  def save(t: Long): Unit

  /** 
   *  Restore the value from the specified discrete time. 
   *  @param t the restoration time.
   *  @param clear if `true`, all the history after `t` is removed.
   */
  def restore(t: Long, clear: Boolean): Unit

  /** Destroy the history. */
  def clear(): Unit
}