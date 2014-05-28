package ch.epfl.lara.synthesis.kingpong.common

object History {
  val MAX_HISTORY_SIZE = 300
}

trait History extends AnyRef {  self =>
  /** Save the current value to the history with the specified 
   *  discrete time.
   */
  def save(t: Int): Unit

  /** Restore the value from the specified discrete time. */
  def restore(t: Int): Unit
  
  /** Destroy the history from the given time. */
  def clear(from: Int): Unit
}