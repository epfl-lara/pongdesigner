package ch.epfl.lara.synthesis.kingpong.objects

trait Timed {

  //TODO Do we want that also apply to the body (physical layer) ?
  /** Do a snapshot. */
  def snapshot(): Unit
  
  //TODO Do we want that also apply to the body (physical layer) ?
  /** Revert to the latest snapshot. */
  def revert(): Unit
  
}