package ch.epfl.lara.synthesis.kingpong.common

trait TimeTracker {
  def pause()
  def unpause(rewindTime: Long)
  def reset()
  //def currentTime()
  //def initializePaused()
  //def initializeUnpaused()
}

/**
 * Class which maps a time slider, time actions to a real time
 */
class TimeTrackerLinear extends TimeTracker {
  /** Absolute start time of the game + editor */
  private var startTime = 0L
  
  /** Total time spent in edit time*/
  private var totalNonPlayedTime = 0L
  
  /** When the last edit started */
  private var enteredEditTime = 0L
  
  /** Total visible time before currentTime*/
  private var totalMaxvisibleTime = 5000L
  
  /** Time at the beginning of the slider */
  private var minTimePosition = 0L

  def pause() {
    enteredEditTime = System.currentTimeMillis()
  }
  
  def unpause(rewindTime: Long = 0) {
    if(enteredEditTime != 0) {
      totalNonPlayedTime += System.currentTimeMillis() - enteredEditTime - rewindTime
      enteredEditTime = 0
    }
  }
  
  def reset() {
    startTime = System.currentTimeMillis()
    totalNonPlayedTime = 0L
    minTimePosition = 0L
  }
  
  def newTime(): Long = {
    val systemTime = System.currentTimeMillis()
    var newTime = systemTime - startTime - totalNonPlayedTime
    newTime
  }
  
  def morNonPlayedTime(delay: Long): Unit = {
    totalNonPlayedTime += delay
  }
  
  def getRelativeTime(absoluteTime: Long): Long = {
    absoluteTime - minTimePosition
  }
  
  /*def updateMinTimePosition(newTime: Long) = {
    if(newTime - minTimePosition > TriggerEvent.lengthStoredEvent) {
      minTimePosition = newTime - TriggerEvent.lengthStoredEvent
    }
  }*/
  
  def timeBarPosition(newTime: Long): Int = {
    (newTime - minTimePosition).toInt
  }
  
}