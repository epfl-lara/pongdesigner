package ch.epfl.lara.synthesis.kingpong

import scala.collection.mutable.ArrayBuffer
import ch.epfl.lara.synthesis.kingpong.common.RingBuffer
import ch.epfl.lara.synthesis.kingpong.common.History
import ch.epfl.lara.synthesis.kingpong.rules.Events._
import ch.epfl.lara.synthesis.kingpong.objects.AssignableProperty
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import org.jbox2d.common.Vec2

/** Handle the history for events like fingers input and
 *  contacts between physical objects.
 */
class EventsHistory(val game: Game) {

  private var recording_time: Int = 0
  private var min_time: Int = time
  private var max_time: Int = time

  // Oldest first (head), more recent at the end (last). Both in O(1).
  private val history: RingBuffer[(Int, Seq[Event])] = new RingBuffer(History.MAX_HISTORY_SIZE)
  private val crtEvents = ArrayBuffer.empty[Event]

  /** Time for the last fully completed step. */
  def time = recording_time - 1
  
  /** Minimum time stored in the history. */
  def minTime = min_time
  
  /** Maximum time stored in the history. */
  def maxTime = max_time

  def restore(t: Int): Unit = {
    if (time >= 0 && time <= max_time) {
      recording_time = t + 1
      crtEvents.clear()
      
      // TODO : Add or remove objects
    }
  }
  
  def foreachEvent(f: (Event, Int) => Unit) = {
    for ((time, events) <- history; e <- events) f(e, time)
  }

  /* Advance the time and store the current events in the history. */
  def step(): Unit = {
    //Log.d("kingpong", s"Before context step, recording_time = $recording_time, min_time = $min_time, max_time = $max_time.")
    val processedEvents = crtEvents.synchronized {
      val res = crtEvents map {
        case FingerMove(from, to, null) =>
          FingerMove(from, to, game.physicalObjectFingerAt(from))
        case FingerDown(pos, null) =>
          FingerDown(pos, game.physicalObjectFingerAt(pos))
        case FingerUp(pos, null) =>
          FingerUp(pos, game.physicalObjectFingerAt(pos))
        case e => e
      }
      crtEvents.clear()
      res
    }
    
    recording_time += 1
    if (max_time < time) {
       max_time = time
    }
    min_time = Math.max(0, Math.max(min_time, max_time - History.MAX_HISTORY_SIZE + 1))

    if (processedEvents.nonEmpty) {
      history += (time, processedEvents)
    }
    //Log.d("kingpong", s"After context step, recording_time = $recording_time, min_time = $min_time, max_time = $max_time.")
  }
  
  /** Return the events from the last fully completed time step. 
   *  Should not be called by another thread than the one who 
   *  calls `step()`.
   */
  def lastEvents: Seq[Event] = {
    if(history.isEmpty || history.last._1 != time ) {
      Seq.empty
    } else {
      history.last._2
    }
  }

  /**
   *  Return the events from the given time step.
   *  Should not be called by another thread than the one who
   *  calls `step()`.
   */
  def events(t: Int): Seq[Event] = {
    val i = history.lastIndexWhere(_._1 == t)
    if (i >= 0) {
      history(i)._2
    } else {
      Seq.empty
    }
  }

  /** Add an event in the ongoing time step. 
   *  Can be called by another thread, from user inputs.
   *  Also handle the current, minimum and maximum time.
   */
  def addEvent(e: Event): Unit = crtEvents.synchronized {
    e match {
      case AccelerometerChanged(_) =>
        // Remove the old AccelerometerChanged, if any
        val i = crtEvents.indexWhere(_.isInstanceOf[AccelerometerChanged])
        if (i >= 0) 
          crtEvents(i) = e
        else
          crtEvents += e
      case _ => 
        crtEvents += e
    }      
  }
  
  /** Clear the history and the ongoing time step from the given time (inclusive). */
  def clear(from: Int): Unit = crtEvents.synchronized {
    //Log.d("kingpong", s"Before context clearing, recording_time = $recording_time, min_time = $min_time, max_time = $max_time.")
    if (from <= 0) {
      recording_time = 0
      history.clear()
    } else  {
      recording_time = from
      val idx = history.lastIndexWhere(_._1 < from)
      history.removeFrom(idx + 1)
    }
    
    crtEvents.clear()
    max_time = time
    min_time = Math.min(min_time, time)
    //Log.d("kingpong", s"After context clearing, recording_time = $recording_time, min_time = $min_time, max_time = $max_time.")
  }
}