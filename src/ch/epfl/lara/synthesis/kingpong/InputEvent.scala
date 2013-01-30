package ch.epfl.lara.synthesis.kingpong

import GameShapes._

object ParameterHistory {
  def apply[T](v: T, t: Long): ParameterHistory[T] = {
    val res = new ParameterHistory[T]
    res.timestamp = t
    res.value = v
    res
  }
}

/**
 * Represents a value for a parameter at a certain time.
 */
class ParameterHistory[T] {
  var next: ParameterHistory[T] = null
  var prev: ParameterHistory[T] = null
  var timestamp: Long = 0
  var value: T = _
}

/**
 * Source of the parameter type T
 */
trait SourceOf[T] {
  protected var mFirstUnusedValue: ParameterHistory[T] = null
  def pop():ParameterHistory[T] = {
    this synchronized {
      val result = mFirstUnusedValue
      if(mFirstUnusedValue != null) {
        mFirstUnusedValue = mFirstUnusedValue.next
        if(mFirstUnusedValue != null) mFirstUnusedValue.prev = null
      }
      if(result != null) {
        result.next = null
        result.prev = null
      }
      result
    }
  }
  def push(b: ParameterHistory[T]) = {
    this synchronized {
      b.next = mFirstUnusedValue
      b.prev = null
      mFirstUnusedValue = b
    }
  }
  protected def createParameterHistory(timestamp: Long):ParameterHistory[T] = {
    this synchronized {
      var result = pop()
      if(result == null) result = new ParameterHistory[T]
      result.timestamp = timestamp
      result
    }
  }
}

/**
 * Keeps a list of T values ordered by timestamp
 */
trait OrderedListByTimestamp[T] extends SourceOf[T] {
  protected var mFirstValue: ParameterHistory[T] = null
  protected var mLastValue: ParameterHistory[T] = null
  
  def last_value = if(mLastValue != null) mLastValue.value else null
  
  def foreachFromFarAway(timestamp: Long)(f: ParameterHistory[T] => Unit) = {
    this synchronized {
      var pointerFirst = mFirstValue
      var pointerLast = mLastValue
      
      while(pointerFirst != pointerLast && pointerFirst != null && pointerLast != null) {
        if(timestamp - pointerFirst.timestamp < pointerLast.timestamp - timestamp) {
          f(pointerLast)
          pointerLast = pointerLast.prev
        } else {
          f(pointerFirst)
          pointerFirst = pointerFirst.next
        }
      }
      if(pointerFirst != null) f(pointerFirst)
    }
  }
  
  def foreach(f: ParameterHistory[T] => Unit) = {
    var pointer = mFirstValue
    while(pointer != null && pointer != pointer.next) {
      f(pointer)
      pointer = pointer.next
    }
    if(pointer != null && pointer == pointer.next) {
      pointer = pointer.next //Error here
    }
  }
  def foreachReverse(f: ParameterHistory[T] => Unit) = {
    var pointer = mLastValue
    while(pointer != null) {
      f(pointer)
      pointer = pointer.prev
    }
  }
  def foreachAfterIncluding(time: Long)(f: ParameterHistory[T] => Unit) = {
    var pointer = mFirstValue
    while(pointer != null) {
      if(pointer.timestamp >= time) f(pointer)
      pointer = pointer.next
    }
  }
  def foreachAfterExcluding(time: Long)(f: ParameterHistory[T] => Unit) = {
    var pointer = mFirstValue
    while(pointer != null) {
      if(pointer.timestamp > time) f(pointer)
      pointer = pointer.next
    }
  }
  def foreachBetween(time: Long, time2: Long)(f: ParameterHistory[T] => Unit) = {
    var pointer = mFirstValue
    while(pointer != null) {
      if(pointer.timestamp > time && pointer.timestamp <= time2) f(pointer)
      pointer = pointer.next
    }
  }
  def foreachReverseUntilIncluding(time: Long)(f: ParameterHistory[T] => Unit) = {
    var pointer = mLastValue
    while(pointer != null && pointer.timestamp >= time) {
      f(pointer)
      pointer = pointer.prev
    }
  }
  def foreachWithUntil[S](other: OrderedListByTimestamp[S], time: Long)(f: (ParameterHistory[T],ParameterHistory[S])  => Unit) = {
    var pointer1 = mLastValue
    var pointer2 = other.mLastValue
    var pointer1isMax = false
    var pointer2isMax = false
    while(pointer1 != null && pointer2 != null && (pointer1.timestamp >= time || pointer2.timestamp >= time)) {
      f(pointer1, pointer2)
      if(pointer1.timestamp >= pointer2.timestamp) pointer1isMax = true
      if(pointer2.timestamp >= pointer1.timestamp) pointer2isMax = true
      
      if(pointer1isMax) pointer1 = pointer1.prev
      if(pointer2isMax) pointer2 = pointer2.prev
    }
  }
  def foreachWithUntil[S](other1: OrderedListByTimestamp[S], other2: OrderedListByTimestamp[S], time: Long)(f: (ParameterHistory[T], ParameterHistory[S], ParameterHistory[S]) => Unit) = {
    var pointer1 = mLastValue
    var pointer2 = other1.mLastValue
    var pointer3 = other2.mLastValue
    var pointer1isMax = false
    var pointer2isMax = false
    var pointer3isMax = false
    while(pointer1 != null && pointer2 != null && pointer3 != null && (pointer1.timestamp >= time || pointer2.timestamp >= time || pointer3.timestamp >= time)) {
      f(pointer1, pointer2, pointer3)
      
      if(pointer1.timestamp >= pointer2.timestamp && pointer1.timestamp >= pointer3.timestamp) pointer1isMax = true
      if(pointer2.timestamp >= pointer3.timestamp && pointer2.timestamp >= pointer1.timestamp) pointer2isMax = true
      if(pointer3.timestamp >= pointer1.timestamp && pointer3.timestamp >= pointer2.timestamp) pointer3isMax = true
      
      if(pointer1isMax) pointer1 = pointer1.prev
      if(pointer2isMax) pointer2 = pointer2.prev
      if(pointer3isMax) pointer3 = pointer3.prev
    }
  }
  
  // Removes all events that happened before the given timestamp
  def removeTooOldValues(givenTimestamp: Long): Unit = {
    this synchronized {
      var pointer = mFirstValue
      while(pointer != null && pointer.timestamp < givenTimestamp) {
        val headToRemove = pointer
        pointer = pointer.next
        if(pointer != null) pointer.prev = null
        push(headToRemove)
      }
      mFirstValue = pointer
    }
  }
  // Removes all events that happened after the given timestamp
  def keepOnlyValuesBeforeAndIncluding(givenTimestamp: Long):Unit = {
    this synchronized {
      var pointer = mLastValue
      while(pointer != null && pointer.timestamp > givenTimestamp) {
        val headToRemove = pointer
        pointer = pointer.prev
        if(pointer != null) pointer.next = null
        push(headToRemove)
      }
      mLastValue = pointer
    }
  }
}

/**
 * Contains a set of historic values for a given parameter.
 */
class ParameterHistoryCollection[T] extends OrderedListByTimestamp[T] {
  /**
   * Returns the n-th based parameter value. Negative counts from the end.
   */
  def apply(n: Int, default_value: T): T = {
    if(n < 0) {
      var pointer = mLastValue
      var i = n
      while(pointer != null && i < -1) { pointer = pointer.prev; i += 1 }
      if(pointer != null) pointer.value else default_value
    } else {
      var pointer = mFirstValue
      var i = n
      while(pointer != null && i > 0) { pointer = pointer.next; i += 1 }
      if(pointer != null) pointer.value else default_value
    }
  }
  
  /**
   * Returns the value of this parameter at the given time.
   * If no previous value exists, takes the first one available, or the default value.
   */
  def apply(timestamp: Long, default_value: T): T = {
    var pointer = mLastValue
    while(pointer != null && pointer.timestamp > timestamp) { pointer = pointer.prev }
    if(pointer != null) pointer.value else {
      if(mFirstValue != null) {
        mFirstValue.value
      } else {
        default_value
      }
    }
  }
  
  // Adds a new value
  def addOrReplaceValue(timestamp: Long, value: T) = {
    this synchronized {
      if(mLastValue == null || mFirstValue == null) {
        val newValue = createParameterHistory(timestamp)
        newValue.value = value
        newValue.next = null
        newValue.prev = null
        mFirstValue = newValue
        mLastValue = newValue
      } else {
        // Insert the event in chronological order, normally at the end, but it can be different.
        // Merges two events if they have the same timestamp.
        var pointer: ParameterHistory[T] = mLastValue
        while(pointer != null && timestamp < pointer.timestamp) {
          pointer = pointer.prev
        }
        if(pointer == null) { // we have reached the start of the list, we insert the element before
          val newValue = createParameterHistory(timestamp)
          newValue.value = value
          newValue.next = mFirstValue
          if(mFirstValue != null) mFirstValue.prev = newValue
          mFirstValue = newValue
        } else if(pointer.timestamp == timestamp) { // We just udpate the value
          pointer.value = value
        } else if(pointer.value == value) { // We do not store the change.
        } else {//the timestamp is posterior and the value is the same or different
          // here pointer.timestamp < timestamp
          val othernode = pointer.next // timestamp < othernode.timestamp
          val newValue = createParameterHistory(timestamp)
          newValue.value = value
          pointer.next = newValue
          newValue.prev = pointer
          newValue.next = othernode
          if(othernode != null) {
            othernode.prev = newValue
          } else {
            mLastValue = newValue
          }
        }
      }
    }
  }
}



/**
 * Contains the definition of all possible trigger events.
 * 
 */
object TriggerEvent {
  final val TOUCHDOWN_EVENT = 0
  final val TOUCHMOVE_EVENT = 1
  final val TOUCHUP_EVENT = 2
  final val NEW_TIMESTAMP_EVENT = 3
  
  final val COLLISION_EVENT = 4
  final val INTEGER_CHANGE_EVENT = 12
  final val INTEGER_EQUAL_EVENT = 13
  final val INTEGER_GREATER_EQUAL_EVENT = 14
  final val INTEGER_LESS_EQUAL_EVENT = 15
  final val INTEGER_POSITIVE_EVENT = 16
  final val INTEGER_NEGATIVE_EVENT = 17

  final val BEYOND_SCREEN_EVENT = 18
  final val BEYOND_SIDE_EVENT = 19
  
  /** Recording the events that occured on the screen*/
  final val lengthStoredEvent = 5000
  
  /** Transforms the input event to make it useful (e.g. by linearizing the finger movement) */
  def getInputEvent(i: ParameterHistory[TriggerEvent]): ParameterHistory[TriggerEvent] = {
    if(i == null) return null
    if(i.value.code != TOUCHMOVE_EVENT) return i
    var result = new TriggerEvent
    result.code = TOUCHMOVE_EVENT

    var pointer: ParameterHistory[TriggerEvent] = i
    var lastX = i.value.x2
    var lastY = i.value.y2
    while(pointer != null) {
      if(pointer.value.code == TOUCHMOVE_EVENT && pointer.value.x1 == lastX && pointer.value.y1 == lastY) {
        lastX = pointer.value.x2
        lastY = pointer.value.y2
      }
      if(pointer.value.code == TOUCHUP_EVENT && pointer.value.x1 == lastX && pointer.value.y1 == lastY) {
        result.x2 = lastX
        result.y2 = lastY
        pointer = null
      } else {
        pointer = pointer.next
      }
    }
    lastX = i.value.x1
    lastY = i.value.y1
    pointer = i
    while(pointer != null) {
      if(pointer.value.code == TOUCHMOVE_EVENT && pointer.value.x2 == lastX && pointer.value.y2 == lastY) {
        lastX = pointer.value.x1
        lastY = pointer.value.y1
      }
      if(pointer.value.code == TOUCHDOWN_EVENT && pointer.value.x1 == lastX && pointer.value.y1 == lastY) {
        result.x1 = lastX
        result.y1 = lastY
        pointer = null
      } else {
        pointer = pointer.prev
      }
    }
    ParameterHistory(result, i.timestamp)
  }
}

/** An trigger event defined by a code, coordinates, and shapes it refers to. */
class TriggerEvent {
  import TriggerEvent._
  
  var code: Int = 0
  
  var x1: Float = 0
  var y1: Float = 0
  var x2: Float = 0
  var y2: Float = 0
  
  def selectableBy(x: Float, y: Float) = {
    (x1 - x) * (x1 - x) + (y1 - y) * (y1 - y) < GameShapes.selectableAreaRadius * GameShapes.selectableAreaRadius
  }
  var shape1: Shape = null
  var shape2: Shape = null
}

/** A traversable collection of trigger events */
class TriggerEventCollection extends OrderedListByTimestamp[TriggerEvent]{
  import TriggerEvent._
  
  def createParameterHistory(timestamp: Long,  code: Int, shape1: Shape, shape2: Shape, x1: Float, y1: Float, x2: Float, y2: Float): ParameterHistory[TriggerEvent]= {
    var result = pop()
    if(result == null) {
      result = new ParameterHistory[TriggerEvent]
      result.value = new TriggerEvent
    }
    result.timestamp = timestamp
    result.value.code = code
    result.value.shape1 = shape1
    result.value.shape2 = shape2
    result.value.x1 = x1
    result.value.y1 = y1
    result.value.x2 = x2
    result.value.y2 = y2
    result
  }
  
  def addEvent(i: ParameterHistory[TriggerEvent]): ParameterHistory[TriggerEvent] = {
    if(i != null) addEvent(i.timestamp, i.value.code, i.value.shape1, i.value.shape2, i.value.x1, i.value.y1, i.value.x2, i.value.y2) else null
  }
  
  // Adds a new event
  def addEvent(timestamp: Long, code: Int, shape1: Shape, shape2: Shape, x1: Float, y1: Float, x2: Float, y2: Float): ParameterHistory[TriggerEvent] = {
    if(!(code == TOUCHMOVE_EVENT && x1 == x2 && y1 == y2)) {
      val newValue = createParameterHistory(timestamp, code, shape1, shape2, x1, y1, x2, y2)
      this synchronized {
        // Adds a new value
        if(mLastValue == null || mFirstValue == null) {
          newValue.next = null
          newValue.prev = null
          mFirstValue = newValue
          mLastValue = newValue
        } else {
          // Insert the event in chronological order, normally at the end, but it can be different.
          var pointer: ParameterHistory[TriggerEvent] = mLastValue
          while(pointer != null && timestamp < pointer.timestamp) {
            pointer = pointer.prev
          }
          if(pointer == null) { // we have reached the start of the list, we insert the element before
            newValue.next = mFirstValue
            if(mFirstValue != null) mFirstValue.prev = newValue
            mFirstValue = newValue
          } else { //the timestamp is posterior and the value is different
            // here pointer.timestamp < timestamp
            val othernode = pointer.next // timestamp < othernode.timestamp
            pointer.next = newValue
            newValue.prev = pointer
            newValue.next = othernode
            if(othernode != null) {
              othernode.prev = newValue
            } else {
              mLastValue = newValue
            }
          }
        }
      }
      newValue
    } else null
  }
}