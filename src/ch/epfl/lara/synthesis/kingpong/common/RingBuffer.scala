package ch.epfl.lara.synthesis.kingpong.common

import scala.collection.mutable.IndexedSeq

/**
 *  Datastructure with constant memory size and maximum number of elements.
 *  When the maximum size is reached, the oldest elements will the removed by the 
 *  newest ones.
 *  Operations Size, Head, Last, Insert, Update, Apply, Drop and Clear are in O(1).
 */
class RingBuffer[A : scala.reflect.ClassTag](maxSize: Int) extends IndexedSeq[A] {
  
  private val array = new Array[A](maxSize)
  private var _size = 0
  private var read = 0
  private var write = 0

  def length = _size
  override def size = _size
  override def isEmpty = _size == 0
  override def head = apply(0)
  override def last = apply(_size-1)

  /** Get the `idx`th element from the start. The first 
   *  element is the oldest, the last one the more 
   *  recently added.
   */
  def apply(idx: Int): A = {
    if (idx >= _size || idx < 0) 
      throw new IndexOutOfBoundsException(idx.toString)
    else 
      array(toIndex(idx))
  }

  /** Update the `idx`th element from the start.
   *  The buffer begins at 0.
   */
  def update(idx: Int, elem: A) {
    if (idx >= _size || idx < 0) 
      throw new IndexOutOfBoundsException(idx.toString)
    else 
      array(toIndex(idx)) = elem
  }
  
  /**
   * Replace the first element at a given time.
   */
  def replace(theElementToChangeIs: A => Boolean, convertElement: A => A): Unit = {
    val i = array.indexWhere(theElementToChangeIs(_))
    array(i) = convertElement(array(i))
  }

  /** Add a new element at the end of this buffer.
   *  If the buffer is already full, the oldest value
   *  will be removed.
   */
  def +=(elem: A): this.type = {
    array(write) = elem
    write = (write + 1) % maxSize
    if (_size == maxSize) 
      read = toIndex(1)
    else 
      _size += 1
    this
  }
  
  /** Add multiples elements at the end of this buffer.
   *  If the buffer is already full, the n oldest values
   *  will be removed.
   */
  def ++=(elems: Iterable[A]) {
    for (elem <- elems) this += elem
  }

  override def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var idx = size - 1
    val it = reverseIterator
    while (idx >= 0 && (idx > end || !p(apply(idx))) ) idx -= 1
    idx
  }
  
  override def iterator = new Iterator[A] {
    var idx = 0
    def hasNext = idx != _size
    def next = {
      val res = apply(idx)
      idx += 1
      res
    }
  }

  /** Remove elements from the start to index `n` exclusive. */
  def removeTo(n: Int): Unit = { 
    if (n >= size) {
      clear()
    } else if (n > 0) {
      read = toIndex(n)
      _size -= n
    }
  }
  
  /** Remove elements from index `n` inclusive to the end. */
  def removeFrom(n: Int): Unit = {
    if (n <= 0) {
      clear()
    } else if (n < size) {
      write = toIndex(n)
      _size = n
    }
  }
  
  /** Remove all elements from this buffer. */
  def clear() {
    read = 0
    write = 0
    _size = 0
  }

  override def toString = {
    this.mkString("[", ",", "]")
  }

  private def toIndex(i: Int): Int = (read + i) % maxSize

}
