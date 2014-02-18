package ch.epfl.lara.synthesis.kingpong.common

import util.control.Breaks._

/**
 *  Datastructure with constant memory size and maximum number of elements.
 *  When the maximum size is reached, the oldest elements will the removed by the 
 *  newest ones.
 *  Operations Size, Head, Last, Insert, Update, Apply, Drop and Clear are in O(1).
 */
class RingBuffer[A : scala.reflect.ClassTag](maxSize: Int) extends scala.collection.mutable.IndexedSeq[A] {
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

  /** Add a new element at the end of this buffer.
   *  If the buffer is already full, the oldest value
   *  will be removed.
   */
  def +=(elem: A) {
    array(write) = elem
    write = (write + 1) % maxSize
    if (_size == maxSize) 
      read = toIndex(1)
    else 
      _size += 1
  }

  /** Add multiples elements at the end of this buffer.
   *  If the buffer is already full, the n oldest values
   *  will be removed.
   */
  def ++=(elems: Iterable[A]) {
    for (elem <- elems) this += elem
  }

  /** Find the last element in the first series of elements 
   *  that satisfy the predicate.
   */
  def findLast(p: A => Boolean): Option[A] = {
    var found: Option[A] = None
    breakable {
      var i: Int = 0
      while (i < size) {
        val e = array(toIndex(i))
        if (p(e)) {
          found = Some(e)
        } else if (found != None) {
          break
        }
        i += 1
      }
    }
    found
  }
  
  /** Iterator that iterates over all elements in this buffer. */
  override def iterator = new Iterator[A] {
    var idx = 0
    def hasNext = idx != _size
    def next = {
      val res = apply(idx)
      idx += 1
      res
    }
  }

  /** Drop the `n` first elements. */
  override def drop(n: Int): RingBuffer[A] = {
    if (n >= maxSize)
      clear()
    else
      read = toIndex(n)
    this
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
