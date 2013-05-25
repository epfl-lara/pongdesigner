package ch.epfl.lara.synthesis.kingpong.common

class RingBuffer[A : scala.reflect.ClassTag](maxSize: Int) extends scala.collection.mutable.IndexedSeq[A] {
  private val array = new Array[A](maxSize)
  private var _size = 0
  private var read = 0
  private var write = 0

  def length = _size
  override def size = _size
  override def isEmpty = read == write
  override def head = apply(0)

  def apply(idx: Int): A = {
    if (idx >= _size) 
      throw new IndexOutOfBoundsException(idx.toString)
    else 
      array((read + idx) % maxSize)
  }

  def update(idx: Int, elem: A) {
    if (idx >= _size) 
      throw new IndexOutOfBoundsException(idx.toString)
    else 
      array((read + idx) % maxSize) = elem
  }

  def +=(elem: A) {
    array(write) = elem
    write = (write + 1) % maxSize
    if (_size == maxSize) 
      read = (read + 1) % maxSize
    else 
      _size += 1
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

  override def drop(n: Int): RingBuffer[A] = {
    if (n >= maxSize) 
      clear()
    else 
      read = (read + n) % maxSize
    this
  }
  
  def clear() {
    read = 0
    write = 0
    _size = 0
  }

}
