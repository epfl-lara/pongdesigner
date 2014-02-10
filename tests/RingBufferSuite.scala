package ch.epfl.lara.synthesis.kingpong.test

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import ch.epfl.lara.synthesis.kingpong.common.RingBuffer

class RingBufferSuite extends FunSuite with BeforeAndAfter {

  private val maxSize = 4

  private var buffer: RingBuffer[Int] = _

  before {
    buffer = new RingBuffer[Int](4)
  }

  test("insert elements") {
    assert(buffer.size === 0)
    assert(buffer.isEmpty === true)

    buffer += 1
    buffer += 2

    assert(buffer.isEmpty === false)
    assert(buffer.nonEmpty === true)
    assert(buffer.size === 2)
    assert(buffer.head === 1)
    assert(buffer.last === 2)
    assert(buffer(0) === 1)
    assert(buffer(buffer.size - 1) === 2)

    buffer ++= Seq(3, 4)
    assert(buffer.size === 4)
    assert(buffer(2) === 3)
    assert(buffer(3) === 4)
  }

  test("update elements") {
    assert(buffer.size === 0)

    buffer += 1
    buffer += 2
    buffer += 3

    assert(buffer.size === 3)
    
    buffer(1) = 4

    assert(buffer.size === 3)
    assert(buffer(0) === 1)
    assert(buffer(1) === 4)
    assert(buffer(2) === 3)
  }

  test("maximum size respected") {
    buffer += 1
    buffer += 2
    buffer += 3
    buffer += 4
    buffer += 5
    buffer += 6
    buffer += 7

    assert(buffer.isEmpty === false)
    assert(buffer.nonEmpty === true)
    assert(buffer.size === 4)
    assert(buffer.head === 4)
    assert(buffer.last === 7)
    assert(buffer(0) === 4)
    assert(buffer(1) === 5)
    assert(buffer(2) === 6)
    assert(buffer(3) === 7)
  }


  test("test exceptions") {
    intercept[IndexOutOfBoundsException] {
      buffer(0)
    }

    intercept[IndexOutOfBoundsException] {
      buffer(-1)
    }

    buffer += 1
    assert(buffer.size === 1)
    assert(buffer.head === 1)
    assert(buffer.last === 1)

    intercept[IndexOutOfBoundsException] {
      buffer(2)
    }

  }

  test("test findLast()") {
    
    buffer += 1
    buffer += 2
    buffer += 3
    buffer += 4

    assert(buffer.findLast(_ > 5) === None)
    assert(buffer.findLast(e => e > 1 && e < 4) === Some(3))
    assert(buffer.findLast(_ > 1) === Some(4))
    assert(buffer.findLast(_ <= 3) === Some(3))
    assert(buffer.findLast(_ % 2 == 0) === Some(2))
  }

}
