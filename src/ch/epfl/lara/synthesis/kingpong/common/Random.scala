package ch.epfl.lara.synthesis.kingpong.common

object Random {
  def apply(): Random = new Random(System.currentTimeMillis())
}

/**
 * A subclass of java.util.random that implements the 
 * Xorshift random number generator.
 */
class Random(private var seed: Long) extends java.util.Random {
  
  protected override def next(nbits: Int): Int = {
    var x = seed
    x ^= (x << 21)
    x ^= (x >>> 35)
    x ^= (x << 4)
    seed = x
    x &= ((1L << nbits) - 1)
    x.toInt
  }
}
