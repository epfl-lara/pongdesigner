package ch.epfl.lara.synthesis.kingpong.common

/**
 * Colors from [[http://developer.android.com/design/style/color.html]]
 */
object ColorConstants {
  
  val blue   = 0xFF33B5E5
  val purple = 0xFFAA66CC
  val green  = 0xFF99CC00
  val orange = 0xFFFFBB33
  val red    = 0xFFFF4444
  val blueDark   = 0xFF0099CC
  val purpleDark = 0xFF9933CC
  val greenDark  = 0xFF669900
  val orangeDark = 0xFFFF8800
  val redDark    = 0xFFCC0000
  val black  = 0xFF000000
  val white  = 0xFFFFFFFF
  
  private val colorfuls = List(blue, purple, green, orange, red)
  
  def colorful(j: Int): Int = colorfuls(j % colorfuls.size)
  
  def darker(color: Int): Int = color match {
    case `blue`   => blueDark
    case `purple` => purpleDark
    case `green`  => greenDark
    case `orange` => orangeDark
    case `red`    => redDark
    case _        => black // TODO what to do ?
  }
  
}