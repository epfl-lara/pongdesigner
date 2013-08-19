package ch.epfl.lara.synthesis.kingpong.common

trait ColorConstants {
  final val red    = 0xFFFF0000
  final val green  = 0xFF00FF00
  final val orange = 0xFFFF8800
  final val yellow = 0xFFFFFF00
  final val blue   = 0xFF0000FF
  final val cyan   = 0xFF00FFFF
  final val black  = 0xFF000000
  final val white  = 0xFFFFFFFF
  final val purple = 0xFFFF00FF
  
  private val colorfuls = List(red, orange, yellow, green, cyan, blue, purple)
  def colorful(j: Int): Int = colorfuls(j % colorfuls.size)
}