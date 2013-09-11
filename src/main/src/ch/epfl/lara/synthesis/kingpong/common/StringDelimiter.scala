package ch.epfl.lara.synthesis.kingpong.common

case class StringDelimiter(first: String, successive: String) {
  private var firsttime = true
  def get: String = if(firsttime) {firsttime = false; first} else successive
}