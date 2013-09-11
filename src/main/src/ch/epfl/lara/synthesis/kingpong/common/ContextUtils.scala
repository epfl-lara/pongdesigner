package ch.epfl.lara.synthesis.kingpong.common

import android.content.Context
import scala.collection.mutable.{Map => MMap}

trait ContextUtils {
  def context: Context
  
  private val colors = MMap[Int, Int]()
  def color(id: Int) = colors.getOrElseUpdate(id, context.getResources().getColor(id))
}