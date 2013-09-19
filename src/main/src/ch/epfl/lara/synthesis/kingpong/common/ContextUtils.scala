package ch.epfl.lara.synthesis.kingpong.common

import android.content.Context
import scala.collection.mutable.{HashMap => MMap}
import android.content.res.TypedArray

trait ContextUtils {
  def context: Context
  
  implicit class RichTypedArray(t: TypedArray) {
    def mapDrawable = {
      (0 until t.length()) map { i => t.getDrawable(i) }
    }
    def mapColor(defValue: Int) = {
      (0 until t.length()) map { i => t.getColor(i, defValue) }
    }
    def map[B](f: Int => B) = {
      (0 until t.length()) map { i => f(t.getResourceId(i, 0)) }
    }
  }
  
  private val colors = MMap[Int, Int]()
  def color(id: Int) = colors.getOrElseUpdate(id, context.getResources().getColor(id))
  
  def getArray(id: Int): TypedArray = context.getResources().obtainTypedArray(id)
  def getString(id: Int) = context.getResources().getString(id)
  def getStringArray(id: Int) = context.getResources().getStringArray(id)
  def getDrawable(id: Int) = context.getResources().getDrawable(id)
  def getDrawableArray(id: Int) = getArray(id) map getDrawable
}