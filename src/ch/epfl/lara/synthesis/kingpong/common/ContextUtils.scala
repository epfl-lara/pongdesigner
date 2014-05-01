package ch.epfl.lara.synthesis.kingpong.common

import android.content.Context
import scala.collection.mutable.{HashMap => MMap, HashSet => MSet}
import scala.collection.concurrent.{TrieMap => CMap, _}
import android.content.res.TypedArray
import android.graphics.drawable.Drawable
import scala.collection.mutable.SynchronizedSet
import scala.concurrent._
import scala.util.Success
import ExecutionContext.Implicits.global
import android.util.Log

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
  object Str{
    var map = MMap[String, Int]()
    def apply(id: Int) = {
      val res = context.getResources().getString(id)
      map += res -> id
      res
    }
    def unapply(str: String): Option[Int] = map.get(str)
  }
  def getStringArray(id: Int) = {
    val res = context.getResources().getStringArray(id)
    val resMapping = getArray(id) map { f: Int => f}
    res zip resMapping foreach {
      case (str, id) => Str.map += str -> id
    }
    res
  }
  def getDrawable(id: Int) = context.getResources().getDrawable(id)
  def getDrawableArray(id: Int) = getArray(id) map getDrawable
  
  private var mapDrawable = CMap[String, Drawable]()
  private var mapRetrieved = new MSet[String] with SynchronizedSet[String]

  def retrieveDrawable(s: String): Option[Drawable] = {
    mapDrawable.get(s) match {
      case some@Some(d) => some
      case None =>
        Log.d("ContextUtils.scala", s"Asked to retrieve $s")
        if(mapRetrieved.add(s)) {
          val c: Future[Option[Drawable]] = future {
            Log.d("ContextUtils.scala", s"Starting aquisition of drawable $s")
            Option(getDrawable(context.getResources().getIdentifier(s, "drawable", context.getPackageName()))) match {
              case some@Some(d) =>
                mapDrawable(s) = d
                some
              case None => None
            }
          }
          c onComplete {
            case Success(res) => res match {
              case Some(d) => mapDrawable(s) = d
                Log.d("ContextUtils.scala", s"Found drawable $s")
              case None =>
                Log.d("ContextUtils.scala", s"Not found drawable $s")
            }
            case _ =>
          }
          None
        } else None
    }
  }
  
  implicit def toRunnable(f: => Unit): Runnable = {
    new Runnable() {
	    override def run() = {
	        f
	    }
	}
  }
}