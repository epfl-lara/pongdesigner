package ch.epfl.lara.synthesis.kingpong;

import android.app.Activity
import android.view.View
import android.os.Bundle
import android.widget.Button
import android.content.Intent
import android.widget.SeekBar
import android.widget.ImageButton
import android.graphics.drawable.Drawable
import android.widget.TextView
import scala.collection.mutable.{HashMap => MMap}
import android.view.MotionEvent
import android.widget.LinearLayout
import android.widget.ImageView
import android.widget.ListView
import language.implicitConversions
import android.widget.GridView

trait Implicits {
  implicit def toOnclickListener(f: ()=>Unit):View.OnClickListener = {
    new View.OnClickListener{ override def onClick(v: View) = f() }
  }
  implicit def toOnTouchListener(f: (View, MotionEvent) => Boolean): View.OnTouchListener = {
    new View.OnTouchListener() {
      def onTouch(v: View, event: MotionEvent): Boolean = {
          f(v, event)
      }
  }}
  
  class RichView(b: View) { // Scala 2.10 !
    def onClicked(f: =>Unit) = b.setOnClickListener{ () => f }
  }
  implicit def toRichView[B <: View](b: B) = new RichView(b)
}

/**
 * Tool to have shorter activity code such as onPause { }
 * or onCreate { bundle =>  ...  }
 */
trait ActivityUtil extends Activity with Implicits { self =>  
  private var customOnPause: () => Unit = null
  override def onPause() = {
    super.onPause()
    if(customOnPause != null) customOnPause()
  }
  def onPause(f: =>Unit) = {
    customOnPause = {() => f}
  }
  private var customOnDestroy: () => Unit = null
  override def onDestroy() = {
    super.onDestroy()
    if(customOnDestroy != null) customOnDestroy()
  }
  def onDestroy(f: =>Unit) = {
    customOnDestroy = {() => f}
  }
  private var customOnResume: () => Unit = null
  override def onResume() = {
    super.onResume()
    if(customOnResume != null) customOnResume()
  }
  def onResume(f: =>Unit) = {
    customOnResume = {() => f}
  }
  private var customOnCreate: Bundle => Unit = null
  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    if(customOnCreate != null) customOnCreate(savedInstanceState)
  }
  def onCreate(f: Bundle => Unit) = {
    customOnCreate = f
  }
  
  private var customOnRetainNonConfigurationInstance: () => Object = null
  def configurationObject(f: => Object) = {
    customOnRetainNonConfigurationInstance = () => f
  }
  override def onRetainNonConfigurationInstance(): Object = customOnRetainNonConfigurationInstance()

  implicit def findView(id: Int): View = findViewById(id)
  private implicit val vMap = MMap[Int, View]()
  //private implicit val ibMap = MMap[Int, ImageButton]()
  private implicit val ivMap = MMap[Int, ImageView]()
  private implicit val sbMap = MMap[Int, SeekBar]()
  private implicit val tvMap = MMap[Int, TextView]()
  private implicit val gvMap = MMap[Int, GameView]()
  private implicit val dMap = MMap[Int, Drawable]()
  private implicit val lMap = MMap[Int, LinearLayout]()
  private implicit val LWMap = MMap[Int, ListView]()
  private implicit val GVWMap = MMap[Int, GridView]()
  
  def findView[A <: View](id: Int)(implicit v: MMap[Int, A]): A = v.getOrElseUpdate(id, findViewById(id).asInstanceOf[A])
  def findDrawable[A <: Drawable](id: Int)(implicit v: MMap[Int, A]): A = v.getOrElseUpdate(id, getResources().getDrawable(id).asInstanceOf[A])

  //implicit def findViewImageButton(id: Int): ImageButton = findView[ImageButton](id)
  implicit def findViewImageView(id: Int): ImageView = findView[ImageView](id)
  implicit def findViewSeekBar(id: Int): SeekBar = findView[SeekBar](id)
  implicit def findViewTextView(id: Int): TextView = findView[TextView](id)
  implicit def findViewListView(id: Int): ListView = findView[ListView](id)
  implicit def findViewGameView(id: Int): GameView = findView[GameView](id)
  implicit def findViewGridView(id: Int): GridView = findView[GridView](id)
  implicit def findDrawableTop(id: Int): Drawable = findDrawable[Drawable](id)
  implicit def findLinearLayout(id: Int): LinearLayout = findView[LinearLayout](id)
  implicit def findOnClicked(id: Int): RichView = toRichView(findView[View](id))
}
