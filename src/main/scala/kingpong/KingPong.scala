package ch.epfl.lara.synthesis.kingpong

import android.app.Activity
import android.os.Bundle

import org.jbox2d.dynamics._
import org.jbox2d.collision._
import org.jbox2d.collision.shapes._
import org.jbox2d.dynamics.contacts.{Contact => JBoxContact}

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.objects._

object Kingpong extends Activity {
  lazy private val view = findViewById(R.id.gameview).asInstanceOf[GameView]



  override def onCreate(savedInstanceState: Bundle) {

  }


  override def onPause() = {
    super.onPause()
  }
    
  override def onResume() = {
    super.onResume()
  }

}