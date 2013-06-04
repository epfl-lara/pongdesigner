package ch.epfl.lara.synthesis.kingpong

import android.app.Activity
import android.os.Bundle
import android.view.View
import android.widget.ImageButton
import android.widget.SeekBar

import org.jbox2d.dynamics._
import org.jbox2d.collision._
import org.jbox2d.collision.shapes._
import org.jbox2d.dynamics.contacts.{Contact => JBoxContact}

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.History
import ch.epfl.lara.synthesis.kingpong.objects._

class Kingpong extends Activity 
               with SeekBar.OnSeekBarChangeListener {

  lazy private val view = findViewById(R.id.gameview).asInstanceOf[GameView]
  lazy private val timeButton = findViewById(R.id.time_button).asInstanceOf[ImageButton]
  lazy private val backButton = findViewById(R.id.back_button).asInstanceOf[ImageButton]
  lazy private val timeBar = findViewById(R.id.time_bar).asInstanceOf[SeekBar]

  lazy private val timeButtonPause = getResources().getDrawable(R.drawable.timebutton)
  lazy private val timeButtonPlay = getResources().getDrawable(R.drawable.timebutton2)

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.main)
    view.setActivity(this)
    
    timeBar.setMax(History.MAX_HISTORY_SIZE)
    timeBar.setProgress(0)
    timeBar.setSecondaryProgress(0)
    timeBar.setOnSeekBarChangeListener(this)
    view.requestFocus()

    timeButton.setOnClickListener( new View.OnClickListener{ 
      override def onClick(v: View) = onTimeButtonClick
    })

    backButton.setOnClickListener( new View.OnClickListener{ 
      override def onClick(v: View) = onBackButtonClick
    })

  }

  override def onPause() = {
    super.onPause()
    view.onPause()
  }
    
  override def onResume() = {
    super.onResume()
    view.onResume()
  }

  private def onBackButtonClick() = {
    timeButton.setImageDrawable(timeButtonPlay)
    view.backToBeginning()
  }
  
  private def onTimeButtonClick() = view.state match {
    case GameView.Editing =>
      view.toRunning()
      timeButton.setImageDrawable(timeButtonPause)

    case GameView.Running =>
      view.toEditing()
      timeButton.setImageDrawable(timeButtonPlay)
  }

  /** When the progress bar changes from the user. */
  def onProgressChanged(bar: SeekBar, progress: Int, fromUser: Boolean) = {
    if (fromUser) {
      view.onProgressBarChanged(progress)
    }
  }

  def onStartTrackingTouch(seekBar: SeekBar) {}
  def onStopTrackingTouch(seekBar: SeekBar) {}

}