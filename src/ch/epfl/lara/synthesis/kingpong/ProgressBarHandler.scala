package ch.epfl.lara.synthesis.kingpong

import android.widget.SeekBar

import ch.epfl.lara.synthesis.kingpong.GameView.Editing
import ch.epfl.lara.synthesis.kingpong.common.History

/**
 * Handler for the time bar to change the time displayed on it.
 */
trait ProgressBarHandler extends SeekBar.OnSeekBarChangeListener { self: GameView =>
  private var bar: SeekBar = _
  
  /** Called when to progress bar is modified by the user. */
  def onProgressBarChanged(progress: Int, secondaryProgress: Int): Unit
  
  def setProgressBar(progressBar: SeekBar): Unit = {
    bar = progressBar
    bar.setMax(History.MAX_HISTORY_SIZE)
    bar.setProgress(0)
    bar.setSecondaryProgress(0)
    bar.setOnSeekBarChangeListener(this)
  }
  
  def setProgressBarTime(t: Int) = {
    val progress = Math.max(0, t)
    bar.setProgress(progress)
    bar.setSecondaryProgress(progress)
  }
  
  def onProgressChanged(seekBar: SeekBar, progress: Int, fromUser: Boolean): Unit = {
    val secondaryProgress = bar.getSecondaryProgress()
    if (fromUser && seekBar == bar && !(progress > secondaryProgress && state != Editing)) {
      if(progress > secondaryProgress) {
        bar.setProgress(seekBar.getSecondaryProgress())
        onProgressBarChanged(secondaryProgress, secondaryProgress)
      } else {
        onProgressBarChanged(progress, secondaryProgress)
      }
    }
  }
  
  def onStartTrackingTouch(seekBar: SeekBar):Unit = {
  }
  def onStopTrackingTouch(seekBar: SeekBar):Unit = {
  }
}