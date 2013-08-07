package ch.epfl.lara.synthesis.kingpong

import android.app.Activity
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.hardware.Sensor
import android.hardware.SensorManager
import android.os.Bundle
import android.os.Handler
import android.widget.ImageButton
import android.widget.SeekBar
import android.view.View
import android.view.Menu
import android.view.MenuItem
import android.hardware.SensorEventListener
import android.hardware.Sensor
import android.hardware.SensorEvent
import android.content.Context
import android.content.pm.ActivityInfo
import android.view.Surface
import android.content.res.Configuration
import android.os.Message
import android.app.ProgressDialog
import android.widget.Toast
import android.util.Log
import android.os.AsyncTask
import java.util.ArrayList
import scala.collection.mutable.ArrayBuffer
import android.content.Intent
import android.net.Uri
import net.londatiga.android._
import android.media.MediaRecorder
import org.jbox2d.dynamics.contacts.{Contact => JBoxContact}
import android.graphics.drawable.Drawable
import ch.epfl.lara.synthesis.kingpong.common.History

object KingPong {
  final val SHOW_PROGRESS_DIALOG = 1
  final val SHOW_PROGRESS_DIALOG_SAVE = 11
  final val HIDE_PROGRESS_DIALOG = 2
  final val TEXT_PROGRESS_DIALOG = 3
  final val FILE_LOAD= 4
  final val FILE_PARSE= 5
  final val FILE_EXPORT = 6
  final val FILE_SAVE = 7
  final val FILE_SAVE_AND_EXPORT = 8
  final val SHOW_INITIAL_TOOLTIP = 9
  final val CANCEL_TUTORIAL = 10
  final val FILENAME_TAG = "filename"
  final val TEXT_TAG = "progress_text"
  final val LINE_TAG = "progress_line"
  final val TOTAL_TAG = "progressTotal"
    
  final val INTERVIEWNAME = "INTERVIEW_NAME"
    
  final val PONGGAMECOMPLETE_FILE = "2-playerPong"
  final val PONGGAMESPACEINVADERS_FILE = "SpaceInvaders"
  final val PONGGAMEPACMAN_FILE = "Pong Man"
  final val PONGGAMEMAZE_FILE = "Maze"
  final val PONGGAMETUTORIAL_FILE = "Tutorial"
  
  def mapGame(s: String): Game = { s match {
      /*case PONGGAMECOMPLETE_FILE => new PongKingPong()
      case PONGGAMESPACEINVADERS_FILE => new PongSpaceInvaders()
      case PONGGAMEPACMAN_FILE => new PongGamePacman()
      case PONGGAMEMAZE_FILE => new PongMaze2()
      case PONGGAMETUTORIAL_FILE => new Pongtutorial()
      */case _ => null
    }
  }
  
  final val PREFS_NAME = "MyPrefsFile"
    
  class LoadSaveGameTask(private var activity: KingPong) extends MyAsyncTask[String, (String, Int, Int), Game] {
    var max=100
    var prog =0
    var gameHandler: Game = null
    var saving: Boolean = false
    var exporting: Boolean = false
    var filename: String = ""
      
    def this(previous: LoadSaveGameTask) = {
      this(previous.activity)
      gameHandler = previous.gameHandler
      record = previous.record
      previous.record = null
      previous.finishRecording = null
      previous.willShutDown = false
    }
    
    protected def doInBackground1(names: ArrayBuffer[String]): Game = {
      filename = names(0)
      prog = 0
      if(saving) {
        val scala_file = filename.endsWith(".scala")
        /*gameHandler.outputItself(filename, !scala_file, {
        (text: String, i: Int, j: Int) => {
          publishProgress((text, i, j))
        }
        })*/ // TODO: output the game to a file
        
      } else {
        filename match {
          case PONGGAMECOMPLETE_FILE | PONGGAMESPACEINVADERS_FILE | PONGGAMEPACMAN_FILE | PONGGAMEMAZE_FILE | PONGGAMETUTORIAL_FILE =>
            publishProgress(("Loading " + filename, 0, 100))
            gameHandler = mapGame(filename)
            publishProgress(("Finished", 100, 100))
          case _ =>
            gameHandler = new EmptyGame()
            /*gameHandler.setGameEngine(activity.mGameView)
            gameHandler.fromFile(filename, {
               (text: String, i: Int, j: Int) => 
                 publishProgress((text, i, j))
             })*/ /// TODO: Create a game from file.
        }
      }
      gameHandler
    }
  
    override protected def onProgressUpdate1(progress: ArrayBuffer[(String, Int, Int)]) = {
      if(activity != null) {
        val msg = Message.obtain()
        msg.what = KingPong.TEXT_PROGRESS_DIALOG
        prog = progress(0)._2
        max =  progress(0)._3
        
        msg.getData().putString(KingPong.TEXT_TAG, progress(0)._1)
        msg.getData().putInt(KingPong.LINE_TAG, progress(0)._2)
        msg.getData().putInt(KingPong.TOTAL_TAG, progress(0)._3)
        activity.mHandler.sendMessage(msg)
      }
      //setProgressPercent(progress(0));
    }
     
     override protected def onPostExecute(game: Game) {
       if(activity != null && activity.mGameView != null && !saving) {
         game.setGameEngine(activity.mGameView)
         activity.mGameView.setGame(game)
         activity.mGameView.initialize()
         // TODO: Initialize the game when loaded
       }
       if(activity != null) {
         var msg = Message.obtain()
         msg.what = KingPong.HIDE_PROGRESS_DIALOG;
         activity.mHandler.sendMessage(msg);
       }
       if(activity != null && saving) {
         if(!exporting) {
          Toast.makeText(activity, "Game saved as '"+filename+"'", 1000).show()
         } else {
           val msg = Message.obtain();
           msg.what = KingPong.FILE_EXPORT;
           msg.getData().putString(KingPong.FILENAME_TAG, filename)
           activity.mHandler.sendMessage(msg)
          ()
        }
       }
       prog = 0
       max = 100
     }
     
     def detach() = activity = null

     def attach(activity: KingPong) {
       this.activity=activity
       if(prog == max && gameHandler != null) {
         onPostExecute(gameHandler)
       }
     }
     var record: MediaRecorder = _
     var finishRecording: Thread = _
     var willShutDown: Boolean = false
     def cancelShutDown() = willShutDown = false
     def startTimerDown() = {
       if(finishRecording != null) {
         willShutDown = false
         Thread.sleep(100)
       }
       finishRecording = new Thread(new Runnable{
         override def run() = {
           willShutDown = true
           val init_time = System.currentTimeMillis
           while(System.currentTimeMillis - init_time < 10000 && willShutDown) {
             Thread.sleep(10)
           }
           if(willShutDown) {
             if(record != null) {
               record.stop()
               record.release()
               record = null
             }
           }
           finishRecording = null
         }
       })
       if(finishRecording != null) finishRecording.start()
     }
  }
}

class KingPong extends Activity 
               with SeekBar.OnSeekBarChangeListener with ActivityUtil with SensorEventListener { self =>
  import R.id._
  import R.drawable._
  import KingPong._

  private lazy val mGameView: GameView = gameview
  // Renaming
  private val timeButtonPause = timebutton
  private val timeButtonPlay = timebutton2
  private var mFirstLaunch = true
  private var mSensorManager: SensorManager = null
  private var mAccelerometer: Sensor = null
  private var task : LoadSaveGameTask = null

  onCreate { savedInstanceState: Bundle =>
    setContentView(R.layout.main)
    mGameView.setActivity(this)
    
    time_bar.setMax(History.MAX_HISTORY_SIZE)
    time_bar.setProgress(0)
    time_bar.setSecondaryProgress(0)
    time_bar.setOnSeekBarChangeListener(this)
    mGameView.requestFocus()

    time_button.onClicked(onTimeButtonClick)
    back_button.onClicked(onBackButtonClick)
    
     mSensorManager = getSystemService(Context.SENSOR_SERVICE).asInstanceOf[SensorManager]
     mAccelerometer = mSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER)
  }

  onPause {
    mGameView.onPause()
  }

  onResume {
    mGameView.onResume()
    if(mGameView != null && !mFirstLaunch) mGameView.onResume()
      mFirstLaunch = false
      mSensorManager.registerListener(this, mAccelerometer, SensorManager.SENSOR_DELAY_NORMAL)
      if(task == null) {
        
      } else {
        // Resume the dialog task if needed.
        task.attach(this)
        mGameView.setGame(task.gameHandler)
        
        /*if(task.record != null) {
          if(mGameView != null) mGameView.mFPSPaint.setColor(0xFFFF0000)
        } else {
          if(mGameView != null) mGameView.mFPSPaint.setColor(0xFFFFFFFF)
        }*/ // TODO : recover the fps paint.
      }
      
      val settings = getSharedPreferences(PREFS_NAME, 0);
      val startTutorial = settings.getBoolean("startTutorial", true);
      if(startTutorial) {
        val m = Message.obtain()
        m.what = SHOW_INITIAL_TOOLTIP
        mHandler.sendMessageDelayed(m, 1000)
      }
  }

  private def onBackButtonClick() = {
    time_button.setImageDrawable(timeButtonPlay)
    mGameView.backToBeginning()
  }
  
  private def onTimeButtonClick() = mGameView.state match {
    case GameView.Editing =>
      mGameView.toRunning()
      time_button.setImageDrawable(timeButtonPause)

    case GameView.Running =>
      mGameView.toEditing()
      time_button.setImageDrawable(timeButtonPlay)
  }

  /** When the progress bar changes from the user. */
  def onProgressChanged(bar: SeekBar, progress: Int, fromUser: Boolean) = {
    if (fromUser) {
      mGameView.onProgressBarChanged(progress)
    }
  }

  def onStartTrackingTouch(seekBar: SeekBar) {}
  def onStopTrackingTouch(seekBar: SeekBar) {}
  
  configurationObject {
    if(task != null) {
      if(mGameView != null) task.gameHandler = mGameView.getGame
      task.gameHandler.storeInitialState(false)
      task.detach()
    }
    task
  }
  
  val gravity = new Array[Float](3)
  
  /**
   * Sensor events
   */
  def onAccuracyChanged(sensor: Sensor, accuracy: Int) {}
  
  /**
   * Sensor events
   */
  def onSensorChanged(event: SensorEvent) = {
    val alpha = 0.8f // Smoothing along time
    
    gravity(0) = alpha * gravity(0) + (1 - alpha) * event.values(0)
    gravity(1) = alpha * gravity(1) + (1 - alpha) * event.values(1)
    gravity(2) = alpha * gravity(2) + (1 - alpha) * event.values(2)
    
    val rotation = getWindowManager().getDefaultDisplay().getOrientation()

    /*rotation match {
      case Surface.ROTATION_0 =>
        mGameView.set2DAcceleration(gravity(0), gravity(1))
      case Surface.ROTATION_90 =>
        mGameView.set2DAcceleration(-gravity(1), gravity(0))
      case Surface.ROTATION_180 =>
        mGameView.set2DAcceleration(-gravity(0), -gravity(1))
      case Surface.ROTATION_270 =>
        mGameView.set2DAcceleration(gravity(1), -gravity(0))
      case _ =>
    }*/ // TODO : integrate motion sensing
  }
  
  var mProgressDialog: ProgressDialog = null
  var tempName: String = null
  //var tempGame: Game = null
  
  def createProgressDialog(save: Boolean = false) = {
    if(mProgressDialog != null) {
      mProgressDialog.dismiss()
      mProgressDialog = null
    }
    mProgressDialog = new ProgressDialog(self)
    if(save) {
      mProgressDialog.setMessage(getResources().getString(R.string.saving_game))
    } else {
      mProgressDialog.setMessage(getResources().getString(R.string.loading_game))
    }
    mProgressDialog.setIndeterminate(false)
    mProgressDialog.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL)
    mProgressDialog.setMax(100)
    mProgressDialog.setCancelable(false)
    mProgressDialog.show()
  }
  
  private var mHandler = new Handler(){
    override def handleMessage(input_msg: Message) {
        input_msg.what match {
        case SHOW_PROGRESS_DIALOG =>
          createProgressDialog()
          //mProgressDialog.setIndeterminate(false)
        case SHOW_PROGRESS_DIALOG_SAVE =>
          createProgressDialog(true)
        case HIDE_PROGRESS_DIALOG =>
          if(mProgressDialog != null) mProgressDialog.dismiss()
          mProgressDialog = null
        case TEXT_PROGRESS_DIALOG =>
          if(mProgressDialog == null) createProgressDialog()
            if(mProgressDialog != null) {
            mProgressDialog.setMessage(input_msg.getData().getString(TEXT_TAG))
            mProgressDialog.setProgress(input_msg.getData().getInt(LINE_TAG))
            val max = input_msg.getData().getInt(TOTAL_TAG)
            if(max > 0) {
              mProgressDialog.setIndeterminate(false)
              mProgressDialog.setMax(max)
            }
          }
        case FILE_LOAD =>
          //new LoadFileTask().execute(input_msg.getData().getString(FILENAME_TAG))
          tempName = input_msg.getData().getString(FILENAME_TAG)
          var msg = Message.obtain()
          msg.what = KingPong.SHOW_PROGRESS_DIALOG
          sendMessage(msg)
          if(task != null) {
            task = new LoadSaveGameTask(task)
          } else {
            task = new LoadSaveGameTask(self)
          }
          task.execute(tempName)
        case FILE_SAVE =>
          tempName = input_msg.getData().getString(FILENAME_TAG)
          var msg = Message.obtain()
          msg.what = KingPong.SHOW_PROGRESS_DIALOG_SAVE
          sendMessage(msg)
          if(task != null) {
            task = new LoadSaveGameTask(task)
          } else {
            task = new LoadSaveGameTask(self)
          }
          task.saving = true
          task.exporting = false
          task.gameHandler = mGameView.getGame
          task.execute(tempName)
        case FILE_SAVE_AND_EXPORT =>
          tempName = input_msg.getData().getString(FILENAME_TAG)
          var msg = Message.obtain()
          msg.what = KingPong.SHOW_PROGRESS_DIALOG_SAVE
          sendMessage(msg)
          if(task != null) {
            task = new LoadSaveGameTask(task)
          } else {
            task = new LoadSaveGameTask(self)
          }
          task.saving = true
          task.exporting = true
          task.gameHandler = mGameView.getGame
          task.execute(tempName)
        case FILE_EXPORT =>
          tempName = input_msg.getData().getString(FILENAME_TAG)
          val sharingIntent: Intent = new Intent(android.content.Intent.ACTION_SEND)
          sharingIntent.setType("text/plain")
          var U : android.net.Uri = null
          var F = getApplicationContext().getFileStreamPath(tempName)
          U = Uri.fromFile(F)
          sharingIntent.putExtra(Intent.EXTRA_STREAM, U)
          startActivity(Intent.createChooser(sharingIntent, self.getString(R.string.share_game_via)))
        case SHOW_INITIAL_TOOLTIP =>
          //Tutorial.setActivity(KingPong.this, this)
          //Tutorial.initialTooltip()
        case CANCEL_TUTORIAL =>
          val settings = getSharedPreferences(PREFS_NAME, 0);
          val editor = settings.edit();
          editor.putBoolean("startTutorial", false);
          editor.commit();
        case _ =>
            Log.w("MyTag","Warning: message type \""+input_msg.what+"\" not supported");
        }
    }
  }
}
