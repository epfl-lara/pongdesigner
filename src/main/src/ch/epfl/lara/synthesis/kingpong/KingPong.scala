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
import ch.epfl.lara.synthesis.kingpong.common.Messages

object KingPong {
  final val INTERVIEWNAME = "INTERVIEW_NAME"
    
  final val PONGGAMECOMPLETE_FILE = "2-playerPong"
  final val PONGGAMESPACEINVADERS_FILE = "SpaceInvaders"
  final val PONGGAMEPACMAN_FILE = "Pong Man"
  final val PONGGAMEMAZE_FILE = "Maze"
  final val PONGGAMETUTORIAL_FILE = "Tutorial"
  
  final val PONGNAME_SIMPLEBRICKBREAKER = "BrickBreaker"
  private var mapgames: Map[String,()=>Game] = Map.empty
  mapgames += (PONGNAME_SIMPLEBRICKBREAKER -> (() => new SimplePong()))
  
  //def mapGame(s: String): Game = { s match {
      /*case PONGGAMECOMPLETE_FILE => new PongKingPong()
      case PONGGAMESPACEINVADERS_FILE => new PongSpaceInvaders()
      case PONGGAMEPACMAN_FILE => new PongGamePacman()
      case PONGGAMEMAZE_FILE => new PongMaze2()
      case PONGGAMETUTORIAL_FILE => new Pongtutorial()
      */
 //     case PONGNAME_SIMPLEBRICKBREAKER => 
  //    case _ => new EmptyGame()
   // }
  //}
  
  final val PREFS_NAME = "MyPrefsFile"
    
  trait AutoShutOff {
    def fromPrevious(previous: AutoShutOff) {
      record = previous.record
      previous.record = null
      previous.finishRecording = null
      previous.willShutDown = false
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
    
  class LoadSaveGameTask(private var activity: KingPong, var saving: Boolean= true, var exporting: Boolean=false, var game: Game = null) extends MyAsyncTask[String, (String, Int, Int), Game] with AutoShutOff {
    var max=100
    var prog =0
    var filename: String = ""
    if(activity.task != null) {
      val previous = activity.task
      fromPrevious(previous)
      game = previous.game
    }
    
    protected def doInBackground1(names: ArrayBuffer[String]): Game = {
      filename = names(0)
      prog = 0
      if(saving) {
        val scala_file = filename.endsWith(".scala")
        /*game.outputItself(filename, !scala_file, {
        (text: String, i: Int, j: Int) => {
          publishProgress((text, i, j))
        }
        })*/ // TODO: output the game to a file
        
      } else {
        if(mapgames contains filename) {
          mapgames(filename)
          publishProgress(("Loading " + filename, 0, 100))
          game = mapgames(filename)()
          publishProgress(("Finished", 100, 100))
        } else {
          game = new EmptyGame()
          /*game.setGameEngine(activity.mGameView)
          game.fromFile(filename, {
             (text: String, i: Int, j: Int) => 
               publishProgress((text, i, j))
           })*/ /// TODO: Create a game from file.
        }
      }
      game
    }
  
    override protected def onProgressUpdate1(progress: ArrayBuffer[(String, Int, Int)]) = {
      if(activity != null) {
        prog = progress(0)._2
        max =  progress(0)._3
        
        activity ! Messages.TextProgressDialog(progress(0))
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
         activity ! Messages.HideProgressDialog()
         activity
       }
       if(activity != null && saving) {
         if(!exporting) {
          Toast.makeText(activity, "Game saved as '"+filename+"'", 1000).show()
         } else {
          activity ! Messages.FileExport(filename)
          ()
        }
       }
       prog = 0
       max = 100
     }
     
     def detach() = activity = null

     def attach(activity: KingPong) {
       this.activity=activity
       if(prog == max && game != null) {
         onPostExecute(game)
       }
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
  Log.d("KingPong", "KingPong class creating")

  onCreate { savedInstanceState: Bundle =>
    setContentView(R.layout.main)
    //setContentView(R.layout.activity_main)
    mGameView.setActivity(this)
    
    task = getLastNonConfigurationInstance().asInstanceOf[LoadSaveGameTask]
    
    if(task == null) {
      task = new LoadSaveGameTask(this)
    }
    if(!mGameView.hasGame()) {
      if(task.game == null) {
        task.game = new EmptyGame()
      }
      mGameView.setGame(task.game)
    }
    //mGameView.enterEditMode()
    mGameView.setKeepScreenOn(true)
    
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
  
  override def onCreateOptionsMenu(menu: Menu): Boolean = {
      val inflater = getMenuInflater()
      inflater.inflate(R.menu.game_menu, menu);
      return true;
  }
  
  override def onOptionsItemSelected(item: MenuItem): Boolean = {
    import common._
      // Handle item selection
      item.getItemId() match {
          case R.id.save =>
            //mGameView.saveGame(mHandler, false)
            true
          case R.id.reset =>
            val context = this
            val res = context.getResources()
            CustomDialogs.launchOKCancelDialog(context, res.getString(R.string.confirm_reset_title), res.getString(R.string.confirm_reset_message), false, 
                { _ =>
                  mGameView.setGame(new EmptyGame())
                },
                { _ => }
            )
            true
          case R.id.exportbutton =>
            //mGameView.saveGame(mHandler, true) // Export
            true
          case R.id.load_external =>
            //new LoadFileTask().execute()
            //mGameView.loadGame(mHandler)
            true
          case R.id.brickbreaker =>
            self ! Messages.FileLoad(PONGNAME_SIMPLEBRICKBREAKER)
            true
          case R.id.tutorial_game =>
            //if(Tutorial.mActions != Nil) Tutorial.executeNextAction() else Tutorial.launch()
            true
          case R.id.optionsButton =>
            val i : Intent = new Intent(this, classOf[UserPreferences])
            startActivity(i)
            true
          case _ =>
              super.onOptionsItemSelected(item)
      }
  }

  onPause {
    mGameView.onPause()
  }
  
  onDestroy {
    if(mProgressDialog != null) mProgressDialog.dismiss()
  }

  onResume {
    mGameView.onResume()
    if(mGameView != null && !mFirstLaunch) mGameView.onResume()
      mFirstLaunch = false
      mSensorManager.registerListener(this, mAccelerometer, SensorManager.SENSOR_DELAY_NORMAL)
      if(task == null) {
        task = new LoadSaveGameTask(self, saving=false, exporting=false, game=null)
        task.execute(PONGNAME_SIMPLEBRICKBREAKER)
      } else {
        // Resume the dialog task if needed.
        task.attach(this)
        mGameView.setGame(task.game)
        
        /*if(task.record != null) {
          if(mGameView != null) mGameView.mFPSPaint.setColor(0xFFFF0000)
        } else {
          if(mGameView != null) mGameView.mFPSPaint.setColor(0xFFFFFFFF)
        }*/ // TODO : recover the fps paint.
      }
      
      val settings = getSharedPreferences(PREFS_NAME, 0);
      val startTutorial = settings.getBoolean("startTutorial", true);
      if(startTutorial) {
        (self after 1000) ! Messages.ShowInitialTooltip()
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
      if(mGameView != null) task.game = mGameView.getGame
      task.game.storeInitialState(false)
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
  def after(delay: Long): RichHandler = new RichHandler(delay, mHandler)
  def !(m: Message) = mHandler ! m
  
  class RichHandler(delay: Long, handler: Handler) {
    def !(m: Message) = handler.sendMessageDelayed(m, delay)
  }
  /**
   * Handler for working with asynchronous messages.
   */
  private var mHandler = new Handler(){
    import Messages._
    def !(m: Message) = sendMessage(m)
    override def handleMessage(input_msg: Message) {
        input_msg match {
          
        case ShowProgressDialog() =>
          createProgressDialog(save=false)
          //mProgressDialog.setIndeterminate(false)
          
        case ShowProgressDialogSave() =>
          createProgressDialog(save=true)
          
        case HideProgressDialog() =>
          if(mProgressDialog != null) mProgressDialog.dismiss()
          mProgressDialog = null
          
        case TextProgressDialog(text, line, max) =>
          if(mProgressDialog == null) createProgressDialog()
            if(mProgressDialog != null) {
            mProgressDialog.setMessage(text)
            mProgressDialog.setProgress(line)
            if(max > 0) {
              mProgressDialog.setIndeterminate(false)
              mProgressDialog.setMax(max)
            }
          }
          
        case FileLoad(tempName) =>
          //new LoadFileTask().execute(input_msg.getData().getString(FILENAME_TAG))
          this ! ShowProgressDialog()
          task = new LoadSaveGameTask(self, saving=false, exporting=false,game=mGameView.getGame)
          task.execute(tempName)
          
        case FileSave(tempName) =>
          ShowProgressDialogSave()
          this ! ShowProgressDialogSave()
          task = new LoadSaveGameTask(self, saving=true, exporting=false, game=mGameView.getGame)
          task.execute(tempName)
          
        case FileSaveAndExport(tempName) =>
          this ! ShowProgressDialogSave()
          task = new LoadSaveGameTask(self, saving=true, exporting=true, game=mGameView.getGame)
          task.execute(tempName)
          
        case FileExport(tempName) =>
          val sharingIntent: Intent = new Intent(android.content.Intent.ACTION_SEND)
          sharingIntent.setType("text/plain")
          var U : android.net.Uri = null
          var F = getApplicationContext().getFileStreamPath(tempName)
          U = Uri.fromFile(F)
          sharingIntent.putExtra(Intent.EXTRA_STREAM, U)
          startActivity(Intent.createChooser(sharingIntent, self.getString(R.string.share_game_via)))
          
        case ShowInitialTooltip() =>
          //Tutorial.setActivity(KingPong.this, this)
          //Tutorial.initialTooltip()
          
        case CancelTutorial() =>
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
