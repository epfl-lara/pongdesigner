package ch.epfl.lara.synthesis.kingpong

import java.util.ArrayList
import scala.collection.mutable.ArrayBuffer
import net.londatiga.android._
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
import android.content.Intent
import android.net.Uri
import android.media.MediaRecorder
import android.graphics.drawable.Drawable
import android.widget.TextView
import android.support.v4.view.GestureDetectorCompat
import android.view.GestureDetector
import android.view.MotionEvent
import android.widget.LinearLayout
import android.text.method.ScrollingMovementMethod
import android.widget.ImageView
import android.view.ViewGroup
import android.widget.ListView
import android.widget.ExpandableListView
import org.jbox2d.dynamics.contacts.{Contact => JBoxContact}
import ch.epfl.lara.synthesis.kingpong.common.History
import ch.epfl.lara.synthesis.kingpong.common.Messages
import ch.epfl.lara.synthesis.kingpong.examples.TestGame
import java.io.FileNotFoundException

object KingPong {
  final val INTERVIEWNAME = "INTERVIEW_NAME"
    
  final val PONGGAMECOMPLETE_FILE = "2-playerPong"
  final val PONGGAMESPACEINVADERS_FILE = "SpaceInvaders"
  final val PONGGAMEPACMAN_FILE = "Pong Man"
  final val PONGGAMEMAZE_FILE = "Maze"
  final val PONGGAMETUTORIAL_FILE = "Tutorial"
  
  final val PONGNAME_SIMPLEBRICKBREAKER = "BrickBreaker"
  final val PONGNAME_SUPERMARIO = "Mario"  
  final val PONGNAME_SLIDING = "SlidingPuzzle"  
  final val PONGNAME_THREEINAROW = "ThreeInARow"  
  private var mapgames: Map[String,()=>Game] = Map.empty
  mapgames += (PONGNAME_SIMPLEBRICKBREAKER -> (() => new examples.SimplePong()))
  mapgames += (PONGNAME_SUPERMARIO -> (() => new examples.PlatformGame()))
  mapgames += (PONGNAME_SLIDING -> (() => new examples.SlidingPuzzle()))
  mapgames += (PONGNAME_THREEINAROW -> (() => new examples.ThreeInARow()))
  
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
    
  class LoadSaveGameTask(private var activity: KingPong, var saving: Boolean= true, var exporting: Boolean=false, var game: Game = null) extends MyAsyncTask[String, (String, Int, Int), Game] with common.AutoShutOff {
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
          game = new TestGame()
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
         //game.setGameEngine(activity.mGameView)
         activity.mGameView.setGame(game)
         activity.mGameView.initialize()
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
               with ActivityUtil with SensorEventListener { self =>
  import R.id._
  import R.layout._
  import R.drawable._
  import KingPong._

  private lazy val mGameView: GameView = R.id.gameview
  private lazy val mCodeView: EditTextCursorWatcher = (code: TextView).asInstanceOf[EditTextCursorWatcher]
  private lazy val mSeekBar: SeekBar   = time_bar
  private lazy val mLayout1: LinearLayout = R.id.layout1
  private lazy val mLayoutcodehorizontal: LinearLayout = R.id.layoutcodehorizontal
  private lazy val mLayoutcodevertical: LinearLayout = R.id.layoutcodevertical
  private lazy val mCodeViewResizer: ImageView = R.id.codeviewResizer
  private lazy val mActions: ExpandableListView = (R.id.actions : ListView).asInstanceOf[ExpandableListView]
  
  // Renaming
  private val timeButtonPause = timebutton
  private val timeButtonPlay = timebutton2
  private var mFirstLaunch = true
  private var mSensorManager: SensorManager = null
  private var mAccelerometer: Sensor = null
  private var task : LoadSaveGameTask = null
  Log.d("KingPong", "KingPong class creating")

  private var mDetector: GestureDetectorCompat = null; 
  
  onCreate { savedInstanceState: Bundle =>
    setContentView(R.layout.main)
    //setContentView(R.layout.activity_main)
    mGameView.setActivity(this)
    mGameView.setCodeDisplay(mCodeView)
    mGameView.setProgressBar(mSeekBar)
    mGameView.setActionBar(mActions)
    
    task = getLastNonConfigurationInstance().asInstanceOf[LoadSaveGameTask]
    
    if(task == null) {
      task = new LoadSaveGameTask(this)
    }
    if(!mGameView.hasGame()) {
      if(task.game == null) {
        task.game = new TestGame()
      }
      mGameView.setGame(task.game)
    }
    //mGameView.enterEditMode()
    mGameView.setKeepScreenOn(true)
    mGameView.requestFocus()

    time_button.onClicked(onTimeButtonClick)
    back_button.onClicked(onBackButtonClick)
    
    menus.ColorMenu.createMenuFromColorArray(this, R.array.colors)
    menus.MenuOptions.context = this
    
    mDetector = new GestureDetectorCompat(self,new GestureDetector.SimpleOnGestureListener {
      override def onDown(event: MotionEvent): Boolean = { 
         Log.d("KingPong","onDown: " + event.toString());
        val x = event.getX()
        val y = event.getY()
        var closest_pos = 0
        var distance = 1897865f
        val layout = mCodeView.getLayout();
        for(pos <- 0 to (mCodeView.getText().length()-1)) {
          val line = layout.getLineForOffset(pos)
          val baseline = layout.getLineBaseline(line)
          val ascent = layout.getLineAscent(line)
          val xpos = layout.getPrimaryHorizontal(pos)
          val ypos = baseline + ascent
          if(y >= ypos && y <= baseline) {
            val dist = Math.abs(x - xpos)
            if(dist < distance) {
              distance = dist
              closest_pos = pos
            }
          }
        }
        mCodeView.setSelection(closest_pos)
        return true
      }
      override def onScroll(e1: MotionEvent, e2: MotionEvent, distanceX: Float, distanceY: Float) = {
        false
      }
    })
    mDetector.setOnDoubleTapListener(new GestureDetector.OnDoubleTapListener {
      def onDoubleTap(x$1: MotionEvent): Boolean = {
        true
      }
      def onDoubleTapEvent(x$1: android.view.MotionEvent): Boolean = return true
      def onSingleTapConfirmed(x$1: android.view.MotionEvent): Boolean  = return true
    });
    
    //mSeekBar.setOnTouchListener{ (v: View, event: MotionEvent) => mDetector.onTouchEvent(event) }
    mCodeView.setOnTouchListener{ (v: View, event: MotionEvent) => mDetector.onTouchEvent(event) }
    if(mCodeViewResizer != null) {
      var xprev = 0f
      var yprev = 0f
      mCodeViewResizer.setOnTouchListener{ (v: View, event: MotionEvent) =>
        val action = event.getAction()
        if(v == mCodeViewResizer) {
          (action & MotionEvent.ACTION_MASK) match {
            case MotionEvent.ACTION_DOWN =>
              xprev = event.getRawX()
              yprev = event.getRawY()
              true
            // A finger moves
            case MotionEvent.ACTION_MOVE | MotionEvent.ACTION_UP =>
              if(mLayoutcodehorizontal != null) {
                val x = event.getRawX()
                val params = mLayoutcodehorizontal.getLayoutParams().asInstanceOf[ViewGroup.LayoutParams]
                val dx = - (x - xprev).toInt
                params.width = params.width +dx
                mCodeViewResizer.setX(mCodeViewResizer.getX - dx)
                if(mActions != null) {
                  //mActions.setX(mCodeViewResizer.getX)
                  mActions.requestLayout()
                }
                Log.d("Test", s"The layout moves dx=$dx")
                mLayoutcodehorizontal.getParent().requestLayout()
                //xprev = x
              }
              if(mLayoutcodevertical != null) {
                val y = event.getRawY()
                val params = mLayoutcodevertical.getLayoutParams().asInstanceOf[ViewGroup.LayoutParams]
                val dy = - (y - yprev).toInt
                params.height = params.height + dy
                Log.d("Test", s"The layout moves dy=$dy")
                mCodeViewResizer.setY(mCodeViewResizer.getY - dy)
                if(mActions != null) {
                  mActions.requestLayout()
                }
                mLayoutcodevertical.getParent().requestLayout()
                //yprev = y
              }
              xprev = event.getRawX()
              yprev = event.getRawY()
              true
            case _ =>
              false
          }
        } else false
      }
    }
    
    //mCodeView.setMovementMethod(new ScrollingMovementMethod())
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
                  mGameView.setGame(new TestGame())
                  mGameView.backToBeginning()
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
          case R.id.supermario =>
            self ! Messages.FileLoad(PONGNAME_SUPERMARIO)
            true
          case R.id.sliding =>
            self ! Messages.FileLoad(PONGNAME_SLIDING)
            true
          case R.id.three_in_a_row =>
            self ! Messages.FileLoad(PONGNAME_THREEINAROW)
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
    time_button.setImageDrawable(timeButtonPlay)
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
  /*def onProgressChanged(bar: SeekBar, progress: Int, fromUser: Boolean) = {
    if (fromUser) {
      mGameView.onProgressBarChanged(progress)
    }
  }

  def onStartTrackingTouch(seekBar: SeekBar) {}
  def onStopTrackingTouch(seekBar: SeekBar) {}*/
  
  configurationObject {
    if(task != null) {
      if(mGameView != null) task.game = mGameView.getGame
      //TODO Mikael, what is the purpose of this?
      //task.game.storeInitialState(false)
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

        case PickImage() =>
          val photoPickerIntent = new Intent(Intent.ACTION_PICK);
          photoPickerIntent.setType("image/*");
          startActivityForResult(photoPickerIntent, 1);
          
        case _ =>
            Log.w("MyTag","Warning: message type \""+input_msg.what+"\" not supported");
        }
    }
  }
  
  override  protected def onActivityResult(requestCode: Int, resultCode: Int, imageReturnedIntent: Intent) { 
        super.onActivityResult(requestCode, resultCode, imageReturnedIntent); 
        requestCode match { 
        case 1 =>
            if(resultCode == Activity.RESULT_OK){
                try {
                    val imageUri = imageReturnedIntent.getData();
                    val imageStream = getContentResolver().openInputStream(imageUri);
                    val selectedImage = BitmapFactory.decodeStream(imageStream);
                    mGameView.setImageSelectedShape(selectedImage)
                } catch {
                  case e: FileNotFoundException =>
                    e.printStackTrace();
                }
 
            }
        }
    }
}
