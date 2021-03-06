package ch.epfl.lara.synthesis.kingpong

import java.io.FileNotFoundException
import scala.collection.mutable.ArrayBuffer
import android.app.Activity
import android.app.ProgressDialog
import android.content.Context
import android.content.Intent
import android.graphics.BitmapFactory
import android.hardware.Sensor
import android.hardware.SensorEvent
import android.hardware.SensorEventListener
import android.hardware.SensorManager
import android.net.Uri
import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.speech.tts.TextToSpeech
import android.speech.tts.TextToSpeech._
import android.support.v4.view.GestureDetectorCompat
import android.util.Log
import android.view.GestureDetector
import android.view.Menu
import android.view.MenuItem
import android.view.MotionEvent
import android.view.View
import android.view.ViewGroup
import android.widget.ExpandableListView
import android.widget.ListView
import android.widget.TextView
import android.widget.Toast
import android.widget.SeekBar
import android.widget.LinearLayout
import android.widget.ImageView
import ch.epfl.lara.synthesis.kingpong.common.Messages
import ch.epfl.lara.synthesis.kingpong.serialization.GameSerializer
import common.UndoRedo
import java.util.Locale
import java.io.OutputStreamWriter
import android.os.Vibrator
import scala.util.parsing.combinator.testing.Str
import ch.epfl.lara.synthesis.kingpong.common.ContextUtils
import android.provider.MediaStore
import android.graphics.Bitmap
import android.graphics.drawable.BitmapDrawable
import android.content.res.Configuration
import java.net.URLDecoder

trait GameCreationContext {
  def bitmap(id: Int): Int
}

object KingPong {
  final val TTS_CHECK = 1
  final val IMPORT_PICT = 2
  final val CHANGE_OPTIONS = 3
  
  
  final val INTERVIEWNAME = "INTERVIEW_NAME"
    
  final val PONGGAMECOMPLETE_FILE = "2-playerPong"
  final val PONGGAMESPACEINVADERS_FILE = "SpaceInvaders"
  final val PONGGAMEPACMAN_FILE = "Pong Man"
  final val PONGGAMEMAZE_FILE = "Maze"
  final val PONGGAMETUTORIAL_FILE = "Tutorial"

  final val PONGNAME_SIMPLEBRICKBREAKER = "BrickBreaker"
  final val PONGNAME_ALGORITHMS = "Algorithms"
  final val PONGNAME_BALANCED_PARENTHESES = "BalancedParentheses"
  final val PONGNAME_SUPERMARIO = "Mario"
  final val PONGNAME_SLIDING = "SlidingPuzzle"  
  final val PONGNAME_THREEINAROW = "ThreeInARow"  
  final val PONGNAME_DRAWINGRECORDER = "DrawingRecorder"
  final val PONGNAME_TESTGAME = "TestGame"
  final val PONGNAME_EMPTY = "empty game"
  private val mapGames: Map[String,(String, GameCreationContext)=>Game] = Map(
    PONGNAME_SIMPLEBRICKBREAKER -> ((language: String, g: GameCreationContext) => new examples.BrickBreaker()),
    PONGNAME_SUPERMARIO -> ((language: String, g: GameCreationContext) => new examples.PlatformGame()),
    PONGNAME_SLIDING -> ((language: String, g: GameCreationContext) => new examples.SlidingPuzzle()),
    PONGNAME_THREEINAROW -> ((language: String, g: GameCreationContext) => new examples.ThreeInARow()),
    PONGNAME_DRAWINGRECORDER -> ((language: String, g: GameCreationContext) => new examples.DrawingRecorder()),
    PONGNAME_ALGORITHMS -> ((language: String, g: GameCreationContext) => new examples.MatricesAlgorithms()),
    PONGNAME_BALANCED_PARENTHESES -> ((language: String, g: GameCreationContext) => new examples.BalancedParentheses()),
    PONGNAME_TESTGAME -> ((language: String, g: GameCreationContext) => new examples.ProofConceptGame(g, language)),
    PONGNAME_EMPTY -> ((language: String, g: GameCreationContext) => new examples.EmptyGame())
  )

  final val PREFS_NAME = "MyPrefsFile"
    
  private def writeToFile(context: Context, s: String, filename: String): Unit = {
    val o = new OutputStreamWriter(context.openFileOutput(filename, Context.MODE_WORLD_READABLE))
    o.write(s)
    o.close()
  }
    
  class LoadSaveGameTask(private var activity: KingPong, var language: String, var saving: Boolean= true, var exporting: Boolean=false, var game: (Game, List[(Int, Bitmap)]) = null) extends MyAsyncTask[String, (String, Int, Int), (Game, List[(Int, Bitmap)])] with common.AutoShutOff {
    var max=100
    var prog =0
    var filename: String = ""
    if(activity.task != null) {
      val previous = activity.task
      fromPrevious(previous)
      game = previous.game
    }
    
    protected def doInBackground1(names: ArrayBuffer[String]): (Game, List[(Int, Bitmap)]) = {
      filename = names(0)
      prog = 0
      if(saving) {
        val scala_file = filename.endsWith(".scala")
        if(scala_file) { //cannot do anything
          
        } else { // Assume the pd2 extension
          GameSerializer.saveGame(activity.mGameView.getGame(), activity.mGameView.getBitmaps(),
              output => {
                writeToFile(activity, output, filename)
              },
              (i, j) => {
                publishProgress(("Saving to "+filename, i, j))
              }
          )
        }
      } else {
        if(mapGames contains filename) {
          //mapGames(filename)
          publishProgress(("Loading " + filename, 0, 100))
          game = (mapGames(filename)(language, activity), Nil)
          publishProgress(("Finished", 100, 100))
        } else {
          val file = new java.io.File(activity.getFilesDir(), filename)
          val content: String = scala.io.Source.fromFile(file).mkString
          game = GameSerializer.loadGame(content,
              (i, j, s) =>
                publishProgress(("Loading from "+filename + ". " + s, i, j)))
          game._1.clear(from = 0)
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
      //setProgressPercent(progress(0))
    }
     
     override protected def onPostExecute(game: (Game, List[(Int, Bitmap)])) {
       if(activity != null && activity.mGameView != null && !saving) {
         //game.setGameEngine(activity.mGameView)
         var toRemove = activity.mGameView.loadBitmapSet(game._2)
         activity.mGameView.setGame(game._1, toRemove)
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
               with ActivityUtil with SensorEventListener with ContextUtils with GameCreationContext { self =>
  import R.id._
  import R.layout._
  import R.drawable._
  import KingPong._
 
  // prove: task == null || mGameView == null || mGameView.game == task.game
  private lazy val mGameView: GameView = R.id.gameview
  private lazy val mCodeView: EditTextCursorWatcher = (code: TextView).asInstanceOf[EditTextCursorWatcher]
  private lazy val mSeekBar: SeekBar = time_bar
  private lazy val mLayout1: LinearLayout = R.id.layout1
  private lazy val mLayoutcodehorizontal: LinearLayout = R.id.layoutcodehorizontal
  private lazy val mLayoutcodevertical: LinearLayout = R.id.layoutcodevertical
  private lazy val mCodeViewResizer: ImageView = R.id.codeviewResizer
  private lazy val mActions: ExpandableListView = (R.id.actions : ListView).asInstanceOf[ExpandableListView]
  private var mTts: TextToSpeech = null
  private var mTtsListener: TextToSpeech.OnInitListener = new TextToSpeech.OnInitListener {
    override def onInit(status: Int) {       
        if (status == TextToSpeech.SUCCESS) {
            //mTts.speak("The test is working.", TextToSpeech.QUEUE_FLUSH, null)
            //mTts.speak("Ceci est un deuxieme test !", TextToSpeech.QUEUE_ADD, null)
        }
    }
  }
  
  // Renaming
  private val timeButtonPause = timebutton
  private val timeButtonPlay = timebutton2
  private var mFirstLaunch = true
  private var mSensorManager: SensorManager = null
  private var mAccelerometer: Sensor = null
  private var task : LoadSaveGameTask = null
  Log.d("KingPong", "KingPong class creating")

  private var mDetector: GestureDetectorCompat = null
  
  def getLanguage() = getSharedPreferences(UserPreferences.PREFERENCES_FILE, 0).getString(UserPreferences.LOCALE_KEY, "en")
  
  onCreate { savedInstanceState: Bundle =>
    setContentView(R.layout.main)
    //setContentView(R.layout.activity_main)
    mGameView.setActivity(this)
    mGameView.setCodeDisplay(mCodeView)
    mGameView.setProgressBar(mSeekBar)
    mGameView.setActionBar(mActions)
    
    task = getLastNonConfigurationInstance().asInstanceOf[LoadSaveGameTask]
    
    if(task == null) {
      task = new LoadSaveGameTask(this, getLanguage())
    }
    if(!mGameView.hasGame()) {
      if(task.game == null) {
        task.game = (new examples.ProofConceptGame(self, getLanguage()), Nil)
      }
      mGameView.loadBitmapSet(task.game._2)
      mGameView.setGame(task.game._1)
    }
    //mGameView.enterEditMode()
    mGameView.setKeepScreenOn(true)
    mGameView.requestFocus()

    time_button.onTouch(onTimeButtonListerer)
    back_button.onClicked(onBackButtonClick)
    
    menus.ColorMenu.createMenuFromColorArray(this, R.array.colors)
    menus.MenuOptions.context = this
    
    val intent = getIntent()
    if(intent != null && intent.getData() != null) {
      val name = intent.getData().getEncodedPath()
      if(name != null && name != "" && (intent.getExtras() == null || !intent.getExtras().getBoolean("used", false))) {
        intent.putExtra("used", true)
        this.mHandler.sendMessageDelayed(Messages.FileLoad(URLDecoder.decode(name, "UTF-8")), 2000)
      }
    }
    
    mDetector = new GestureDetectorCompat(self,new GestureDetector.SimpleOnGestureListener {
      def retrieveTextPosition(x: Float, y: Float): Int = {
        var closest_pos = 0
        var distance = 1897865f
        val layout = mCodeView.getLayout
        for(pos <- 0 to (mCodeView.getText.length()-1)) {
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
        closest_pos
      }
      
      override def onLongPress(event: MotionEvent): Unit = {
        Log.d("kingpong","onLongPress: " + event.toString)
        val x = event.getX
        val y = event.getY
        //vibrate()
        val closest_pos = retrieveTextPosition(x, y)
        mGameView.codeViewOnLongPress(closest_pos, x, y)
      }
      override def onDown(event: MotionEvent): Boolean = { 
        Log.d("kingpong","onDown: " + event.toString)
        val x = event.getX
        val y = event.getY
        val closest_pos = retrieveTextPosition(x, y)
        mGameView.setCodeViewMenuCoords(x, y)
        mCodeView.setSelection(closest_pos)
        mGameView.codeViewMotionEventListener(event)
        true
      }
      override def onScroll(e1: MotionEvent, e2: MotionEvent, distanceX: Float, distanceY: Float) = {
        mGameView.codeViewMotionEventListener(e2)
      }
    })
    mDetector.setOnDoubleTapListener(new GestureDetector.OnDoubleTapListener {
      def onDoubleTap(x$1: MotionEvent): Boolean = true
      def onDoubleTapEvent(x$1: android.view.MotionEvent): Boolean = true
      def onSingleTapConfirmed(x$1: android.view.MotionEvent): Boolean  = true
    })
    
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
                val dx = - (event.getRawX() - xprev).toInt
                val params = mLayoutcodehorizontal.getLayoutParams().asInstanceOf[ViewGroup.LayoutParams]
                params.width += dx
                mCodeViewResizer.setX(mCodeViewResizer.getX - dx)
                mActions.setX(mCodeViewResizer.getX)
                mLayoutcodehorizontal.getParent().requestLayout()
              }
              if(mLayoutcodevertical != null) {
                val dy = - (event.getRawY() - yprev).toInt
                val params = mLayoutcodevertical.getLayoutParams().asInstanceOf[ViewGroup.LayoutParams]
                params.height = params.height + dy
                mCodeViewResizer.setY(mCodeViewResizer.getY - dy)
                mLayoutcodevertical.getParent().requestLayout()
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
    
    val checkIntent = new Intent()
		checkIntent.setAction("android.speech.tts.engine.CHECK_TTS_DATA" /*Engine.ACTION_CHECK_TTS_DATA*/);
		startActivityForResult(checkIntent, TTS_CHECK)
  }
  
  def bitmap(id: Int): Int = {
    if(mGameView != null) mGameView.addDrawable(id) else 0
  }
  
  def showActionBar() = {
    mCodeViewResizer.setVisibility(View.VISIBLE)
    mActions.setVisibility(View.VISIBLE)
  }
  
  def hideActionBar() = {
    mCodeViewResizer.setVisibility(View.GONE)
    mActions.setVisibility(View.GONE)
  }
  
  override def onCreateOptionsMenu(menu: Menu): Boolean = {
      val inflater = getMenuInflater()
      inflater.inflate(R.menu.game_menu, menu)
      true
  }
  final val UNDO_ACTION = 1
  final val REDO_ACTION = 2
  
  def addUndoList(undo_item: MenuItem, expand: Boolean = false) = {
    val undos = UndoRedo.getUndos()
    val undo_menu = undo_item.getSubMenu()
    undo_menu.clear()
    var i = 1
    for(undo <- undos) {
      undo_menu.add(Menu.NONE, UNDO_ACTION, i, undo)
      i += 1
    }
    if(undos.size == 0) {
      undo_menu.add(Menu.NONE, REDO_ACTION, 0, Str(R.string.nothing_to_undo))
    }
    if(expand) undo_item.expandActionView()
  }
  
  def addRedoList(redo_item: MenuItem, expand: Boolean = false) = {
    val redos = UndoRedo.getRedos()
    val redo_menu = redo_item.getSubMenu()
    redo_menu.clear()
    var i = 1
    for(redo <- redos) {
      redo_menu.add(Menu.NONE, REDO_ACTION, i, redo)
      i += 1
    }
    if(redos.size == 0) {
      redo_menu.add(Menu.NONE, REDO_ACTION, 0, Str(R.string.nothing_to_redo))
    }
    if(expand) redo_item.expandActionView()
  }
  
  override def onPrepareOptionsMenu(menu: Menu): Boolean = {
    val undo_item = menu.findItem(R.id.undo)
    val redo_item = menu.findItem(R.id.redo)
    addUndoList(undo_item)
    addRedoList(redo_item)
    return super.onPrepareOptionsMenu(menu)
}
  
  override def onOptionsItemSelected(item: MenuItem): Boolean = {
    import common._
      // Handle item selection
      item.getItemId() match {
          case R.id.undo =>
            addUndoList(item, expand=true)
            //UndoRedo.undo(1)
            
            true//false
          case R.id.redo =>
            addRedoList(item, expand=true)
            //UndoRedo.redo(1)
            true//false
          case UNDO_ACTION =>
            val i = item.getOrder()
            if(i > 0) UndoRedo.undo(i)
            true
          case REDO_ACTION =>
            val i = item.getOrder()
            if(i > 0) UndoRedo.redo(i)
            true
          case R.id.save =>
            mGameView.saveGame(mHandler, false)
            true
          case R.id.reset =>
            val context = this
            val res = context.getResources()
            CustomDialogs.launchOKCancelDialog(context, res.getString(R.string.confirm_reset_title), res.getString(R.string.confirm_reset_message), false, 
                { _ =>
                  self ! Messages.FileLoad(PONGNAME_EMPTY)
                  time_button.setImageDrawable(timeButtonPlay)
                  if(mTts != null) mTts.stop()
                  mGameView.backToBeginning()
                },
                { _ => }
            )
            true
          case R.id.exportbutton =>
            mGameView.saveGame(mHandler, true) // Export
            true
          case R.id.load_external =>
            mGameView.loadGame(mHandler)
            true
          case R.id.brickbreaker =>   self ! Messages.FileLoad(PONGNAME_SIMPLEBRICKBREAKER)
          case R.id.supermario   =>   self ! Messages.FileLoad(PONGNAME_SUPERMARIO)
          case R.id.sliding =>        self ! Messages.FileLoad(PONGNAME_SLIDING)
          case R.id.three_in_a_row => self ! Messages.FileLoad(PONGNAME_THREEINAROW)
          case R.id.drawing_game =>   self ! Messages.FileLoad(PONGNAME_DRAWINGRECORDER)
          case R.id.algorithms =>     self ! Messages.FileLoad(PONGNAME_ALGORITHMS)
          case R.id.balanced_parentheses => self ! Messages.FileLoad(PONGNAME_BALANCED_PARENTHESES)
          case R.id.tutorial_game =>  self ! Messages.FileLoad(PONGNAME_TESTGAME)
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
    task.game = (mGameView.getGame(), mGameView.getBitmaps())
    time_button.setImageDrawable(timeButtonPlay)
  }
  
  onDestroy {
    if(mProgressDialog != null) mProgressDialog.dismiss()
    if(mTts != null) mTts.shutdown()
  }

  override def onResume() {
    val preferences = getSharedPreferences(UserPreferences.PREFERENCES_FILE, 0)
    val currentlocale = Option(Locale.getDefault()).map(_.getLanguage()).getOrElse("en")
    val locale = preferences.getString(UserPreferences.LOCALE_KEY, currentlocale)
    if(locale != currentlocale) { // Restart the activity
      val l = new Locale(locale)
      Locale.setDefault(l);
       val config = new Configuration()
      config.locale = l;
      getBaseContext().getResources().updateConfiguration(config, getBaseContext().getResources().getDisplayMetrics());
      refresh();
      super.onResume();
    } else {
      super.onResume();
      //Log.d("GraphismeBaseActivity", "onResume")
      if(mGameView != null) {
        if(!mFirstLaunch) mGameView.onResume()
	      mFirstLaunch = false
	      mSensorManager.registerListener(this, mAccelerometer, SensorManager.SENSOR_DELAY_NORMAL)
	      if(task == null) {
	        task = new LoadSaveGameTask(self, locale, saving=false, exporting=false, game=null)
	        task.execute(PONGNAME_SIMPLEBRICKBREAKER)
	      } else {
	        // Resume the dialog task if needed.
	        task.attach(this)
	        if(mGameView.getGame() != task.game._1) {
	          mGameView.loadBitmapSet(task.game._2)
	          mGameView.setGame(task.game._1)
	        }
	        
	        
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
    }
  }
  
  def refresh() {
    finish();
    val myIntent = new Intent(KingPong.this, classOf[KingPong]);
    startActivity(myIntent);
  }

  private def onBackButtonClick() = {
    time_button.setImageDrawable(timeButtonPlay)
    if(mTts != null) mTts.stop()
    mGameView.backToBeginning()
    showActionBar()
  }
  
  private var vibratorInstance: Vibrator = null
  def vibrate(vibrations: Array[Long] = null): Unit = {
    if(vibratorInstance == null) {
      vibratorInstance = this.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[Vibrator]
      if(vibratorInstance == null) return
    }
    if(vibrations == null) {
      vibratorInstance.vibrate(50)
    } else {
      vibratorInstance.vibrate(vibrations, -1)
    }
  }
  def vibrateCancel(): Unit = {
    if(vibratorInstance != null) vibratorInstance.cancel()
  }
  
  private var startTouchDown = 0L
  private final val MAX_MS_EDITWHILERUNNING = 750
  private val onTimeButtonListerer = (v: View, event: MotionEvent) => {
	(event.getAction() & MotionEvent.ACTION_MASK) match {
		case MotionEvent.ACTION_DOWN | MotionEvent.ACTION_POINTER_DOWN =>
		  startTouchDown = System.currentTimeMillis()
		  mGameView.state match {
				case GameView.Running =>
				  if(mGameView.editWhileRunning) {
				    mGameView.editWhileRunning = false
				    hideActionBar()
				  } else {
			      mGameView.editWhileRunning = true 
			      vibrate(Array(MAX_MS_EDITWHILERUNNING, 100))
				  }
				case GameView.Editing =>
		  }
			true
		case MotionEvent.ACTION_UP | MotionEvent.ACTION_POINTER_UP =>
		  if(System.currentTimeMillis() - startTouchDown > MAX_MS_EDITWHILERUNNING) {
		    // Do nothing, just keep going !
		    //vibrate()
		    showActionBar()
		    vibrateCancel()
		    if(mGameView.editWhileRunning) Toast.makeText(this, R.string.editwhilerunningon, Toast.LENGTH_SHORT).show() else {
		      Toast.makeText(this, R.string.editwhilerunningoff, Toast.LENGTH_SHORT).show()
		    }
		    true
		  } else {
				mGameView.editWhileRunning = false
				mGameView.state match {
					case GameView.Editing =>
					  mGameView.toRunning()
					  hideActionBar()
					  time_button.setImageDrawable(timeButtonPause)
	
					case GameView.Running =>
					  mGameView.toEditing()
					  showActionBar()
					  if(mTts != null) mTts.stop()
					  time_button.setImageDrawable(timeButtonPlay)
				  }
				true
		  }
		case MotionEvent.ACTION_MOVE =>
          false
		case _ => false
    }
	
  }

  configurationObject {
    if(task != null) {
      if(mGameView != null) {
        task.game = (mGameView.getGame, mGameView.getBitmaps())
      }
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
  import scala.language.reflectiveCalls //???
  def !(m: Message) = mHandler ! m
  
  class RichHandler(delay: Long, handler: Handler) {
    def !(m: Message) = handler.sendMessageDelayed(m, delay)
  }
  /**
   * Handler for working with asynchronous messages.
   */
  private var mHandler = new Handler(){
    import Messages._
    def !(m: Message): Boolean = sendMessage(m)
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
          task = new LoadSaveGameTask(self, getLanguage(), saving=false, exporting=false,game=(mGameView.getGame, Nil))
          task.execute(tempName)
          
        case FileSave(tempName) =>
          this ! ShowProgressDialogSave()
          mGameView.getGame().validateNextToCurrent()
          task = new LoadSaveGameTask(self, getLanguage(), saving=true, exporting=false, game=(mGameView.getGame, mGameView.getBitmaps))
          task.execute(tempName)
          
        case FileSaveAndExport(tempName) =>
          this ! ShowProgressDialogSave()
          mGameView.getGame().validateNextToCurrent()
          task = new LoadSaveGameTask(self, getLanguage(), saving=true, exporting=true, game=(mGameView.getGame, mGameView.getBitmaps))
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
          val settings = getSharedPreferences(PREFS_NAME, 0)
          val editor = settings.edit()
          editor.putBoolean("startTutorial", false)
          editor.commit()

        case PickImage() =>
          val pickIntent = new Intent()
          pickIntent.setType("image/*")
          pickIntent.setAction(Intent.ACTION_GET_CONTENT)
          var photoIntent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE)
          
          val pickTitle = getResources().getString(R.string.select_or_take_picture)
          val chooserIntent = Intent.createChooser(pickIntent, pickTitle)
          chooserIntent.putExtra(
              Intent.EXTRA_INITIAL_INTENTS,
              Array[Intent](photoIntent))
          startActivityForResult(chooserIntent, IMPORT_PICT)
        
        case ReadAloud(language, msg) =>
          if(mTts != null) {
            if(language != "") {
              val locale  = new Locale(language)
              val isAvailable = mTts.isLanguageAvailable(locale)
              isAvailable match {
                case LANG_AVAILABLE | LANG_COUNTRY_AVAILABLE | LANG_COUNTRY_VAR_AVAILABLE =>
                  mTts.setLanguage(locale);
                case _ =>
              }
            }
            mTts.speak(msg, TextToSpeech.QUEUE_FLUSH, null);
          }

        case _ =>
            Log.w("kingpong","Warning: message type \""+input_msg.what+"\" not supported")
        }
    }
  }
  
  override  protected def onActivityResult(requestCode: Int, resultCode: Int, data: Intent) { 
        super.onActivityResult(requestCode, resultCode, data)
        requestCode match { 
        case IMPORT_PICT =>
            if(resultCode == Activity.RESULT_OK){
                try {
                    val imageUri = data.getData
                    if(imageUri == null) {
	                    val extras = data.getExtras();
							        val imageBitmap = extras.get("data").asInstanceOf[Bitmap];
							        mGameView.setImageSelectedShape(imageBitmap);
	                  } else {
	                    val imageStream = getContentResolver.openInputStream(imageUri)
	                    val selectedImage = BitmapFactory.decodeStream(imageStream)
	                    mGameView.setImageSelectedShape(selectedImage)
                    }
                } catch {
                  case e: FileNotFoundException =>
                    e.printStackTrace()
                }
 
            }
        case TTS_CHECK =>
          if(resultCode == 1 /*TextToSpeech.Engine.CHECK_VOICE_DATA_PASS*/) {
            // Succes, au moins un moteur de TTS a ete trouve, on l'instancie
            mTts = new TextToSpeech(this, mTtsListener)
            if (mTts.isLanguageAvailable(Locale.getDefault) == TextToSpeech.LANG_COUNTRY_AVAILABLE) {
            	mTts.setLanguage(Locale.getDefault)
          	}
	        } else {
            // Echec, aucun moteur n'a ete trouve, on propose e l'utilisateur d'en installer un depuis le Market
            val installIntent = new Intent()
            installIntent.setAction("android.speech.tts.engine.INSTALL_TTS_DATA" /*TextToSpeech.Engine.ACTION_INSTALL_TTS_DATA*/)
            startActivity(installIntent)
	        }
      }
    }
  
  def context = this
}
