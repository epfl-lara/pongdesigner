package ch.epfl.lara.synthesis.kingpong;

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

class GameDesignerActivity extends Activity with SensorEventListener {
    
    lazy private val mGameView = findViewById(R.id.gameview).asInstanceOf[GameEngine2DView]
    lazy private val mTimeButton = findViewById(R.id.time_button).asInstanceOf[ImageButton]
    lazy private val mBackButton = findViewById(R.id.back_button).asInstanceOf[ImageButton]
    lazy private val mGhostButton = findViewById(R.id.ghost_button).asInstanceOf[ImageButton]
    lazy private val mTimeBar = findViewById(R.id.time_bar).asInstanceOf[SeekBar]
    private var mFirstLaunch = true
    lazy private val mTimeButtonDrawable2 = getResources().getDrawable(R.drawable.timebutton2)
    lazy private val mTimeButtonDrawable = getResources().getDrawable(R.drawable.timebutton)
    lazy private val mGhostDrawable1 = getResources().getDrawable(R.drawable.ghost)
    lazy private val mGhostDrawable2 = getResources().getDrawable(R.drawable.ghostred)
    private var mSensorManager: SensorManager = null
    private var mAccelerometer: Sensor = null

    implicit def toOnclickListener(f: ()=>Unit):View.OnClickListener = {
      new View.OnClickListener{ override def onClick(v: View) = f() }
    }

    override def onCreate(savedInstanceState: Bundle) {
      super.onCreate(savedInstanceState);
      setContentView(R.layout.game_with_menu)
      mSensorManager = getSystemService(Context.SENSOR_SERVICE).asInstanceOf[SensorManager]
      mAccelerometer = mSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER)
      
      attachGame()
      mGameView.enterEditMode()
      mGameView.setKeepScreenOn(true)
      mGameView.setTimeBar(mTimeBar)
      //set background image as rgb565 to render the background faster and save memory
      val options: BitmapFactory.Options = new BitmapFactory.Options();    
      options.inPreferredConfig = Bitmap.Config.RGB_565;
      mGameView.setBackground(BitmapFactory.decodeResource(getResources(), R.drawable.back1, options)); //Background
      
      mTimeButton.setOnClickListener { () =>
        if(!mGameView.isInEditMode()) {
          mGameView.enterEditMode()
          mTimeButton.setImageDrawable(mTimeButtonDrawable2)
          unlockScreenOrientation()
        } else {
          mGameView.exitEditMode()
          switchGhostMode()
          mTimeButton.setImageDrawable(mTimeButtonDrawable)
          lockScreenOrientation()          
        }
      }
      mBackButton.setOnClickListener { () =>
        if(mGameView.isInEditMode()) {
          mGameView.reset()
        }
      }
      mGhostButton.setOnClickListener { () => switchGhostMode() }
  }
    
  var mScreenOrientationLocked = false
    
  def lockScreenOrientation() {
    if (!mScreenOrientationLocked) {
        val orientation = getResources().getConfiguration().orientation
        val rotation = getWindowManager().getDefaultDisplay().getOrientation()

        if (rotation == Surface.ROTATION_0 || rotation == Surface.ROTATION_90) {
            if (orientation == Configuration.ORIENTATION_PORTRAIT) {
                setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_REVERSE_PORTRAIT);
            }
            else if (orientation == Configuration.ORIENTATION_LANDSCAPE) {
                setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
            }
        }
        else if (rotation == Surface.ROTATION_180 || rotation == Surface.ROTATION_270) {
            if (orientation == Configuration.ORIENTATION_PORTRAIT) {
                setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
            }
            else if (orientation == Configuration.ORIENTATION_LANDSCAPE) {
                setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_REVERSE_LANDSCAPE);
            }
        }

        mScreenOrientationLocked = true;
    }
  }
  
  def unlockScreenOrientation() {
      setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_SENSOR);
      mScreenOrientationLocked = false;
  }
    
  def switchGhostMode() {
    if(!mGameView.isInGhostMode() && mGameView.isInEditMode()) {
        mGameView.activateGhostMode()
        mGhostButton.setImageDrawable(mGhostDrawable2)
      } else {
        mGameView.deactivateGhostMode()
        mGhostButton.setImageDrawable(mGhostDrawable1)
      }
  }
    
  override def onCreateOptionsMenu(menu: Menu): Boolean = {
      val inflater = getMenuInflater()
      inflater.inflate(R.menu.game_menu, menu);
      return true;
  }
  
  override def onOptionsItemSelected(item: MenuItem): Boolean = {
      // Handle item selection
      item.getItemId() match {
          case R.id.save =>
            mGameView.outputGame()
            true
          case R.id.reset =>
            val context = this
            val res = context.getResources()
            CustomDialogs.launchOKCancelDialog(context, res.getString(R.string.confirm_reset_title), res.getString(R.string.confirm_reset_message), false, 
                { _ =>
                  mGameView.setGame(new PongGameEmpty())
                  mGameView.reset()
                },
                { _ => }
            )
            
            true
          case R.id.saved_game_1 =>
            mGameView.setGame(new PongFibonacci())
            mGameView.reset()
            true
          case R.id.saved_game_2 =>
            mGameView.setGame(new PongGameSyracuse())
            mGameView.reset()
            true
          case R.id.saved_game_3 =>
            mGameView.setGame(new PongGameComplete())
            mGameView.reset()
            true
          case R.id.saved_game_4 =>
            mGameView.setGame(new PongMaze())
            mGameView.reset()
            true
          case R.id.saved_game_5 =>
            mGameView.setGame(new PongEscape())
            mGameView.reset()
            true
          case R.id.saved_game_6 =>
            mGameView.setGame(new PongGamePacman())
            mGameView.reset()
            true
          case R.id.saved_game_7 =>
            mGameView.setGame(new PongGCDCanvas())
            mGameView.reset()
            true
          case R.id.saved_game_8 =>
            mGameView.setGame(new PongBalloon())
            mGameView.reset()
            true
          case R.id.saved_game_9 =>
            mGameView.setGame(new PongBrickBreaker3Players())
            mGameView.reset()
            true
          case _ =>
              super.onOptionsItemSelected(item)
      }
  }
  
  override def onPause() = {
    super.onPause();
    if(mGameView != null) mGameView.onPause()
    mSensorManager.unregisterListener(this)
  }
    
  override def onResume() = {
    super.onResume();
    if(mGameView != null && !mFirstLaunch) mGameView.onResume()
    mFirstLaunch = false
    mSensorManager.registerListener(this, mAccelerometer, SensorManager.SENSOR_DELAY_NORMAL)
  }
   
  def attachGame() = {
     if(!mGameView.hasGame()) {
       var game = getLastNonConfigurationInstance().asInstanceOf[Game]
       if(game == null) game = new PongGameEmpty()
       mGameView.setGame(game)
     }
   }
  
  override def onRetainNonConfigurationInstance(): Object = {
    mGameView.getGame()
  }
  
  val gravity = new Array[Float](3)
  
  /**
   * Sensor events
   */
  def onAccuracyChanged(sensor: Sensor, accuracy: Int) {
    
  }
  
  /**
   * Sensor events
   */
  def onSensorChanged(event: SensorEvent) = {
    val alpha = 0.8f
    
    gravity(0) = alpha * gravity(0) + (1 - alpha) * event.values(0)
    gravity(1) = alpha * gravity(1) + (1 - alpha) * event.values(1)
    gravity(2) = alpha * gravity(2) + (1 - alpha) * event.values(2)
    
    val rotation = getWindowManager().getDefaultDisplay().getOrientation()

    rotation match {
      case Surface.ROTATION_0 =>
        mGameView.set2DAcceleration(gravity(0), gravity(1))
      case Surface.ROTATION_90 =>
        mGameView.set2DAcceleration(-gravity(1), gravity(0))
      case Surface.ROTATION_180 =>
        mGameView.set2DAcceleration(-gravity(0), -gravity(1))
      case Surface.ROTATION_270 =>
        mGameView.set2DAcceleration(gravity(1), -gravity(0))
      case _ =>
        
    }
    
  }
}
