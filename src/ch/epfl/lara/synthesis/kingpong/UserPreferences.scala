package ch.epfl.lara.synthesis.kingpong

import android.content.Context
import android.os.Bundle
import android.preference.CheckBoxPreference
import android.preference.ListPreference
import android.preference.Preference
import android.preference.PreferenceActivity
import android.preference.Preference.OnPreferenceChangeListener
import java.util.Locale
import android.widget.Toast
import android.content.res.Configuration
import android.util.Log

object Options {
  object Access {
    var showTooltips = true
  }
  object Event {
    var selectableAreaRadius = 30.0f
  }
}

/**
 * UserPreferences stores utilities to retrieve options for users.
 */
object UserPreferences {
  /** Name of the global preferences file */
  final val GLOBAL_OPTIONS = "globaloptions"
    
  /**
   * Per-user preferences
   */
  
  final val TOOLTIPS_KEY = "tooltips"
  final val LOCALE_KEY = "locale"

  def PREFERENCES_FILE :String = {
    "PREFERENCES"
  }

  /**
   * Updates the preferences for this particular user.
   */
  def update(context: Context) = {
    val prefs: android.content.SharedPreferences = context.getSharedPreferences(PREFERENCES_FILE, Context.MODE_PRIVATE)
    updateTooltips(prefs.getBoolean(TOOLTIPS_KEY, true))
  }
  
  def updateTooltips(b: Boolean) = {
    Options.Access.showTooltips = b
  }
}

/**
 * Displays the user preferences and allows for modifications
 */
class UserPreferences extends PreferenceActivity with OnPreferenceChangeListener {
  import UserPreferences._
  
  /**
   * Available preferences
   */

  var mTooltipsOption: CheckBoxPreference = null
  var mLocaleOption: ListPreference = null
  
  /**
   * onCreate method.
   */
  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    
    getPreferenceManager().setSharedPreferencesName(PREFERENCES_FILE)
    addPreferencesFromResource(R.xml.prefs)
    
    val prefs = getPreferenceScreen()
    
    mTooltipsOption = prefs.findPreference(TOOLTIPS_KEY).asInstanceOf[CheckBoxPreference]
    mLocaleOption = prefs.findPreference(LOCALE_KEY).asInstanceOf[ListPreference]
    
    mTooltipsOption.setOnPreferenceChangeListener(this)
    mLocaleOption.setOnPreferenceChangeListener(this)
  }
  
  /**
   * Called whenever a preference is changed.
   */
  def onPreferenceChange(preference: Preference, newValue: Object): Boolean = {
    preference.getKey() match {
      case TOOLTIPS_KEY =>
        val tooltips_value = newValue.asInstanceOf[Boolean]
        updateTooltips(tooltips_value)
        true
      case LOCALE_KEY =>
        val sign = newValue.asInstanceOf[String]
        val locale = new Locale(sign); 
        val config = new Configuration()
        if(sign != Locale.getDefault().getLanguage()) {
          val previous = Locale.getDefault().getLanguage()
          config.locale = locale;
          getBaseContext().getResources().updateConfiguration(config, getBaseContext().getResources().getDisplayMetrics());
          Log.d("UserPreferences.scala", s"Locale set from $previous to $sign")
        }
        true
      case _ =>
        false
    }
  }
}