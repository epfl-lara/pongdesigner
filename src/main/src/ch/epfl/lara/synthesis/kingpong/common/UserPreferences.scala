package ch.epfl.lara.synthesis.kingpong.common

import android.content.Context
import android.os.Bundle
import android.preference.CheckBoxPreference
import android.preference.ListPreference
import android.preference.Preference
import android.preference.PreferenceActivity
import android.preference.Preference.OnPreferenceChangeListener
import ch.epfl.lara.synthesis.kingpong.R

object Options {
  object Access {
    var showTooltips = true
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
  
  /**
   * onCreate method.
   */
  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    
    getPreferenceManager().setSharedPreferencesName(PREFERENCES_FILE)
    addPreferencesFromResource(R.xml.prefs)
    
    val prefs = getPreferenceScreen()
    
    mTooltipsOption = prefs.findPreference(TOOLTIPS_KEY).asInstanceOf[CheckBoxPreference]

    mTooltipsOption.setOnPreferenceChangeListener(this)
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
    }
  }
}