package ch.epfl.lara.synthesis.kingpong
import scala.language.dynamics
import scala.Dynamic
import ch.epfl.lara.synthesis.kingpong.expression.Trees._

trait Localization extends AnyRef { self =>
  type Language
  type Key
  type Output
  private var currentOutputs: Map[Key, Output] = Map()
  var outputs: Map[Language, Map[Key, Output]] = Map()
  var fallback: Map[Language, Language] = Map()
  private var mLanguage: Language = _
  def language: Language = mLanguage
  def language_=(newLanguage: Language) = {
    mLanguage = newLanguage;
    loadCurrentLanguage()
  }
  private def loadCurrentLanguage() {
    if(outputs != null && outputs.contains(mLanguage)) {
      currentOutputs = outputs(mLanguage)
    }
  }
  /** Abstract method*/
  def parentLanguage(language: Language): Option[Language]
  def fallbackOutput(key: Key): Output
  
  
  /** If the language has already been tested before, return the key.
   *  Re-test on fallback languages if available.
   *  Re-test on the country language (e.g. fr_FR => fr) if available
   */
  private def StrFallback(key: Key, language: Language, testedLanguages: List[Language] = Nil): Output = {
    if(testedLanguages.contains(language) || language == null) return fallbackOutput(key)
    if(fallback == null || !fallback.contains(language)) {
      parentLanguage(language) match {
        case Some(parent) => StrFallback(key, parent, language::testedLanguages)
        case _ => fallbackOutput(key)
      }
    } else {
      Str(key, fallback(language), language::testedLanguages)
    }
  }
  
  /** Converts a key to a language */
  object Str {
    def apply(key: Key, language: Language, testedLanguages: List[Language]): Output = {
	    if(outputs == null) return fallbackOutput(key)
	    outputs.get(language) match {
	      case Some(map: Map[Key, Output]) => map.get(key) match {
	        case Some(res) => res
	        case None => StrFallback(key, language, testedLanguages)
	      }
	      case None => StrFallback(key, language, testedLanguages)
	    }
	  }
    def apply(key: Key): Output = {
	    if(currentOutputs == null) loadCurrentLanguage()
	    if(currentOutputs == null) return fallbackOutput(key)
	    currentOutputs.get(key) match  {
	      case Some(res) => res
	      case None => StrFallback(key, mLanguage)
	    }
	  }
    
  }
}

trait StringLocalization extends Localization {
  type Language = String
  type Key = String
  type Output = String
  
  object Translate {
    def apply(key: String): Expr = MethodCall("Translate", List(StringLiteral(key)))
  }
  def parentLanguage(s: String): Option[String] = {
    if(s != null && s.length > 2) {
       Some(s.substring(0, 2))
    } else None
  }
  def fallbackOutput(key: String): String = key
}