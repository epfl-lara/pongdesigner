package ch.epfl.lara.synthesis.kingpong

import android.text.style.CharacterStyle
import android.text.SpannableStringBuilder
import android.text.Spannable
import android.text.style.StyleSpan
import android.graphics.Typeface
import android.text.SpannableString
import android.text.Spanned
import android.text.style.ForegroundColorSpan

object SyntaxColoring {
  def setSpanOnKeywords(text: CharSequence, keywords: Seq[String], cs: (()=>CharacterStyle)*): CharSequence = {
    // Start and end refer to the points where the span will apply
    val thetext = new SpannableString(text)
  
    val keywordpattern = ("\\b("+keywords.reduceLeft(_ + "|" + _)+")\\b").r
    for(m <- keywordpattern findAllMatchIn text) {
      for (c <- cs) thetext.setSpan(c(), m.start, m.end, Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
    }
    return thetext
  }
  def setSpanOnBounds(text: CharSequence, start: Int, end: Int, cs: (() => CharacterStyle)*): CharSequence = {
    val thetext = new SpannableString(text)
    for (c <- cs) thetext.setSpan(c(), start, Math.min(end, text.length), Spanned.SPAN_INCLUSIVE_EXCLUSIVE)
    thetext
  }
  /**
   * Gives certain styles to keywords
   */
  /*implicit class ToSpannableString(result: CharSequence) {
    def makeStyle: CharSequence = {
      val result1 = setSpanOnKeywords(result, LANGUAGE_SYMBOLS, () => new StyleSpan(Typeface.BOLD), () => new ForegroundColorSpan(0xFF950055))
      result1
    }
  }*/
}