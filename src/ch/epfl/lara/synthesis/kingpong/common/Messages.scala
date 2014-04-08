package ch.epfl.lara.synthesis.kingpong.common

import android.os.Message
import language.implicitConversions

/***
 * Message Builder and unapply methods for easy pattern matching.
 */
object Messages {
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
  final val PICK_IMAGE = 11
  final val FILENAME_TAG = "filename"
  final val TEXT_TAG = "progress_text"
  final val LINE_TAG = "progress_line"
  final val TOTAL_TAG = "progressTotal"
    
  implicit def toMessage(what: Int): Message = {
    val m = Message.obtain()
    m.what = what
    m
  }
  
  implicit def toRichMessage(what: Int): toRichMessage = {
    new toRichMessage(toMessage(what))
  }
  
  implicit class toRichMessage(that: Message) {
    def !(keyvalue: (String, String)): Message = {
      that.getData.putString(keyvalue._1, keyvalue._2)
      that
    }
    def !(keyvalue: =>(String, Int)): Message = {
      that.getData.putInt(keyvalue._1, keyvalue._2)
      that
    }
  }
  
  def check(m: Message, what: Int): Option[Nil.type] = if(m.what == what) Some(Nil) else None
    
  object ShowProgressDialog {
    def unapplySeq(m: Message): Option[Nil.type] = check(m, SHOW_PROGRESS_DIALOG)
    def apply(): Message = SHOW_PROGRESS_DIALOG
  }
  
  object ShowProgressDialogSave {
    def unapplySeq(m: Message): Option[Nil.type] = check(m, SHOW_PROGRESS_DIALOG_SAVE)
    def apply(): Message = SHOW_PROGRESS_DIALOG_SAVE
  }
  
  object HideProgressDialog {
    def unapplySeq(m: Message): Option[Nil.type] = check(m, HIDE_PROGRESS_DIALOG)
    def apply(): Message = HIDE_PROGRESS_DIALOG
  }
  
  object TextProgressDialog {
    def unapply(m: Message): Option[(String, Int, Int)] = {
      if(m.what == TEXT_PROGRESS_DIALOG) {
        val data = m.getData()
        Some((data.getString(TEXT_TAG), data.getInt(LINE_TAG), data.getInt(TOTAL_TAG)))
      } else None
    }
    def apply(s: String, l: Int, t: Int): Message = TEXT_PROGRESS_DIALOG ! TEXT_TAG->s ! LINE_TAG->l ! TOTAL_TAG->t
    def apply(arg: (String, Int, Int)): Message = apply(arg._1, arg._2, arg._3)
  }
  
  object FileLoad {
    def unapply(m: Message): Option[(String)] = {
      if(m.what == FILE_LOAD) {
        val data = m.getData()
        Some((data.getString(FILENAME_TAG)))
      } else None
    }
    def apply(s: String): Message = FILE_LOAD ! FILENAME_TAG->s
  }
  
  object FileSave {
    def unapply(m: Message): Option[(String)] = {
      if(m.what == FILE_SAVE) {
        val data = m.getData()
        Some((data.getString(FILENAME_TAG)))
      } else None
    }
    def apply(s: String): Message = FILE_SAVE ! FILENAME_TAG->s
  }
  
  object FileSaveAndExport {
    def unapply(m: Message): Option[(String)] = {
      if(m.what == FILE_SAVE_AND_EXPORT) {
        val data = m.getData()
        Some((data.getString(FILENAME_TAG)))
      } else None
    }
  }
  def apply(s: String): Message = FILE_SAVE_AND_EXPORT ! FILENAME_TAG->s
  
  object FileExport {
    def unapply(m: Message): Option[(String)] = {
      if(m.what == FILE_EXPORT) {
        val data = m.getData()
        Some((data.getString(FILENAME_TAG)))
      } else None
    }
    def apply(s: String): Message = TEXT_PROGRESS_DIALOG ! FILENAME_TAG->s
  }
  
  object ShowInitialTooltip {
    def unapply(m: Message): Option[Nil.type] = check(m, SHOW_INITIAL_TOOLTIP)
    def apply(): Message = SHOW_INITIAL_TOOLTIP
  }
  
  object CancelTutorial {
    def unapply(m: Message): Option[Nil.type] = check(m, CANCEL_TUTORIAL)
    def apply(): Message = CANCEL_TUTORIAL
  }
  
  object PickImage {
    def unapply(m: Message): Option[Nil.type] = check(m, PICK_IMAGE)
    def apply(): Message = PICK_IMAGE
  }
}