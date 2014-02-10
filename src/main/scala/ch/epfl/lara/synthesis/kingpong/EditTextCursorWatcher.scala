package ch.epfl.lara.synthesis.kingpong

import android.content.Context;
import android.util.AttributeSet;
import android.widget.EditText;
import android.widget.Toast;

class EditTextCursorWatcher(context: Context, attrs: AttributeSet,
            defStyle: Int) extends EditText(context, attrs, defStyle) {

    def this(context: Context, attrs: AttributeSet) = {
      this(context, attrs, 0)
    }

    def this(context: Context) {
      this(context, null)
    }

   private var mSelectionChangedListener: (Int, Int) => Unit = null
   def setOnSelectionChangedListener(f: (Int, Int) => Unit) = mSelectionChangedListener = f
    
   override protected def onSelectionChanged(selStart: Int, selEnd: Int) { 
     if(mSelectionChangedListener!=null) mSelectionChangedListener(selStart, selEnd)
   }
}