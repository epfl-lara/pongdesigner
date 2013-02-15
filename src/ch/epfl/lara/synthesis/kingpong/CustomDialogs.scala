package ch.epfl.lara.synthesis.kingpong

import android.content.Context
import android.widget.EditText
import android.app.AlertDialog
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Paint
import android.view.View
import android.view.View.MeasureSpec
import android.widget.TextView
import android.content.DialogInterface
import ch.epfl.lara.synthesis.kingpong.ast._

object CustomDialogs {
  /**
   * Launches a new OK Cancel dialog with optional EditText.
   * If OK, calls the functionOK with the entered text if any.
   * If canceled, calls the funcCanceled with the entered text.
   **/
  def launchOKCancelDialog(
      context: Context,
      title: String,
      message: String,
      needInput: Boolean,
      funcOK: String => Unit,
      funcCanceled: String => Unit) {
    val alert = new AlertDialog.Builder(context)

    alert.setTitle(title)
    alert.setMessage(message)
    
    // Set an EditText view to get user input 
    val input = if(needInput) {
      val input = new EditText(context)
      alert.setView(input)
      input
    } else null
    
    alert.setPositiveButton("OK", new DialogInterface.OnClickListener() {
      def onClick(dialog: DialogInterface, whichButton: Int) {
        if(needInput) funcOK(input.getText().toString()) else funcOK(null)
      }
    })
    
    alert.setNegativeButton("Cancel", new DialogInterface.OnClickListener() {
      def onClick(dialog: DialogInterface, whichButton: Int) {
        if(needInput) funcCanceled(input.getText().toString()) else funcCanceled(null)
      }
    })
    alert.show()
  }
  
  /**
   * Launches a new OK Cancel dialog with a string array from which the user chooses a value.
   * The array is supplied as a resource integer.
   * If the first value is chosen, opens a custom dialog to read a custom value entered by the user.
   **/
  def launchChoiceDialog(context: Context, title: String, array: Int, funcOK: String => Unit, funcCanceled: () => Unit) {
    val alert = new AlertDialog.Builder(context)

    alert.setTitle(title)
    alert.setItems(array, new DialogInterface.OnClickListener() {
       def onClick(dialog: DialogInterface, which: Int) {
         // The 'which' argument contains the index position
         // of the selected item
         val string_array = context.getResources().getStringArray(array)
         if(which == 0) {
           launchOKCancelDialog(context, title, "", true, funcOK, (_ => funcCanceled())) // the user clicked on custom
         } else {
           funcOK(string_array(which))
         }
       }
    });
    alert.show()
  }

  /**
   * Launches a new OK Cancel dialog with a string array from which the user chooses a value.
   * The array is given directly.
   * If the first value is chosen, opens a custom dialog to read a custom value entered by the user.
   **/
  def launchChoiceDialog(context: Context, title: String, array: List[CharSequence], funcOK: Int => Unit, funcCanceled: () => Unit) {
    val alert = new AlertDialog.Builder(context)

    alert.setTitle(title)
    alert.setItems(array.toArray, new DialogInterface.OnClickListener() {
       def onClick(dialog: DialogInterface, which: Int) {
         // The 'which' argument contains the index position
         // of the selected item
         funcOK(which)
       }
    });
    alert.show()
  }
  
  /** Launches a new OK Cancel dialog with a list of code lines from which the user choose among equivalence classes. */
  def launchRuleMakerDialog(context: Context, title: String, message1: String, message2: String, originalRule: ReactiveRule,
      codeFragmentsStrings:List[CharSequence],
      codeFragments:List[Expression],
      equivalences: List[Int],
      funcAccept: ReactiveRule => Unit, funcCanceled: () => Unit) {
    val alert = new AlertDialog.Builder(context)

    alert.setTitle(message1)
    var chosen = 0
    //alert.setMessage(message1)// The header of the rule
    //alert.setMessage(message)
    // Checks the first items of each code.
    def buildCheckList(code: List[Expression], equivalence: List[Int], current: Int, result:List[Boolean]): Array[Boolean] = (code, equivalence) match {
      case (Nil, Nil) => result.reverse.toArray
      case ((a::q), (b::p)) if b == current => buildCheckList(q, p, current, false::result)
      case ((a::q), (b::p)) => buildCheckList(q, p, b, true::result)
      case (_, _) => throw new Exception("The list code " + code + " and equivalence " + equivalence + " do not have the same size")
    }
    
    var selected = buildCheckList(codeFragments, equivalences, -1, Nil)
    
    // Set an multi choice view to get user input 
    alert.setMultiChoiceItems(codeFragmentsStrings.toArray, selected, new DialogInterface.OnMultiChoiceClickListener() {
      def onClick(dialog: DialogInterface, which: Int, checked: Boolean) {
        chosen = which
        selected(which) = checked
        val e = equivalences(which)
        var i = 0
        // checks the rules by equivalence.
        equivalences foreach { eqvalue =>
          if(eqvalue == e) {
            if(i != which) {
              selected(i) = false
              dialog.asInstanceOf[AlertDialog].getListView().setItemChecked(i, false)
            } else {
              selected(i) = true;
              dialog.asInstanceOf[AlertDialog].getListView().setItemChecked(i, true)
            }
          }
          i += 1
        }
      }
    })
    //alert.setMessage(message2)// The footer of the rule
    
    alert.setPositiveButton("OK", new DialogInterface.OnClickListener() {
      def onClick(dialog: DialogInterface, which: Int) {
        // Reorder the rules.
        var i = 0
        var eqvalueprev = -1
        var currentCodeList: List[Expression] = Nil
        var totalCode: List[Expression] = Nil
        var max = equivalences.size
        
        /**
         * listCode: The list of flattened code to parse.
         * equivalenceClass: The integer indexes of equivalence classes
         * listCodeString: the list of the flattened code that is converted to string
         * previousCode: the list of already generated code
         * bufferCode: list of code lines in the same equivalence class.
         * eqvalueprev: the current equivalence class
         */
        def foldCode(
            expandedCodeLines: List[Expression],
            expandedEqClasses: List[Int],
            expandedStrings: List[String],
            selected: List[Boolean],
            foldedCode:List[Expression],
            codeGathered:List[Expression],
            codeGatheredEqClass: Int): (List[Expression], List[Int], List[String], List[Boolean], List[Expression]) = {
          (expandedCodeLines, expandedEqClasses, expandedStrings, selected) match {
            case (IfCode(condition, Nil, Nil)::remaining_line_code, equ1::remaining_equivalenceClass, code::remaining_codeString, s::remaining_selected) =>
              val (l, e, c, s, codeIfTrue) = foldCode(remaining_line_code, remaining_equivalenceClass, remaining_codeString, remaining_selected, Nil, Nil, equ1)
              (l, e, c, s) match {
                case (Expression.NONE::remaining_line_code, equ2::remaining_equivalenceClass, code2::remaining_codeString, s::remaining_selected) if code2.contains("} else {") => 
                  val (l2, e2, c2, s2, codeIfFalse) = foldCode(remaining_line_code, remaining_equivalenceClass, remaining_codeString, remaining_selected, Nil, Nil, equ2) // Stops at the "}"
                  foldCode(l2, e2, c2, s2, foldedCode ++ List(IfCode(condition, codeIfTrue, codeIfFalse)), Nil, equ2)
                case (Expression.NONE::remaining_line_code, equ2::remaining_equivalenceClass, code2::remaining_codeString, s::remaining_selected) if code2.contains("}") => 
                  foldCode(remaining_line_code, remaining_equivalenceClass, remaining_codeString, remaining_selected, foldedCode ++ List(IfCode(condition, codeIfTrue, Nil)), Nil, equ2)                  
                case _ =>
                  throw new Exception("Should not happen with " + (l, e, c))
              }
            case (Expression.NONE::remaining_line_code, e::remaining_equivalenceClass, code::remaining_codeString, s::remaining_selected)
            if code.contains("} else {") =>
              (expandedCodeLines, expandedEqClasses, expandedStrings, selected, foldedCode ++ (if(codeGathered != Nil) ParallelExpressions(codeGathered)::Nil else Nil))
            case (Expression.NONE::remaining_line_code, e::remaining_equivalenceClass, code::remaining_codeString, s::remaining_selected)
            if code.contains("}") =>
              (expandedCodeLines, expandedEqClasses, expandedStrings, selected, foldedCode ++ (if(codeGathered != Nil) ParallelExpressions(codeGathered)::Nil else Nil))
            case (codeLine::remaining_line_code, eqvalue::remaining_equivalenceClass, code::remaining_codeString, s::remaining_selected) if eqvalue == codeGatheredEqClass =>
              // Continues the previous parallel group
              foldCode(remaining_line_code, remaining_equivalenceClass, remaining_codeString, remaining_selected, foldedCode, if(s) (codeLine :: codeGathered) else codeGathered ++ List(codeLine), eqvalue)
            case (codeLine::remaining_line_code, eqvalue::remaining_equivalenceClass, code::remaining_codeString, s::remaining_selected) if eqvalue != codeGatheredEqClass =>
              // Finishes the previous parallel group and starts a new one
              foldCode(remaining_line_code, remaining_equivalenceClass, remaining_codeString, remaining_selected, foldedCode ++ (if(codeGathered != Nil) ParallelExpressions(codeGathered)::Nil else Nil), codeLine::Nil, eqvalue)
            case (Nil, Nil, Nil, Nil) =>
              (Nil, Nil, Nil, Nil, foldedCode ++ (if(codeGathered != Nil) (ParallelExpressions(codeGathered)::Nil) else Nil))
            case _ =>
              throw new Exception("Problem while folding lists : " + (expandedCodeLines, expandedEqClasses, expandedStrings, selected).toString())
          }
        }
        
        val (l, e, s, sel, result) = foldCode(codeFragments, equivalences, codeFragmentsStrings map (_.toString()), selected.toList, Nil, Nil, -1)
       
        originalRule.code = result
        funcAccept(originalRule)
      }
    })
    alert.setNegativeButton("Cancel", new DialogInterface.OnClickListener() {
      def onClick(dialog: DialogInterface, whichButton: Int) {
        funcCanceled()
      }
    })
    alert.show()
  }
  
  /** Launches a new OK Cancel dialog with a list of rules */
  def launchRuleChooserDialog(context: Context, title: String, rules_string:List[CharSequence], rules:List[ReactiveRule], funcAccept: ReactiveRule => Unit, funcCanceled: () => Unit) {
    val alert = new AlertDialog.Builder(context)

    alert.setTitle(title)
    var chosen = 0
    //alert.setMessage(message)
    
    // Set an EditText view to get user input 
    alert.setSingleChoiceItems(rules_string.toArray, 0, new DialogInterface.OnClickListener() {
      def onClick(dialog: DialogInterface, which: Int) {
        chosen = which
      }
    })
    
    alert.setPositiveButton("OK", new DialogInterface.OnClickListener() {
      def onClick(dialog: DialogInterface, whichButton: Int) {
        if(chosen >= rules.size) {
          funcCanceled()
        } else {
          funcAccept(rules(chosen))
        }
      }
    })
    alert.setNegativeButton("Cancel", new DialogInterface.OnClickListener() {
      def onClick(dialog: DialogInterface, whichButton: Int) {
        funcCanceled()
      }
    })
    alert.show()
  } 
}