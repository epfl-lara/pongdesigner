package ch.epfl.lara.synthesis.kingpong.menus

import scala.collection.mutable.HashMap
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong._
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.drawable.Drawable
import android.content.Context
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import android.graphics.RectF
import android.graphics.Rect
import android.graphics.Paint
import android.text.TextPaint
import android.graphics.Typeface
import android.util.Log
import android.graphics.Matrix
/*
object RuleMenu extends MenuCenter {
  menus = List(MoveRuleButton, EditRuleButton, ApplyRuleButton, TrashRuleButton)
  def draw(canvas: Canvas, gameEngine: GameView, selectedShape: GameObject, bitmaps: HashMap[Int, Drawable], cx: Float, cy: Float): Unit = {
    
    //RenameButtonRule.setText(selectedShape.mName)
    //RenameButtonRule.setPos(gameEngine.whitePaint, 33f/49f, 0, top_shift-1)
    MoveButton.setPos(0, 0)
    EditRuleButton.setPos(0, -1)
    ApplyRuleButton.setPos(1, -1)
    TrashRuleButton.setPos(2, -1)
    for(menu <- menus) {
      menu.draw(canvas, gameEngine, selectedShape, bitmaps, cx, cy)
    }
  }
}*/
/*
/** Buttons that allows to move the rule. */
object MoveRuleButton extends MenuButton {
  import MenuOptions._
  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    val selectedRule = gameEngine.ruleEditor.selectedRule
    if(selectedRule != null) {
      selectedRule.x = Math.floor((selectedRule.x.toFloat + selectedRule.height/2)/selectedRule.height).toInt * selectedRule.height
      selectedRule.y = Math.floor((selectedRule.y.toFloat + selectedRule.width/2)/selectedRule.width).toInt * selectedRule.width
      hovered = false
    }
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    val selectedRule = gameEngine.ruleEditor.selectedRule
    if(selectedRule != null) {
      selectedRule.x = selectedRule.x + shiftX.toInt
      selectedRule.y = selectedRule.y + shiftY.toInt
    }
  }
  
  def icons(gameEngine: GameView, selectedShape: GameObject) = List(R.drawable.cross_move)
  
  def hint_id = R.string.change_position_hint
}

/** Sends a rule to trash*/
object TrashRuleButton extends MenuButton {
  import MenuOptions._
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    val res = context.getResources()
    CustomDialogs.launchOKCancelDialog(context,
        String.format(res.getString(R.string.delete_title), "this rule"),
        res.getString(R.string.confirm_delete), false, { _ =>
          gameEngine.getGame().deleteRule(gameEngine.ruleEditor.selectedRule);
          gameEngine.ruleEditor.unselect()
          gameEngine.setModeModifyGame(false)}, {_ => ()})
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: 
       R.drawable.trashcan ::  Nil

  def hint_id = R.string.change_trash_hint
}

/** Sends a rule to the editor */
object EditRuleButton extends MenuButton {
  import MenuOptions._
  var selected = false
  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    if(selected) {
      gameEngine.getGame().restorePrev()
      //AddRuleButton.onFingerUp(gameEngine, selectedShape, x, y)
      gameEngine.setModeModifyGame(true)
      selected = false
    } else {
      if(gameEngine.ruleEditor.selectedRule != null) {
        gameEngine.ruleEditor.selectedRule.execute(gameEngine.getGame().context, false)
        gameEngine.setModeSelectEffects()
        selected = true
      }
    }
    
    //CodeGenerator.modifyAndInsertRule(context, gameEngine.getGame(), gameEngine.ruleEditor.selectedRule, -1, {rule =>
      //Replay the rule and select its effects.
      // Need to select an event corresponding to the rule.
    //  if(gameEngine.ruleEditor.selectedRule != null) gameEngine.ruleEditor.selectedRule.opened = false
    //})
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }
  
 def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) (if(selected) R.drawable.flat_button_selected_highlighted else R.drawable.flat_button_highlighted) else (if(selected) R.drawable.flat_button_selected else R.drawable.flat_button)) :: R.drawable.existing_rules :: Nil
    
 def hint_id = R.string.change_rule_hint
}

/** Applies the rule */
object ApplyRuleButton extends MenuButton {
  import MenuOptions._
  
  override def onFingerUp(gameEngine: GameView, selectedShape: GameObject, x: Float, y: Float) = {
    if(hovered) {
      gameEngine.getGame().restorePrev()
      if(gameEngine.ruleEditor.selectedRule != null) {
        gameEngine.ruleEditor.selectedRule.execute(gameEngine.getGame().context, true)
        if(EditRuleButton.selected) {
          gameEngine.ruleEditor.selectedRule.execute(gameEngine.getGame().context, false)
        }
      }
    }
    hovered = false
  }
  
  override def onFingerMove(gameEngine: GameView, selectedShape: GameObject, relativeX: Float, relativeY: Float, shiftX: Float, shiftY: Float, mDisplacementX: Float, mDisplacementY: Float) = {
    // Nothing
  }
  
  def icons(gameEngine: GameView, selectedShape: GameObject) =
    (if(hovered) R.drawable.flat_button_highlighted else R.drawable.flat_button) :: R.drawable.timebutton3 :: Nil

 def hint_id = R.string.play_rule_hint
}

/**
 * Handles code editing infrastructure.
 */
object CodeEditorHandle {
  final val ROUND_RECT_RADIUS = 5

  var rectFData = new RectF(0, 0, 0, 0)
  var rectData = new Rect(0, 0, 0, 0)
  var defaultPaint = new Paint()
  defaultPaint.setColor(0xFF00FFFF)
  defaultPaint.setStyle(Paint.Style.FILL_AND_STROKE)
  defaultPaint.setStrokeWidth(2)
  defaultPaint.setAntiAlias(true)
  var defaultStrokePaint = new Paint()
  defaultStrokePaint.set(defaultPaint)
  defaultStrokePaint.setStrokeWidth(2)
  defaultStrokePaint.setStyle(Paint.Style.STROKE)
  
  var actionPaint = new Paint()
  actionPaint.setStrokeWidth(4)
  actionPaint.setColor(0xAAAAAAFF)
  actionPaint.setStyle(Paint.Style.FILL)
  actionPaint.setAntiAlias(true)
  
  val mCodePaint = new TextPaint()
  mCodePaint.setTypeface(Typeface.MONOSPACE)
  mCodePaint.setAntiAlias(true)
  mCodePaint.setSubpixelText(true)
  mCodePaint.setColor(0xFF000000)
  
  var c = new Matrix()
  val c_array = new Array[Float](9)
  
  def drawRuleCode(canvas: Canvas, gameEngine: GameView, selectedRule: ReactiveRule): Unit = {
    drawRule(canvas, gameEngine, selectedRule)
    var i = 1
    canvas.getMatrix(c)
    c.getValues(c_array)
    val scale = c_array(Matrix.MSCALE_X)
    var box_width = getBoxWidth(scale)
    
    // Draw the background, the action markers below, and then the text of the rule
    if(EditRuleButton.selected && gameEngine.ruleEditor.selectedRuleStringSplit != null) {
      for(line <- gameEngine.ruleEditor.selectedRuleStringSplit) {
        drawCodeLineBelowRule(canvas, selectedRule, scale, line, i, color_selected, true, false)
        i += 1
      }
      
      for(action <- gameEngine.ruleEditor.selectedRuleActions) {
        val line = action.line
        val column = action.column
        val length = action.length
        val x1 = selectedRule.x + 8 + column * box_width
        val y1 = selectedRule.y + line * selectedRule.height + 4
        val x2 = x1 + length * box_width
        val y2 = y1 + selectedRule.height - 8
        rectFData.set(x1, y1, x2, y2)
        action match {
          case ActionChangeParallelCode(p, _, _, _) =>
            canvas.drawRoundRect(rectFData, ROUND_RECT_RADIUS, ROUND_RECT_RADIUS, actionPaint)
          case ActionModifyColor(p, _, _, _) =>
            defaultPaint.setColor(p.number_value.toInt)
            canvas.drawRoundRect(rectFData, ROUND_RECT_RADIUS, ROUND_RECT_RADIUS, defaultPaint)
          case ActionModifyConstant(p, _, _, _) =>
            canvas.drawRoundRect(rectFData, ROUND_RECT_RADIUS, ROUND_RECT_RADIUS, actionPaint)
          case ActionDeleteLineNumber(_, _, _, _) =>
            canvas.drawRoundRect(rectFData, ROUND_RECT_RADIUS, ROUND_RECT_RADIUS, actionPaint)
            defaultStrokePaint.setColor(0xFFFF0000)
            canvas.drawLine(x1, y1, x2, y2, defaultStrokePaint)
            canvas.drawLine(x2, y1, x1, y2, defaultStrokePaint)
        }
        action.x1 = x1
        action.y1 = y1
        action.x2 = x2
        action.y2 = y2
      }
      
      i = 1 // Restart
      //selectedRule.selectionRectangle.set(selectedRule.x, selectedRule.y+selectedRule.height, selectedRule.x + selectedRule.width, selectedRule.y + selectedRule.height)
      for(line <- gameEngine.ruleEditor.selectedRuleStringSplit) {
        val length = drawCodeLineBelowRule(canvas, selectedRule, scale, line, i, color_selected, false, true)
        i += 1
      }
      // Draw rectangles around the numbers that can be changed.
    }
  }

  val color_notselected = 0xAAAA8822
  val color_selected = 0xAAAAFFFF
  
  def background_rule(gameEngine: GameView, rule: ReactiveRule): Int = if(rule == gameEngine.ruleEditor.selectedRule) {
    color_selected
  } else {
    if(gameEngine.shapeEditor.selectedShape != null && rule.containsVisible(gameEngine.shapeEditor.selectedShape)) {
      color_selected
    } else {
      color_notselected
    }
  }
  
  def drawBackgroundRectangle(canvas: Canvas, x: Float, y: Float, width: Float, height: Float, color: Int = 0xFFFF0000, border: Boolean = true) = {
    defaultPaint.setColor(color)
    rectFData.set(x, y, x + width, y + height)
    canvas.drawRoundRect(rectFData, ROUND_RECT_RADIUS, ROUND_RECT_RADIUS, defaultPaint)
    if(border) {
      defaultStrokePaint.setColor(color & 0xFFFF00FF)
      canvas.drawRoundRect(rectFData, ROUND_RECT_RADIUS, ROUND_RECT_RADIUS, defaultStrokePaint)
    }
  }

  
  def drawRule(canvas: Canvas, gameEngine: GameView, rule: ReactiveRule) = {
    if(rule != null) {
      val color_background = background_rule(gameEngine, rule)
      drawBackgroundRectangle(canvas, rule.x, rule.y, rule.width, rule.height,color_background)
      
      // Minimum : Fill with the shape(s) on which the rule applies,
      // Minimum : Then add the icon of the event.
      //This will appear monospace
      //canvas.drawText("foo", 10, 10, mCodePaint);
      rectData.set(rule.x+4, rule.y+4, rule.x + rule.width - 4, rule.y + rule.height - 4)
      mCodePaint.setTextSize(ReactiveRule.size*2f/3f)
      
      rule.selectionRectangle.set(rule.x+4, rule.y+4, rule.x + rule.width - 4, rule.y + rule.height - 4)
      
      rule match {
        case w@WhenEverRule(condition, code) =>
          // TODO : Display the text of the condition + the name of the rule
          // Maybe a different icon ?
          val outscreenBitmap = gameEngine.bitmaps(R.drawable.outscreen)
          outscreenBitmap.setAlpha(0xFF)
          outscreenBitmap.setBounds(rectData)
          outscreenBitmap.draw(canvas)
          if(w.shape != null) drawNameBeforeRule(canvas, rule, w.shape.name, color_background)
        case WhenFingerMovesOnRule(EIdentShape(s1), coords, code) =>
          // TODO : Display the GameObject + a finger movement icon
          val miniFingerMove =  gameEngine.bitmaps(R.drawable.fingermove)
          miniFingerMove.setBounds(rectData)
          miniFingerMove.draw(canvas) 
          drawNameBeforeRule(canvas, rule, s1.name, color_background)
        case WhenFingerDownOnRule(EIdentShape(s), code) =>
          val miniFingerDown =  gameEngine.bitmaps(R.drawable.fingerdown)
          miniFingerDown.setBounds(rectData)
          miniFingerDown.draw(canvas)
          drawNameBeforeRule(canvas, rule, s.name, color_background)
        case WhenFingerUpOnRule(EIdentShape(s), code) =>
          val miniFingerUp=  gameEngine.bitmaps(R.drawable.fingerup)
          miniFingerUp.setBounds(rectData)
          miniFingerUp.draw(canvas) 
          drawNameBeforeRule(canvas, rule, s.name, color_background)
       case WhenCollisionBetweenRule(EIdentShape(s1), EIdentShape(s2), code) =>
          val bingBitmap=  gameEngine.bitmaps(R.drawable.bing)
          bingBitmap.setAlpha(0xFF)
          bingBitmap.setBounds(rectData)
          bingBitmap.draw(canvas)
          drawNameBeforeRule(canvas, rule, s1.name, color_background)
          drawNameAfterRule(canvas, rule, s2.name, color_background)
        case WhenIntegerChangesRule(EIdentShape(s1), coords, code) =>
          val numbersBitmap=  gameEngine.bitmaps(R.drawable.numbers)
          numbersBitmap.setAlpha(0xFF)
          numbersBitmap.setBounds(rectData)
          numbersBitmap.draw(canvas)
          drawNameBeforeRule(canvas, rule, s1.name, color_background)
        case NoCollisionBetweenRule(EIdentShape(s1), EIdentShape(s2)) =>
          val noCollision=  gameEngine.bitmaps(R.drawable.no_collision)
          noCollision.setBounds(rectData)
          noCollision.draw(canvas)
          drawNameBeforeRule(canvas, rule, s1.name, color_background)
          drawNameAfterRule(canvas, rule, s2.name, color_background)
        case NoCollisionEffectBetweenRule(EIdentShape(s1), EIdentShape(s2)) =>
          val noCollisionEffect=  gameEngine.bitmaps(R.drawable.no_collision_effect)
          noCollisionEffect.setBounds(rectData)
          noCollisionEffect.draw(canvas)
          drawNameBeforeRule(canvas, rule, s1.name, color_background)
          drawNameAfterRule(canvas, rule, s2.name, color_background)
        case _ =>
          // No rule to display
      }
      rule.selectionRectangle.push()
      
      // Draw the code in a box below if the rule is opened.
      // When the user clicks on the rule, he/she can modify the effects.
    }
    // Write the name of the rule on top of it when selected ?
    // 
  }
  
    
  val dimText = new Rect()

  
  def drawNameAfterRule(canvas: Canvas, rule: ReactiveRule, s: String, color: Int ) = {
    mCodePaint.getTextBounds(s, 0, s.length(), dimText)
    drawBackgroundRectangle(canvas, rule.x + rule.width, rule.y, dimText.width() + 16, rule.height, color)
    rule.selectionRectangle.augment(rule.x + rule.width + dimText.width() + 16, rule.y)
    canvas.drawText(s, rule.x + rule.width + 8 - dimText.left, rule.y + (rule.height - dimText.height())/2  - dimText.top, mCodePaint)
  }
  
  def drawNameBeforeRule(canvas: Canvas, rule: ReactiveRule, s: String, color:Int) = {
    mCodePaint.getTextBounds(s, 0, s.length(), dimText)
    drawBackgroundRectangle(canvas, rule.x - dimText.right - 16, rule.y, dimText.width() + 16, rule.height, color)
    rule.selectionRectangle.augment(rule.x - dimText.right - 16, rule.y)
    canvas.drawText(s, rule.x - 8 - dimText.right, rule.y + (rule.height - dimText.height())/2  - dimText.top, mCodePaint)
  }
  
  val dimText1 = new Rect()
  val dimText2 = new Rect()
  
  final val s1 = "  Score.value = Score.prev_value + 1 // <-|->"
  /*final val t1 = "ii"
  final val t2 = "i"
  
  def getCharWidth(): Float = {
    mCodePaint.getTextBounds(s1, 0, s1.length, dimText1)
    mCodePaint.getTextBounds(s2, 0, s2.length, dimText2)
    (dimText2.width() * (s1.length - 1) - dimText1.width() *(s2.length - 1))/(s1.length - s2.length)
  }
  
  def getIntercharWidth(): Float = {
    mCodePaint.getTextBounds(s1, 0, s1.length, dimText1)
    mCodePaint.getTextBounds(s2, 0, s2.length, dimText2)
    // dimText2.width() = char_length * s2.length + space_between_chars*(s2.length-1)
    // dimText1.width() = char_length * s1.length + space_between_chars*(s1.length-1)
    (dimText1.width * s2.length - dimText2.width * s1.length)/(s1.length - s2.length)
  }*/
  
  def getBoxWidth(scale: Float): Float = {
    mCodePaint.setTextSize(ReactiveRule.size*2f/3f * scale)
    val result = mCodePaint.measureText(s1).toFloat / s1.length / scale
    mCodePaint.setTextSize(ReactiveRule.size*2f/3f)
    result
  }

  // Draw the code line
  def drawCodeLineBelowRule(canvas: Canvas, rule: ReactiveRule, scale: Float, s: String, line_num: Int, color: Int = color_selected, background: Boolean = true, text: Boolean = true): Unit = {
    mCodePaint.setTextSize(ReactiveRule.size*2f/3f * scale)
    mCodePaint.getTextBounds(s, 0, s.length(), dimText)
    mCodePaint.setTextSize(ReactiveRule.size*2f/3f)
    dimText.left = 0
    val x1 = rule.x
    val y1 = rule.y + line_num * rule.height 
    val x2 = x1 + dimText.width()/scale + 24
    val y2 = y1 + rule.height
    if(background) drawBackgroundRectangle(canvas, x1, y1, x2-x1, y2-y1, color, false)
    if(text) {
      val textX = rule.x + 8
      val textY = rule.y + line_num * rule.height + (rule.height - dimText.height()/scale)/2  - dimText.top/scale
      canvas.drawText(s, textX, textY, mCodePaint)
    }
  }
}*/