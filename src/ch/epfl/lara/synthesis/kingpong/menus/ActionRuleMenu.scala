package ch.epfl.lara.synthesis.kingpong.menus

import scala.collection.mutable.HashMap
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong._
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.drawable.Drawable
import android.content.Context
import android.graphics.RectF
import android.graphics.Rect
import android.graphics.Paint
import android.text.TextPaint
import android.graphics.Typeface
import android.util.Log
import android.graphics.Matrix
/*
object ActionRuleMenu extends MenuCenter {
  menus = Nil
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