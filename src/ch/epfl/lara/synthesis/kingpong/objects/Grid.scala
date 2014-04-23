package ch.epfl.lara.synthesis.kingpong.objects

import android.graphics.Canvas
import ch.epfl.lara.synthesis.kingpong.Game
import android.graphics.Matrix
import android.graphics.Paint

object Grid {
  def apply(m: Matrix, width: Int, numSteps: Int, stroke_width: Float=1, color:Int = 0x88000000): Grid = {
    val a = Array(0f, 0f, width, 0f)
    m.mapPoints(a)
    
    var n = Math.floor(Math.log((a(2)-a(1))/numSteps)/Math.log(2))
    var step = Math.pow(2, n).toFloat
    new Grid(step, offset=0, stroke_width=stroke_width, color=color)
  }
}

/**
 * A grid to display objects on.
 */
class Grid(val step: Float, offset: Float, stroke_width: Float, color:Int) {
  private val min = Array(0f, 0f)
  private val max = Array(0f, 0f)
  private val a = Array(0f, 0f, 0f, 0f)
  private val gridPaint = new Paint()
  gridPaint.setColor(color)
  gridPaint.setStrokeWidth(stroke_width)
  gridPaint.setAntiAlias(true)
  gridPaint.setStyle(Paint.Style.STROKE)
  def drawOn(m: Matrix, im: Matrix, c: Canvas) = {
    min(0) = 0f
    min(1) = 0f
    max(0) = c.getWidth()
    max(1) = c.getHeight()
    im.mapPoints(min)
    im.mapPoints(max)
    //val (_, iStart) = choose( (x: Float, i: Int) => x == step*i + offset  && minimize(x) && x >= min(0))
    //val (_, iEnd) = choose( (x: Float, i: Int) => x == step*i + offset  &&  maximize(x) && x <= max(0))
    //val (_, jStart) = choose( (x: Float, i: Int) => y == step*i + offset  && minimize(y) && y >= min(1))
    //val (_, jEnd) = choose( (x: Float, i: Int) => y == step*i + offset  && maximize(y) && y <= max(1))
    val iStart = Math.ceil((min(0) - offset)/step).toInt
    val iEnd = Math.floor((max(0) - offset)/step).toInt

    val jStart = Math.ceil((min(1) - offset)/step).toInt
    val jEnd = Math.floor((max(1) - offset)/step).toInt

    for(i <- iStart to iEnd) {
      a(0) = i*step+offset
      a(1) = min(1)
      a(2) = i*step+offset
      a(3) = max(1)
      m.mapPoints(a)
      c.drawLine(a(0), a(1), a(2), a(3), gridPaint)
    }
    for(j <- jStart to jEnd) {
      a(0) = min(0)
      a(1) = j*step + offset
      a(2) = max(0)
      a(3) = j*step + offset
      m.mapPoints(a)
      c.drawLine(a(0), a(1), a(2), a(3), gridPaint)
    }
  }
  
  // Snaps the corresponding coordinate along the x axis
  def snap(x: Float): Float = {
    (((x - offset)/step).round*step + offset).toFloat
  }
  
//  def snap(v: Vec2V): Vec2V = {
//    Vec2V(snap(v.x), snap(v.y))
//  }
}