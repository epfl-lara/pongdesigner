package ch.epfl.lara.synthesis.kingpong.view

import android.graphics.{Rect, Path, Canvas, Bitmap}
import android.graphics.drawable.{BitmapDrawable, Drawable}

/**
 * Some utility functions for android bitmaps.
 */
object BitmapUtils {

  /**
   * Crop the given bitmap in a circular shape.
   * Source: [[http://www.androiddevelopersolution.com/2012/09/crop-image-in-circular-shape-in-android.html]]
   */
  def toRoundedShape(sourceBitmap: Bitmap): Bitmap = {
    val targetSize = Math.min(sourceBitmap.getWidth, sourceBitmap.getHeight)
    val targetBitmap = Bitmap.createBitmap(targetSize, targetSize, Bitmap.Config.ARGB_8888)

    val canvas = new Canvas(targetBitmap)
    val path = new Path()
    path.addCircle((targetSize.toFloat - 1) / 2,
      (targetSize.toFloat - 1) / 2,
      targetSize / 2,
      Path.Direction.CCW)

    canvas.clipPath(path)
    canvas.drawBitmap(sourceBitmap,
      new Rect(0, 0, sourceBitmap.getWidth, sourceBitmap.getHeight),
      new Rect(0, 0, targetSize, targetSize), null)
    targetBitmap
  }

  def cutBitmap(sourceBitmap: Bitmap, x: Int, y: Int, width: Int, height: Int): Bitmap = {
    Bitmap.createBitmap(sourceBitmap, x, y, width, height)
  }

  def cutDrawable(sourceDrawable: Drawable, x: Int, y: Int, width: Int, height: Int): Bitmap = {
    val bitmap = drawableToBitmap(sourceDrawable)
    cutBitmap(bitmap, x, y, width, height)
  }

  def drawableToBitmap(drawable: Drawable): Bitmap = drawable match {
    case b: BitmapDrawable =>
      b.getBitmap
    case _ =>
      val bitmap = Bitmap.createBitmap(drawable.getIntrinsicWidth(), drawable.getIntrinsicHeight(), Bitmap.Config.ARGB_8888)
      val canvas = new Canvas(bitmap)
      drawable.setBounds(0, 0, canvas.getWidth(), canvas.getHeight())
      drawable.draw(canvas)
      bitmap
  }

}
