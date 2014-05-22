package ch.epfl.lara.synthesis.kingpong.view

import android.graphics.{Rect, Path, Canvas, Bitmap}

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

  def cutBitmap(sourceBitmap: Bitmap, columns: Int, rows: Int): Seq[Bitmap] = {
    require(columns > 0 && rows > 0, "Number of columns and rows must be both positive.")

    val width = sourceBitmap.getWidth / columns
    val height = sourceBitmap.getHeight / rows

    for {
      column <- 0 until columns
      row    <- 0 until rows
    } yield {
      val x = column * width
      val y = row * height
      Bitmap.createBitmap(sourceBitmap, x, y, width, height)
    }
  }

}
