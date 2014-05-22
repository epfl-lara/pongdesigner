package ch.epfl.lara.synthesis.kingpong.view

import android.graphics.Paint

/**
 * Constants for the rendering.
 * The mutable fields must *NOT* be modified.
 */
object RenderConstants {

  val whitePaint = {
    val p = new Paint()
    p.setColor(0xFFFFFFFF)
    p.setStyle(Paint.Style.FILL_AND_STROKE)
    p.setStrokeWidth(1)
    p.setAntiAlias(true)
    p
  }

}
