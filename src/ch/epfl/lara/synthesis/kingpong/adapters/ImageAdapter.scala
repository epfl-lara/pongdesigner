package ch.epfl.lara.synthesis.kingpong.adapters

import android.content.Context
import android.view.View
import android.view.ViewGroup
import android.widget.BaseAdapter
import android.widget.GridView
import android.widget.ImageView
import ch.epfl.lara.synthesis.kingpong._
import scala.collection.JavaConversions._
import android.widget.AbsListView

class ImageAdapter(private var mContext: Context, mThumbIds: Array[Integer]) extends BaseAdapter {

  def getCount(): Int = mThumbIds.length

  def getItem(position: Int): AnyRef = mThumbIds(position)

  def getItemId(position: Int): Long = mThumbIds(position).toLong

  def getView(position: Int, convertView: View, parent: ViewGroup): View = {
    var imageView: ImageView = null
    if (convertView == null) {
      imageView = new ImageView(mContext)
      imageView.setLayoutParams(new AbsListView.LayoutParams(85, 85))
      imageView.setScaleType(ImageView.ScaleType.CENTER_CROP)
      imageView.setPadding(8, 8, 8, 8)
    } else {
      imageView = convertView.asInstanceOf[ImageView]
    }
    imageView.setImageResource(mThumbIds(position))
    imageView
  }
}
