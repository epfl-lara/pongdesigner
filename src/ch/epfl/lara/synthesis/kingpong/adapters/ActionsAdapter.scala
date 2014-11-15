package ch.epfl.lara.synthesis.kingpong.adapters

import java.util.List
import scala.collection.mutable.{HashMap => MMap}
import ch.epfl.lara.synthesis.kingpong.R
import ch.epfl.lara.synthesis.kingpong.Implicits

import android.app.Activity
import android.app.AlertDialog
import android.content.Context
import android.content.DialogInterface
import android.graphics.Typeface
import android.util.Log
import android.view.LayoutInflater
import android.view.View
import android.view.View.OnClickListener
import android.view.ViewGroup
import android.widget.BaseExpandableListAdapter
import android.widget.ImageView
import android.widget.ExpandableListView
import android.widget.TextView;
import android.graphics.drawable.Drawable

class ActionsAdapter(val context: Context, val listView: ExpandableListView, val actions: IndexedSeq[String], val actionsCollection: Map[String, IndexedSeq[String]], val bitmaps: MMap[String, Drawable], val callbacks: String => Boolean) extends BaseExpandableListAdapter with Implicits {

  def getChild(groupPosition: Int, childPosition: Int) = {
    actionsCollection(actions(groupPosition))(childPosition);
  }

  def getChildId(groupPosition: Int, childPosition: Int) = childPosition
  
  def getChildView(groupPosition: Int, childPosition: Int,
        isLastChild: Boolean, convertView: View, parent: ViewGroup): View = {
    val action = getChild(groupPosition, childPosition)
    val inflater = LayoutInflater.from(context)
    
    val view = if (convertView == null) {
      inflater.inflate(R.layout.actions_itemview, null)
    } else convertView

    val item = view.findViewById(R.id.menudrawable).asInstanceOf[ImageView]
    
    view.setOnClickListener{
      val str = actionsCollection(actions(groupPosition))(childPosition)
      () => { callbacks(str); () }
    }
    bitmaps.get(action) match {
      case Some(drawable) => item.setImageDrawable(drawable)
      case None =>
    }
    view
  }

  def getChildrenCount(groupPosition: Int): Int = {
    actionsCollection(actions(groupPosition)).size
  }

  def getGroup(groupPosition: Int): Object = {
    actions(groupPosition)
  }

  def getGroupCount(): Int = {
    return actions.size
  }

  def getGroupId(groupPosition: Int): Long = {
    groupPosition
  }

  def getGroupView(groupPosition: Int, isExpanded: Boolean,
      convertView: View, parent: ViewGroup): View = {
    val action = getGroup(groupPosition).asInstanceOf[String]
    val view = if (convertView == null) {
      val infalInflater = LayoutInflater.from(context)
      infalInflater.inflate(R.layout.actions_groupview, null)
    } else convertView
    
    val item = view.findViewById(R.id.menugroupdrawable).asInstanceOf[ImageView]
    /*view.setOnClickListener{
      val str = actions(groupPosition)
      () => callbacks(str)
    }*/
    bitmaps.get(action) match {
      case Some(drawable) => item.setImageDrawable(drawable)
      case None =>
    }
    view
  }

  def hasStableIds(): Boolean = {
    true
  }

  def isChildSelectable(groupPosition: Int, childPosition: Int): Boolean = {
    true
  }
  var lastExpandedGroupPosition = -1
  override def onGroupExpanded(groupPosition: Int) {
      //collapse the old expanded group, if not the same
      //as new group to expand
      if(groupPosition != lastExpandedGroupPosition && lastExpandedGroupPosition != -1){
          listView.collapseGroup(lastExpandedGroupPosition);
      }

      super.onGroupExpanded(groupPosition);           
      lastExpandedGroupPosition = groupPosition;
  }
}