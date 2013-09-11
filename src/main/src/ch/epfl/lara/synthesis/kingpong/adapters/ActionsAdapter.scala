package ch.epfl.lara.synthesis.kingpong.adapters

import java.util.List
import java.util.Map
import ch.epfl.lara.synthesis.kingpong.R
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
import android.widget.TextView;
import android.graphics.drawable.Drawable

class ActionsAdapter(val context: Context, val actions: IndexedSeq[String], val actionsCollection: Map[String, IndexedSeq[Drawable]]) extends BaseExpandableListAdapter {

  def getChild(groupPosition: Int, childPosition: Int) = {
    actionsCollection.get(actions(groupPosition))(childPosition);
  }

  def getChildId(groupPosition: Int, childPosition: Int) = childPosition
  def getChildView(final int groupPosition, final int childPosition,
boolean isLastChild, View convertView, ViewGroup parent) {
final String laptop = (String) getChild(groupPosition, childPosition);
LayoutInflater inflater = context.getLayoutInflater();

if (convertView == null) {
convertView = inflater.inflate(R.layout.child_item, null);
}

TextView item = (TextView) convertView.findViewById(R.id.laptop);

ImageView delete = (ImageView) convertView.findViewById(R.id.delete);
delete.setOnClickListener(new OnClickListener() {

public void onClick(View v) {
AlertDialog.Builder builder = new AlertDialog.Builder(context);
builder.setMessage("Do you want to remove?");
builder.setCancelable(false);
builder.setPositiveButton("Yes",
new DialogInterface.OnClickListener() {
public void onClick(DialogInterface dialog, int id) {
List<String> child =
laptopCollections.get(laptops.get(groupPosition));
child.remove(childPosition);
notifyDataSetChanged();
}
});
builder.setNegativeButton("No",
new DialogInterface.OnClickListener() {
public void onClick(DialogInterface dialog, int id) {
dialog.cancel();
}
});
AlertDialog alertDialog = builder.create();
alertDialog.show();
}
});

item.setText(laptop);
return convertView;
}

public int getChildrenCount(int groupPosition) {
return laptopCollections.get(laptops.get(groupPosition)).size();
}

public Object getGroup(int groupPosition) {
return laptops.get(groupPosition);
}

public int getGroupCount() {
return laptops.size();
}

public long getGroupId(int groupPosition) {
return groupPosition;
}

public View getGroupView(int groupPosition, boolean isExpanded,
View convertView, ViewGroup parent) {
String laptopName = (String) getGroup(groupPosition);
if (convertView == null) {
LayoutInflater infalInflater = (LayoutInflater) context
.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
convertView = infalInflater.inflate(R.layout.group_item,
null);
}
TextView item = (TextView) convertView.findViewById(R.id.laptop);
item.setTypeface(null, Typeface.BOLD);
item.setText(laptopName);
return convertView;
}

public boolean hasStableIds() {
return true;
}

public boolean isChildSelectable(int groupPosition, int childPosition) {
return true;
}
}