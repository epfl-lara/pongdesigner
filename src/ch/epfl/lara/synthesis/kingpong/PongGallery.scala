package ch.epfl.lara.synthesis.kingpong

import android.app.Activity
import adapters._
import android.widget.GridView
import android.widget.AdapterView.OnItemClickListener
import android.view.View
import android.widget.AdapterView
import android.net.Uri

class PongGallery extends Activity with ActivityUtil { self =>
  lazy val gridView = R.id.pongGalleryGridView : GridView
  
  onCreate{ bundle =>
    setContentView(R.layout.pong_gallery)
    
    val images = (for(i <- R.drawable.image_aaaa + 1 to R.drawable.image_zzzz - 1) yield new Integer(i)).toArray
    
    val adapter = new ImageAdapter(this, images)
    gridView.setAdapter(adapter)
    gridView.setOnItemClickListener(new OnItemClickListener() {
      def onItemClick(parent: AdapterView[_], v: View, position: Int, id: Long) {
          val intent = self.getIntent();
          val path = Uri.parse("android.resource://"+self.getPackageName()+"/" + id);
          intent.setData(path)
          self.setResult(Activity.RESULT_OK, intent);
          self.finish();
      }
    });
  }
  
  onPause {
    
  }
  
  onResume {
    
  }
 
}