package ch.epfl.lara.synthesis.kingpong;

import scala.collection.mutable.ArrayBuffer;
import android.os.AsyncTask;

/**
 * Temporary workaround to solve a Scala compiler issue which shows up
 * at runtime with the error message
 * "java.lang.AbstractMethodError: abstract method not implemented"
 * for the missing method LookupTask.doInBackground(String... args).
 *
 * Our solution: the Java method doInBackground(String... args) forwards
 * the call to the Scala method doInBackground1(String[] args).
 */
public abstract class MyAsyncTask<U, V, W> extends AsyncTask<U, V, W> {

    @Override
    protected W doInBackground(U ... args) {
        int count = args.length;
        ArrayBuffer<U> res = new ArrayBuffer<U>(count);
        for (int i = 0; i < count; i++) {
          res.$plus$eq(args[i]);
        }
        return doInBackground1(res);
    }

    protected abstract W doInBackground1(ArrayBuffer<U> args);
    
    @Override
    protected void onProgressUpdate(V ... args) {
      int count = args.length;
      ArrayBuffer<V> res = new ArrayBuffer<V>(count);
      
      res.ensureSize(count);
      res.reduceToSize(0);
      
      for (int i = 0; i < count; i++) {
        res.$plus$eq(args[i]);
      }
      onProgressUpdate1(res);
   }
    
    protected abstract void onProgressUpdate1(ArrayBuffer<V> args);
}