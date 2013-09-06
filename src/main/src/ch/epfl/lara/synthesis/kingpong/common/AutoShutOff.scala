package ch.epfl.lara.synthesis.kingpong.common

import android.media.MediaRecorder

trait AutoShutOff {
   var record: MediaRecorder = _
   var finishRecording: Thread = _
   var willShutDown: Boolean = false
   def fromPrevious(previous: AutoShutOff) {
    record = previous.record
    previous.record = null
    previous.finishRecording = null
    previous.willShutDown = false
  }
   def cancelShutDown() = willShutDown = false
   def startTimerDown() = {
     if(finishRecording != null) {
       willShutDown = false
       Thread.sleep(100)
     }
     finishRecording = new Thread(new Runnable{
       override def run() = {
         willShutDown = true
         val init_time = System.currentTimeMillis
         while(System.currentTimeMillis - init_time < 10000 && willShutDown) {
           Thread.sleep(10)
         }
         if(willShutDown) {
           if(record != null) {
             record.stop()
             record.release()
             record = null
           }
         }
         finishRecording = null
       }
     })
     if(finishRecording != null) finishRecording.start()
   }
}