package ch.epfl.lara.synthesis.kingpong.objects

import android.media.MediaPlayer
import android.net.Uri
import android.media.AudioManager
import android.content.Context

trait SoundPlayer {
  private var mp: MediaPlayer = null
  def exists() = mp != null
  def loadFromUri(context: Context, uri: Uri) = {
    val mp = new MediaPlayer();
		mp.setAudioStreamType(AudioManager.STREAM_MUSIC);
		mp.setDataSource(context, uri);
		
  }
}