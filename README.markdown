Pong Designer
=============

This is the experimental branch (v2) and the current source code of the [Google Play application](https://play.google.com/store/apps/details?id=ch.epfl.lara.synthesis.kingpong)

![Screenshot](https://lh6.ggpht.com/a9zMAt61KEVyc0BrX2dPKFYgZPKNneHeqOeqoPWnht19nAGe81UL6hUayNWks0opxcY=h900-rw)

Watch the video: [Create a Pong Game in less than one minute](https://youtu.be/bErU--8GRsQ?list=PLLIzdB2ffI_ts2WnDJF37RZmAuxRr0Nyg)

Usage with SBT
--------------

The project uses the plugin [android-sdk-plugin](https://github.com/pfn/android-sdk-plugin).

Make sure that the Android SDK is present and export its location:

    export ANDROID_SDK_ROOT=/usr/local/Cellar/android-sdk/22.3

Compile and package:

    sbt
    > compile
    > android:package

Install on the device:

    > android:install

Debug
-----

You can debug a running instance with `ADB`. The keywork `kingpong` is used as a marker for all debug statements related to the application.

    adb logcat ActivityManager:I kingpong:D *:S

To see full exceptions trace, run the command without any filter:

    adb logcat
    
Screencast / Screenshot
-----------------------

If using *KitKat* (android 4.4), you can directly record your screen.

    adb shell screenrecord --bit-rate 2000000 /sdcard/pong.mp4
    adb pull /sdcard/pong.mp4
    
Other options are available (size, time limit, ...).

For a simple screenshot, the following command is available.

    adb shell screencap -p /sdcard/capture.png
    adb pull /sdcard/capture.png


Authors
-------

- Mikaël Mayer
- Lomig Mégard
