Pong Designer
=============

This is the experimental branch (v2).

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
    
Screencast
----------

If using *KitKat* (android 4.4), you can directly record your screen.

    adb shell screenrecord --bit-rate 2000000 /sdcard/pong.mp4
    adb pull /sdcard/pong.mp4
    
Other options are available (size, time limit, ...).


Authors
-------

- Mikaël Mayer
- Lomig Mégard
