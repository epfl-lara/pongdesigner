Pong Designer
=============

This is the experimental branch.

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

Authors
-------

- Mikaël Mayer
- Lomig Mégard