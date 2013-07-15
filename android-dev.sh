#!/bin/bash

# This assumes:
# - Install the Android Developer Tools bundle (update the path below)
# - Install the Android NDK (update the path below)
# - run android and install levels 15 (my phone) and 16 (Ouya)
# - Install the Oracle JDK (update the path below)
# - Install alex, ant, cabal, ghc, happy and llvm-3.2
# - Install automake-1.13
# - git clone git@github.com:neurocyte/ghc-android.git
# - In ghc-android:
#   CONFIG_SUB_SRC=/usr/local/share/automake-1.13 ./build
#   CONFIG_SUB_SRC=/usr/local/share/automake-1.13 ./build --x86
# - git clone git@github.com:neurocyte/foreign-jni.git
# - (edit foreign-jni/configure to make it work with dash)
# - In foreign-jni, for each of arm-linux-adroideabi- and i686-linux-android-:
#   ...cabal update
#   ...cabal install
# - git clone git@github.com:neurocyte/android-haskell-activity.git
# - In android-haskell-activity:
#   android update project --name HaskellActivity --path . --target android-15
#   ant debug

export JAVA_HOME=~/jdk1.7.0_25
export ANDROID_HOME=~/adt-bundle-linux-x86_64-20130522/sdk
export NDK_HOME=~/android-ndk-r8e
export PATH="~/.ghc/android-14/arm-linux-androideabi-4.7/bin:~/.ghc/android-14/x86-4.7/bin:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools:$NDK_HOME:$PATH"
