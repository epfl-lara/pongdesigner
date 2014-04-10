import sbt._
import Keys._
import android.Keys._

object General {

  // Parameters:
  val buildName           = "pong_designer_experimental"
  val buildOrganization   = "ch.epfl.lara.synthesis"
  val buildScalaVersion   = "2.10.3"
  val buildAndroidVersion = "16"

  val settings = Defaults.defaultSettings ++ 
                 android.Plugin.androidBuild ++ Seq (

    name         := buildName,
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    platformTarget in Android := "android-" + buildAndroidVersion,
    proguardScala in Android := true,
    debugIncludesTests in Android := false,

    proguardOptions in Android ++= Seq(
      //"-keep public class * extends junit.framework.TestCase",
      //"-keepclass class * extends junit.framework.TestCase { *; }",
      "-dontwarn android.test.**",
      "-dontwarn java.awt.**"
    ),

    // Jbox2d
    //libraryDependencies += "org.jbox2d" % "jbox2d-library" % "2.2.1.2",
    
    // ScalaTest
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test",
    //libraryDependencies += "junit" % "junit" % "4.11",

    // Android support
    libraryDependencies += "com.android.support" % "support-v4" % "18.0.0",
    
    scalacOptions ++= Seq(
      "-optimize",
      "-feature",                      // Enable language feature warnings
      "-deprecation",                  // Enable detailed deprecation warnings 
      "-unchecked",                    // Enable detailed unchecked warnings 
      //"-language:experimental.macros", // Enable scala macros
      "-language:implicitConversions", // Remove feature warning about implicit methods
      "-language:postfixOps",          // Remove feature warning about postfix operators
      "-language:reflectiveCalls"      // Remove feature warning about reflective calls
    ),
    javacOptions ++= Seq(
      "-deprecation"
    )
  )

}

object AndroidBuild extends Build {
  lazy val main = Project (
    General.buildName,
    file("."),
    settings = General.settings
  )
}
