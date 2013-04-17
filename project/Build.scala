import sbt._
import Keys._
import AndroidKeys._

object General {

  // Parameters:
  val buildName           = "kingpong_experiment"
  val buildOrganization   = "ch.epfl.lara.synthesis"
  val buildScalaVersion   = "2.10.1"
  val buildAndroidVersion = "16"

  val settings = Defaults.defaultSettings ++ Seq (
    
    // version and versionCode are in the manifest. We could also put them 
    // here and let the plugin update the manifest with `AndroidManifestGenerator`

    name         := buildName,
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    
    // Jbox2d
    //libraryDependencies += "org.jbox2d" % "jbox2d-library" % "2.2.1.2",
    
    // ScalaTest
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b" % "test",

    scalacOptions ++= Seq(
        "-feature",                      // Enable language feature warnings
        "-deprecation",                  // Enable detailed deprecation warnings 
        "-unchecked"//,                    // Enable detailed unchecked warnings 
        //"-language:experimental.macros", // Enable scala macros
        //"-language:implicitConversions", // Remove feature warning about implicit methods
        //"-language:postfixOps"           // Remove feature warning about postfix operators
      )
  )

   lazy val androidSettings: Seq[Setting[_]] = inConfig(Android) (Seq(

    platformName := "android-" + buildAndroidVersion,
    keyalias := buildName,

    useProguard := true,
    //proguardOption := "-keep class scala.Function1",
    proguardOptimizations := Seq.empty
  ))

  lazy val fullAndroidSettings =
    settings ++
    AndroidProject.androidSettings ++
    TypedResources.settings ++
    androidSettings

    // For more advance features:
    //AndroidManifestGenerator.settings ++
    //AndroidMarketPublish.settings ++ 
}

object AndroidBuild extends Build {
  lazy val main = Project (
    General.buildName,
    file("."),
    settings = General.fullAndroidSettings
  )
}
