import sbt._
import Keys._
import AndroidKeys._

object General {

  // Parameters:
  val buildName           = "kingpong"
  val buildOrganization   = "ch.epfl.lara.synthesis"
  val buildScalaVersion   = "2.10.0"
  val buildAndroidVersion = "16"

  val settings = Defaults.defaultSettings ++ Seq (
    
    // version and versionCode are in the manifest. We could also put them 
    // here and let the plugin update the manifest with `AndroidManifestGenerator`

    name         := buildName,
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    
    scalaSource in Compile <<= baseDirectory(_ / "src"),
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

    manifestPath <<= (baseDirectory, manifestName) map((s,m) => Seq(s / m)) map (x=>x),
    manifestTemplatePath <<= (baseDirectory, manifestName) (_ / _),
    mainResPath <<= (baseDirectory, resDirectoryName) (_ / _) map (x=>x),
    mainAssetsPath <<= (baseDirectory, assetsDirectoryName) (_ / _),

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
