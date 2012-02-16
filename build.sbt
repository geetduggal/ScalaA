name := "ScalaA"

version := "1.0"

scalaVersion := "2.9.1"
  
scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"
  
seq(ProguardPlugin.proguardSettings :_*)

proguardOptions ++= Seq (
  "-keep class scalala.** { *; }",
  "-keep class scalaa.** { *; }",
  "-dontoptimize",
  "-dontobfuscate", 
  keepAllScala,
  "-keep interface scala.ScalaObject"
)