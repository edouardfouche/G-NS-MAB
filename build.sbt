name := "G-NS-MAB"

organization := "com.edouardfouche"

version := "1.0"
scalaVersion := "2.12.8"
fork in run := true

javaOptions += "-Xmx20G" // Change depending on your machine configuration
javaOptions += "-Xms20G" // Change depending on your machine configuration
//javaOptions += "-XX:-UseGCOverheadLimit" // may lead to some problems

scalacOptions ++= Seq("-feature")

libraryDependencies += "de.lmu.ifi.dbs.elki" % "elki" % "0.7.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies ++= Seq(
  // Last stable release
  "org.scalanlp" %% "breeze" % "0.13.1",

  // Native libraries are not included by default. add this if you want them (as of 0.7)
  // Native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as far as I know.
  "org.scalanlp" %% "breeze-natives" % "0.13.1"

  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  // "org.scalanlp" %% "breeze-viz" % "0.13.1"
)

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
libraryDependencies += "ch.qos.logback" % "logback-classic" %  "1.1.2" //"1.2.3" // this is for the logging backend
// Note: from logback 1.1.5, threads do not inherit the MDC anymore
resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

//assemblySettings

//import sbtassembly.Plugin.AssemblyKeys._
assemblyJarName in assembly := s"${name.value}-${version.value}.jar"
test in assembly := {}

javacOptions ++= Seq("-encoding", "UTF-8")
//logLevel := Level.Debug
