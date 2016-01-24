import sbt.Keys._

name := "SPADE"

version := "0.0.1"

scalaVersion := "2.11.7"

libraryDependencies += "org.apache.spark" %% "spark-core" % "1.6.0" % "provided"


mainClass in (Compile, packageBin) := Some("SpadePureApp")
