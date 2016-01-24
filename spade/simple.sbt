
name := "SPADE"

version := "0.0.1"

scalaVersion := "2.10.4"

libraryDependencies += "org.apache.spark" %% "spark-core" % "1.5.2"


mainClass in (Compile, packageBin) := Some("SpadePureApp")
