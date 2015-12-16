import sbt.Keys._

lazy val commonSettings = Seq(
  organization := "com.github.gmousset",
  name := "raml-parser",
  version := "0.1.0",
  scalaVersion := "2.11.7",
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3",
  libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.3.3"
)

lazy val root = (project in file(".")).settings(commonSettings: _*)


