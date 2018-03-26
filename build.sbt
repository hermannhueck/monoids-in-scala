import Dependencies._
import sbt.Keys.scalacOptions

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "monoids-in-scala",
      scalaVersion := "2.12.5",
      version      := "0.1.0-SNAPSHOT",
        scalacOptions := Seq(
        "-unchecked",
        "-deprecation",
        "-feature",
        "-Ywarn-unused",
        "-Ywarn-unused-import",
        "-Ywarn-dead-code",
        "-Ywarn-value-discard"
      )
    )),
    name := "monoids-in-scala",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaCheck % Test
  )
