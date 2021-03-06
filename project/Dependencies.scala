import sbt._

object Dependencies {
  lazy val akkaStreams = "com.typesafe.akka" %% "akka-stream" % "2.5.18"
  lazy val spark = "org.apache.spark" %% "spark-core" % "2.4.0"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0"
}
