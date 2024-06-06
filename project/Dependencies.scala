import sbt._

object Dependencies {
  val scala3Next = "3.4.2"
  val asmVersion = "9.7"
  val coursierVersion = "2.1.10"

  val tastyQuery = "ch.epfl.scala" %% "tasty-query" % "1.3.0"
  val asm = "org.ow2.asm" % "asm" % asmVersion
  val asmUtil = "org.ow2.asm" % "asm-util" % asmVersion

  // test dependencies
  val munit = "org.scalameta" %% "munit" % "1.0.0"
  val coursier = "io.get-coursier" %% "coursier" % coursierVersion
  val coursierJvm = "io.get-coursier" %% "coursier-jvm" % coursierVersion
}
