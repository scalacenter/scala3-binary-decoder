import sbt._

object Dependencies {
  val scala3Next = "3.6.2"
  val asmVersion = "9.7.1"
  val coursierVersion = "2.1.24"

  val tastyQuery = "ch.epfl.scala" %% "tasty-query" % "1.4.0"
  val asm = "org.ow2.asm" % "asm" % asmVersion
  val asmUtil = "org.ow2.asm" % "asm-util" % asmVersion

  // test dependencies
  val munit = "org.scalameta" %% "munit" % "1.0.4"
  val coursier = ("io.get-coursier" %% "coursier" % coursierVersion).cross(CrossVersion.for3Use2_13)
  val coursierJvm = ("io.get-coursier" %% "coursier-jvm" % coursierVersion).cross(CrossVersion.for3Use2_13)
}
