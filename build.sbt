
lazy val decoder: Project = project
  .in(file("."))
  .settings(
    name := "scala3-binary-decoder",
    scalaVersion := Dependencies.scala3Next,
    Compile / doc / sources := Seq.empty,
    libraryDependencies ++= Seq(
      Dependencies.tastyQuery,
      Dependencies.asm,
      Dependencies.asmUtil,
      Dependencies.munit % Test
    ),
    Test / fork := true,
    Test / testOptions += Tests.Argument(TestFrameworks.MUnit, "+l"),
  )
