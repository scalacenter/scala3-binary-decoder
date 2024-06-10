def isRelease() =
  System.getenv("GITHUB_REPOSITORY") == "scalacenter/scala3-binary-decoder" &&
    Option(System.getenv("GITHUB_WORKFLOW")).exists(_.contains("Release"))

inThisBuild(
  Seq(
    organization := "ch.epfl.scala",
    homepage := Some(url("https://github.com/scalacenter/scala3-binary-decoder")),
    onLoadMessage := s"Welcome to scala3-binary-decoder ${version.value}",
    licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := Developers.all,
    version ~= { dynVer => if (isRelease) dynVer else "1.0.0-SNAPSHOT" }
  )
)

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
      Dependencies.munit % Test,
      Dependencies.coursier % Test,
      Dependencies.coursierJvm % Test
    ),
    Test / fork := true,
    Test / testOptions += Tests.Argument(TestFrameworks.MUnit, "+l")
  )
