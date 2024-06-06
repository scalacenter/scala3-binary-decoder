package ch.epfl.scala.decoder.testutils

import coursier.*

import java.io.File
import scala.collection.mutable
import scala.util.Try

case class FetchOptions(
    keepOptional: Boolean = false,
    keepProvided: Boolean = false,
    repositories: Seq[Repository] = Seq.empty,
    exclusions: Seq[(String, String)] = Seq.empty
)

object FetchOptions:
  def default = FetchOptions()

object Resolver:
  def fetchOnly(org: String, name: String, version: String): ClasspathEntry =
    fetch(org, name, version)
      .find(_.absolutePath.getFileName.toString == s"$name-$version.jar")
      .get

  def fetch(
      org: String,
      name: String,
      version: String,
      options: FetchOptions = FetchOptions.default
  ): Seq[ClasspathEntry] =
    val dep = Dependency(coursier.Module(Organization(org), ModuleName(name)), version)
    fetch(Seq(dep), options)

  def fetch(dependencies: Dependency*): Seq[ClasspathEntry] = fetch(dependencies, FetchOptions.default)

  def fetch(dependencies: Seq[Dependency], options: FetchOptions): Seq[ClasspathEntry] =
    coursier
      .Fetch()
      .addRepositories(options.repositories :+ MavenRepository("https://repo1.maven.org/maven2/dev")*)
      .addDependencies(dependencies*)
      // .addClassifiers(Classifier.sources)
      .withMainArtifacts()
      .mapResolutionParams { params =>
        val exclusions = options.exclusions.map { case (org, mod) => (Organization(org), ModuleName(mod)) }.toSet
        params
          .withKeepOptionalDependencies(options.keepOptional)
          // .withKeepProvidedDependencies(options.keepProvided)
          .withExclusions(exclusions)
      }
      .run()
      .groupBy(getArtifactId)
      .toSeq
      .flatMap { case (artifactId, jars) =>
        for {
          // sourceJar <- jars.find(_.getName.endsWith("-sources.jar"))
          classJar <- jars.headOption
        } yield {
          val verisonFolder =
            if classJar.getParentFile.getName == "jars" then classJar.getParentFile.getParentFile
            else classJar.getParentFile
          ClasspathEntry(artifactId, classJar.toPath)
        }
      }

  private def getArtifactId(file: File): String =
    file.getName.stripSuffix(".jar").stripSuffix("-sources")

  def fetch(scalaVersion: ScalaVersion): ScalaInstance =
    if scalaVersion.isScala2 then fetchScala2(scalaVersion) else fetchScala3(scalaVersion)

  private def fetchScala2(scalaVersion: ScalaVersion): Scala2Instance =
    val dep = Dependency(
      coursier.Module(Organization("org.scala-lang"), ModuleName("scala-compiler")),
      scalaVersion.value
    )
    val jars = fetch(dep)
    val libraryJars = jars.filter(jar => jar.name.startsWith("scala-library"))
    val compilerJars = jars.filter(jar => !libraryJars.contains(jar))
    new Scala2Instance(libraryJars, compilerJars)

  private def fetchScala3(scalaVersion: ScalaVersion): Scala3Instance =
    val dep = Dependency(
      coursier.Module(Organization("org.scala-lang"), ModuleName("scala3-compiler_3")),
      scalaVersion.value
    )
    val jars = fetch(dep)
    val libraryJars =
      jars.filter(jar => jar.name.startsWith("scala-library") || jar.name.startsWith("scala3-library_3"))
    val compilerJars = jars.filter(jar => !libraryJars.contains(jar))
    new Scala3Instance(libraryJars, compilerJars)
end Resolver
