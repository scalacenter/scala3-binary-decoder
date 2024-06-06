package ch.epfl.scala.decoder.testutils

import java.io.File
import java.net.URLClassLoader
import java.nio.file.Path
import scala.collection.mutable

sealed trait ScalaInstance(val libraryJars: Seq[ClasspathEntry], val compilerJars: Seq[ClasspathEntry]):
  val libraryClassLoader = new URLClassLoader(libraryJars.map(_.toURL).toArray, null)
  val compilerClassLoader = new URLClassLoader(compilerJars.map(_.toURL).toArray, libraryClassLoader)

  def compile(
      classDir: Path,
      classPath: Seq[ClasspathEntry],
      scalacOptions: Seq[String],
      sourceFiles: Seq[Path]
  ): Unit =
    val args = Array(
      "-d",
      classDir.toString,
      "-classpath",
      classPath.map(_.absolutePath).mkString(File.pathSeparator)
    ) ++
      scalacOptions ++
      sourceFiles.map(_.toString)
    compileInternal(args)

  protected def compileInternal(args: Array[String]): Unit
end ScalaInstance

final class Scala2Instance(
    libraryJars: Seq[ClasspathEntry],
    compilerJars: Seq[ClasspathEntry]
) extends ScalaInstance(libraryJars, compilerJars):
  override protected def compileInternal(args: Array[String]): Unit =
    val main = compilerClassLoader.loadClass("scala.tools.nsc.Main")
    val process = main.getMethod("process", classOf[Array[String]])
    val success = process.invoke(null, args).asInstanceOf[Boolean]
    if !success then throw new Exception("compilation failed")

final class Scala3Instance(
    libraryJars: Seq[ClasspathEntry],
    compilerJars: Seq[ClasspathEntry]
) extends ScalaInstance(libraryJars, compilerJars):
  override protected def compileInternal(args: Array[String]): Unit =
    val main = compilerClassLoader.loadClass("dotty.tools.dotc.Main")
    val process = main.getMethod("process", classOf[Array[String]])
    val classOfReporter =
      compilerClassLoader.loadClass("dotty.tools.dotc.reporting.Reporter")
    val hasErrors = classOfReporter.getMethod("hasErrors")
    val reporter = process.invoke(null, args)
    val success = !hasErrors.invoke(reporter).asInstanceOf[Boolean]
    if !success then throw new Exception("compilation failed")

object ScalaInstance:
  private val cache = mutable.Map.empty[ScalaVersion, ScalaInstance]

  def apply(scalaVersion: ScalaVersion): ScalaInstance =
    cache.getOrElseUpdate(scalaVersion, Resolver.fetch(scalaVersion))
end ScalaInstance
