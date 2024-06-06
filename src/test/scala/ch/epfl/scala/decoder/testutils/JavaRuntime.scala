package ch.epfl.scala.decoder.testutils

import java.net.URI
import java.net.URLClassLoader
import java.nio.file.FileSystem
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Collections
import scala.collection.mutable

sealed trait JavaRuntime:
  def javaHome: Path
  def name: String = javaHome.getFileName.toString

final case class Java8(javaHome: Path, classJars: Seq[Path], sourceZip: Option[Path]) extends JavaRuntime

final case class Java9OrAbove(javaHome: Path, fsJar: Path, sourceZip: Option[Path]) extends JavaRuntime:
  def classSystems: Seq[FileSystem] =
    val classLoader = new URLClassLoader(Array(fsJar.toUri.toURL), null)
    Seq(JavaRuntimeSystem(classLoader, javaHome))

object JavaRuntimeSystem:
  private val fileSystems: mutable.Map[Path, FileSystem] = mutable.Map()

  def apply(classLoader: ClassLoader, javaHome: Path): FileSystem =
    val properties = Collections.singletonMap("java.home", javaHome.toString)
    fileSystems.getOrElseUpdate(
      javaHome,
      FileSystems.newFileSystem(URI.create("jrt:/"), properties, classLoader)
    )
end JavaRuntimeSystem

object JavaRuntime:
  def apply(javaHome: String): Option[JavaRuntime] = JavaRuntime(Paths.get(javaHome))

  def apply(javaHome: Path): Option[JavaRuntime] =
    val sources = Seq("src.zip", "lib/src.zip", "../src.zip").map(javaHome.resolve).find(Files.exists(_))
    java8(javaHome, sources).orElse(java9OrAbove(javaHome, sources))

  private def java8(javaHome: Path, srcZip: Option[Path]): Option[JavaRuntime] =
    for
      runtimeJar <- Seq("jre/lib/rt.jar", "lib/rt.jar").map(javaHome.resolve).find(Files.exists(_))
    yield
      val otherJars = Seq("jre/lib/charsets.jar", "lib/charsets.jar").map(javaHome.resolve).filter(Files.exists(_))
      Java8(javaHome, Seq(runtimeJar) ++ otherJars, srcZip)

  private def java9OrAbove(javaHome: Path, srcZip: Option[Path]): Option[JavaRuntime] =
    Some("lib/jrt-fs.jar")
      .map(javaHome.resolve)
      .filter(Files.exists(_))
      .map(Java9OrAbove(javaHome, _, srcZip))
end JavaRuntime
