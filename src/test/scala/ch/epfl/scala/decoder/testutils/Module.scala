package ch.epfl.scala.decoder.testutils

import java.io.BufferedReader
import java.io.InputStream
import java.io.InputStreamReader
import java.nio.file.Files
import java.nio.file.Paths
import scala.util.Properties
import scala.util.control.NonFatal

case class Module(mainEntry: ClasspathEntry, classpath: Seq[ClasspathEntry])

object Module:
  val javaHome = Paths.get(Properties.jdkHome)
  private val ext = if Properties.isWin then ".exe" else ""
  private val javac =
    val paths = Seq(s"bin/javac$ext", s"../bin/javac$ext")
    paths.map(javaHome.resolve).find(Files.exists(_)).get

  def fromSource(source: String, scalaVersion: ScalaVersion): Module =
    fromSources(Seq("Test.scala" -> source), scalaVersion, Seq.empty, Seq.empty)

  def fromSource(source: String, scalaVersion: ScalaVersion, dependencies: Seq[ClasspathEntry]): Module =
    fromSources(Seq("Test.scala" -> source), scalaVersion, Seq.empty, dependencies)

  def fromSources(
    sources: Seq[(String, String)],
    scalaVersion: ScalaVersion,
    scalacOptions: Seq[String],
    dependencies: Seq[ClasspathEntry]
  ): Module =
    val tempDir = Files.createTempDirectory("scala3-binary-decoder")
    val srcDir = tempDir.resolve("src")
    Files.createDirectory(srcDir)
    val classDir = tempDir.resolve("classes")
    Files.createDirectory(classDir)

    val sourceFiles = for ((fileName, source) <- sources) yield {
      val sourceFile = srcDir.resolve(fileName)
      Files.write(sourceFile, source.getBytes())
      sourceFile
    }

    val scalaInstance = ScalaInstance(scalaVersion)
    def isScalaLibrary(dep: ClasspathEntry): Boolean = {
      if scalaVersion.isScala3 then dep.name.startsWith("scala-library") || dep.name.startsWith("scala3-library")
      else dep.name.startsWith("scala-library")
    }
    val allDependencies =
      if dependencies.isEmpty then scalaInstance.libraryJars
      else dependencies.filter(!isScalaLibrary(_)) ++ scalaInstance.libraryJars

    scalaInstance.compile(classDir, allDependencies, scalacOptions, sourceFiles)
    val mainEntry = ClasspathEntry(sources.head(0), classDir)
    Module(mainEntry, mainEntry +: allDependencies)
  end fromSources

  def fromJavaSource(source: String, scalaVersion: ScalaVersion): Module =
    val tempDir = Files.createTempDirectory("scala-debug-adapter")
    val name = "Test.java"

    val srcDir = tempDir.resolve("src")
    Files.createDirectory(srcDir)
    val classDir = tempDir.resolve("classes")
    Files.createDirectory(classDir)

    val srcFile = srcDir.resolve(name)
    Files.write(srcFile, source.getBytes())

    val command = Array(javac.toString, "-g:source,lines,vars", "-d", classDir.toString, srcFile.toString)
    val builder = new ProcessBuilder(command*)
    val process = builder.start()

    startCrawling(process.getInputStream)(System.out.println)
    startCrawling(process.getErrorStream)(System.err.println)

    val exitValue = process.waitFor()
    if exitValue != 0 then throw new IllegalArgumentException(s"Cannot compile $srcFile")

    val mainEntry = ClasspathEntry(name, classDir)
    Module(mainEntry, Seq(mainEntry))
  end fromJavaSource

  private def startCrawling(input: InputStream)(f: String => Unit): Unit =
    val reader = new BufferedReader(new InputStreamReader(input))
    val thread = new Thread:
      override def run(): Unit =
        var terminated = false
        try
          while (!terminated) do
            val line = reader.readLine()
            if line == null then terminated = true else f(line)
          input.close()
        catch case NonFatal(_) => ()
    thread.start()
