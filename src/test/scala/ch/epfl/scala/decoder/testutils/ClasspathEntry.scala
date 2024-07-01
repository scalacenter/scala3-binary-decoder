package ch.epfl.scala.decoder.testutils

import java.net.URL
import java.nio.file.Path

case class ClasspathEntry(name: String, absolutePath: Path):
  def toURL: URL = absolutePath.toUri.toURL
  def isJar: Boolean = absolutePath.toString.endsWith(".jar")
