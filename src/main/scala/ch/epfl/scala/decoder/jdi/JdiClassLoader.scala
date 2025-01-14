package ch.epfl.scala.decoder.jdi

import java.util as ju
import scala.jdk.CollectionConverters.*
import ch.epfl.scala.decoder.binary.BinaryClassLoader

class JdiClassLoader(classLoader: com.sun.jdi.ClassLoaderReference) extends BinaryClassLoader:
  override def loadClass(name: String): JdiClass =
    JdiClass(classLoader.visibleClasses.asScala.find(_.name == name).get)

  override def toString = classLoader.toString
