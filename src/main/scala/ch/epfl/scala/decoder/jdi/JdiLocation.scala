package ch.epfl.scala.decoder.jdi

class JdiLocation(val obj: Any) extends JavaReflection(obj, "com.sun.jdi.Location"):
  def lineNumber: Int = invokeMethod[Int]("lineNumber")
