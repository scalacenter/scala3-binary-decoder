package ch.epfl.scala.decoder.jdi

import ch.epfl.scala.decoder.binary.*

class JdiVariable(variable: com.sun.jdi.LocalVariable, method: com.sun.jdi.Method) extends Variable:

  override def declaringMethod: Method =
    JdiMethod(method)

  override def name: String = variable.name
  override def sourceLines: Option[SourceLines] = None
  override def `type`: Type = JdiType(variable.`type`)
