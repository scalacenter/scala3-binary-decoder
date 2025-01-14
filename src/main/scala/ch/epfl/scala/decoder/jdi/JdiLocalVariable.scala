package ch.epfl.scala.decoder.jdi

import ch.epfl.scala.decoder.binary.*

class JdiLocalVariable(localVariable: com.sun.jdi.LocalVariable) extends Parameter:
  override def name: String = localVariable.name
  override def sourceLines: Option[SourceLines] = None
  override def `type`: Type = JdiType(localVariable.`type`)

  override def toString: String = localVariable.toString
