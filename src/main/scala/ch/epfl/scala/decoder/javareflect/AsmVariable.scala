package ch.epfl.scala.decoder.javareflect

import ch.epfl.scala.decoder.binary
import ch.epfl.scala.decoder.binary.Type
import ch.epfl.scala.decoder.binary.SourceLines
import ch.epfl.scala.decoder.binary.Method
import ch.epfl.scala.decoder.binary.Variable

class AsmVariable(val name: String, val asmType: String, val declaringMethod: Method) extends Variable:
  override def `type`: Type =
    new Type:

      override def sourceLines: Option[SourceLines] = None

      def name: String = asmType

  override def sourceLines: Option[SourceLines] = None

  override def toString: String = s"$name: $asmType"
