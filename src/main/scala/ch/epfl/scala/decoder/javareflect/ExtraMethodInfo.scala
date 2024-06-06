package ch.epfl.scala.decoder.javareflect

import ch.epfl.scala.decoder.binary

private case class ExtraMethodInfo(sourceLines: Option[binary.SourceLines], instructions: Seq[binary.Instruction])

private object ExtraMethodInfo:
  def empty: ExtraMethodInfo = ExtraMethodInfo(None, Seq.empty)
