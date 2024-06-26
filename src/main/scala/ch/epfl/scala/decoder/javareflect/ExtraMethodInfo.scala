package ch.epfl.scala.decoder.javareflect

import ch.epfl.scala.decoder.binary
import org.objectweb.asm

private case class ExtraMethodInfo(
    sourceLines: Option[binary.SourceLines],
    instructions: Seq[binary.Instruction],
    variables: Seq[ExtraMethodInfo.Variable],
    labels: Map[asm.Label, Int]
)

private object ExtraMethodInfo:
  def empty: ExtraMethodInfo = ExtraMethodInfo(None, Seq.empty, Seq.empty, Map.empty)
  case class Variable(name: String, descriptor: String, signature: String, start: asm.Label, end: asm.Label)
  case class LineNumberNode(line: Int, start: asm.Label, sourceName: String)
