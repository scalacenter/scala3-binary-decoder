package ch.epfl.scala.decoder.javareflect

import ch.epfl.scala.decoder.binary
import org.objectweb.asm
import scala.collection.SeqMap
import ch.epfl.scala.decoder.binary.SourceLines

private case class ExtraMethodInfo(
    sourceLines: Option[binary.SourceLines],
    instructions: Seq[binary.Instruction],
    variables: Seq[ExtraMethodInfo.Variable],
    labelLines: SeqMap[asm.Label, Int]
):
  val labels = labelLines.keys.toSeq

  // not parameters
  def localVariables: Seq[ExtraMethodInfo.Variable] =
    variables.filter(v => v.start != labelLines.head(0) || v.name == "this")

  def debugLines(variable: ExtraMethodInfo.Variable): Seq[Int] =
    val startIdx = labels.indexOf(variable.start)
    val endIdx = labels.indexOf(variable.end)
    labelLines.values.slice(startIdx, endIdx).toSeq.distinct

private object ExtraMethodInfo:
  def empty: ExtraMethodInfo = ExtraMethodInfo(None, Seq.empty, Seq.empty, SeqMap.empty)
  case class Variable(name: String, descriptor: String, signature: String, start: asm.Label, end: asm.Label)
