package ch.epfl.scala.decoder.javareflect

import ch.epfl.scala.decoder.binary

private final case class ExtraClassInfo(
    sourceLines: Option[binary.SourceLines],
    methodsInfo: Map[binary.SignedName, ExtraMethodInfo]
):
  def getMethodInfo(sig: binary.SignedName): ExtraMethodInfo = methodsInfo.getOrElse(sig, ExtraMethodInfo.empty)

private object ExtraClassInfo:
  def empty: ExtraClassInfo = ExtraClassInfo(None, Map.empty)
