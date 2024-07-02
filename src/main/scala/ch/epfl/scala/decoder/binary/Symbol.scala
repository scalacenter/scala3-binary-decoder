package ch.epfl.scala.decoder.binary

trait Symbol:
  def name: String
  def sourceLines: Option[SourceLines]

  def showSpan: String =
    sourceLines.map(_.showSpan).getOrElse("")
