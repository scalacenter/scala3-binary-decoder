package ch.epfl.scala.decoder.binary

trait Symbol:
  def name: String
  def sourceLines: Option[SourceLines]
  def sourceName: Option[String] = sourceLines.map(_.sourceName)

  def showSpan: String =
    sourceLines.map(_.showSpan).getOrElse("")
