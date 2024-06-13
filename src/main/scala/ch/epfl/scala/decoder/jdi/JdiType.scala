package ch.epfl.scala.decoder.jdi
import ch.epfl.scala.decoder.binary.*

class JdiType(tpe: com.sun.jdi.Type) extends Type:
  override def name: String = tpe.name
  override def sourceLines: Option[SourceLines] = None
