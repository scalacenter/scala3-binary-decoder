package ch.epfl.scala.decoder.testutils

case class ScalaVersion(value: String) extends Ordered[ScalaVersion]:

  def isScala2: Boolean = value.startsWith("2")
  def isScala3: Boolean = value.startsWith("3")
  def isScala212: Boolean = value.startsWith("2.12")
  def isScala213: Boolean = value.startsWith("2.13")
  def isScala33: Boolean = value.startsWith("3.3")
  def isScala34: Boolean = value.startsWith("3.4")

  def parts: (Int, Int, Int) =
    val regex = "(\\d+)\\.(\\d+)\\.(\\d+)(-.+)?".r
    regex
      .unapplySeq(value)
      .collect { case major :: minor :: patch :: tail => (major.toInt, minor.toInt, patch.toInt) }
      .get

  override def compare(that: ScalaVersion): Int =
    (parts, that.parts) match
      case ((x, _, _), (y, _, _)) if x != y => x - y
      case ((_, x, _), (_, y, _)) if x != y => x - y
      case ((_, _, x), (_, _, y)) if x != y => x - y
      case _ => 0

  def binaryVersion: String = if isScala3 then "3" else if  isScala213 then "2.13" else "2.12"

  def isRelease: Boolean = !value.contains("-")

  override def toString: String = value
end ScalaVersion

object ScalaVersion:
  val `3.lts` = ScalaVersion("3.3.3")
  val `3.next` = ScalaVersion("3.4.2") // TODO use BuildInfo ScalaVersion(BuildInfo.scala3Next)
