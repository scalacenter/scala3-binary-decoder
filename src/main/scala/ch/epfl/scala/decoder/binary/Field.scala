package ch.epfl.scala.decoder.binary

trait Field extends Symbol:
  def declaringClass: ClassType
  def `type`: Type
  def isStatic: Boolean
