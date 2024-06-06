package ch.epfl.scala.decoder.testutils

import java.lang.management.ManagementFactory
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Properties

trait CommonFunSuite extends munit.FunSuite:
  override def munitTimeout: Duration = if isDebug then 8.hours else super.munitTimeout

  def isJava8: Boolean = Properties.javaVersion.startsWith("1.8")

  def isDebug: Boolean =
    val mxBean = ManagementFactory.getRuntimeMXBean
    mxBean.getInputArguments.asScala.exists(_.contains("jdwp"))

