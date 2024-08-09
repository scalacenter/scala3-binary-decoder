package ch.epfl.scala.decoder

import ch.epfl.scala.decoder.binary
import ch.epfl.scala.decoder.internal.*
import tastyquery.Contexts.Context
import tastyquery.Symbols.Symbol
import tastyquery.jdk.ClasspathLoaders

import java.nio.file.Path
import scala.collection.concurrent.TrieMap
import scala.util.Try

object BinaryDecoder:
  def apply(classEntries: Seq[Path])(using ThrowOrWarn): BinaryDecoder =
    val classpath = CustomClasspath(ClasspathLoaders.read(classEntries.toList))
    val ctx = Context.initialize(classpath)
    new BinaryDecoder(using ctx)

  def cached(classEntries: Seq[Path])(using ThrowOrWarn): BinaryDecoder =
    val classpath = CustomClasspath(ClasspathLoaders.read(classEntries.toList))
    val ctx = Context.initialize(classpath)
    new BinaryDecoder(using ctx):
      // we cache successes and failures, which can be quite expensive too
      private val classCache: TrieMap[String, Try[DecodedClass]] = TrieMap.empty
      private val methodCache: TrieMap[(String, binary.SignedName), Try[DecodedMethod]] = TrieMap.empty
      private val liftedTreesCache: TrieMap[Symbol, Try[Seq[LiftedTree[?]]]] = TrieMap.empty

      override def decode(cls: binary.ClassType): DecodedClass =
        classCache.getOrElseUpdate(cls.name, Try(super.decode(cls))).get

      override def decode(method: binary.Method): DecodedMethod =
        methodCache.getOrElseUpdate((method.declaringClass.name, method.signedName), Try(super.decode(method))).get

      override protected def collectAllLiftedTrees(owner: Symbol): Seq[LiftedTree[?]] =
        liftedTreesCache.getOrElseUpdate(owner, Try(super.collectAllLiftedTrees(owner))).get
    end new

class BinaryDecoder(using Context, ThrowOrWarn)
    extends BinaryClassDecoder,
      BinaryMethodDecoder,
      BinaryFieldDecoder,
      BinaryVariableDecoder
