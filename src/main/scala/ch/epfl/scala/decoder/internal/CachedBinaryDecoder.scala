package ch.epfl.scala.decoder.internal

import ch.epfl.scala.decoder.binary
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import ch.epfl.scala.decoder.*

import scala.collection.concurrent.TrieMap
import scala.util.Try

class CachedBinaryDecoder(using Context, ThrowOrWarn) extends BinaryDecoder:
  // even failures can be quite expensive, so we cache them
  private val classCache: TrieMap[String, Try[DecodedClass]] = TrieMap.empty
  private val methodCache: TrieMap[(String, binary.SignedName), Try[DecodedMethod]] = TrieMap.empty
  private val liftedTreesCache: TrieMap[Symbol, Try[Seq[LiftedTree[?]]]] = TrieMap.empty

  override def decode(cls: binary.ClassType): DecodedClass =
    classCache.getOrElseUpdate(cls.name, Try(super.decode(cls))).get

  override def decode(method: binary.Method): DecodedMethod =
    methodCache.getOrElseUpdate((method.declaringClass.name, method.signedName), Try(super.decode(method))).get

  override protected def collectAllLiftedTrees(owner: Symbol): Seq[LiftedTree[?]] =
    liftedTreesCache.getOrElseUpdate(owner, Try(super.collectAllLiftedTrees(owner))).get
