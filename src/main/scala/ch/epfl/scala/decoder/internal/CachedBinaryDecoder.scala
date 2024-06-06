package ch.epfl.scala.decoder.internal

import ch.epfl.scala.decoder.binary
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import ch.epfl.scala.decoder.*

import scala.collection.concurrent.TrieMap

class CachedBinaryDecoder(using Context, ThrowOrWarn) extends BinaryDecoder:
  private val classCache: TrieMap[String, DecodedClass] = TrieMap.empty
  private val methodCache: TrieMap[(String, binary.SignedName), DecodedMethod] = TrieMap.empty
  private val liftedTreesCache: TrieMap[Symbol, Seq[LiftedTree[?]]] = TrieMap.empty

  override def decode(cls: binary.ClassType): DecodedClass =
    classCache.getOrElseUpdate(cls.name, super.decode(cls))

  override def decode(method: binary.Method): DecodedMethod =
    methodCache.getOrElseUpdate((method.declaringClass.name, method.signedName), super.decode(method))

  override protected def collectAllLiftedTrees(owner: Symbol): Seq[LiftedTree[?]] =
    liftedTreesCache.getOrElseUpdate(owner, super.collectAllLiftedTrees(owner))
