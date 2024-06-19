package ch.epfl.scala.decoder.internal

import tastyquery.Trees.*
import scala.collection.mutable
import tastyquery.Symbols.*
import tastyquery.Traversers.*
import tastyquery.Contexts.*
import tastyquery.SourcePosition
import tastyquery.Types.*
import tastyquery.Traversers
import ch.epfl.scala.decoder.ThrowOrWarn
import scala.languageFeature.postfixOps

object CaptureCollector:
  def collectCaptures(cls: ClassSymbol | TermSymbol)(using Context, ThrowOrWarn): Set[TermSymbol] =
    val collector = CaptureCollector(cls)
    collector.traverse(cls.tree)
    collector.capture.toSet

class CaptureCollector(cls: ClassSymbol | TermSymbol)(using Context, ThrowOrWarn) extends TreeTraverser:
  val capture: mutable.Set[TermSymbol] = mutable.Set.empty
  val alreadySeen: mutable.Set[Symbol] = mutable.Set.empty

  def loopCollect(symbol: Symbol)(collect: => Unit): Unit =
    if !alreadySeen.contains(symbol) then
      alreadySeen += symbol
      collect
  override def traverse(tree: Tree): Unit =
    tree match
      case _: TypeTree => ()
      case ident: Ident =>
        for sym <- ident.safeSymbol.collect { case sym: TermSymbol => sym } do
          // check that sym is local
          // and check that no owners of sym is cls
          if !alreadySeen.contains(sym) then
            if sym.isLocal then
              if !ownersIsCls(sym) then capture += sym
              if sym.isMethod || sym.isLazyVal then loopCollect(sym)(sym.tree.foreach(traverse))
              else if sym.isModuleVal then loopCollect(sym)(sym.moduleClass.flatMap(_.tree).foreach(traverse))
      case _ => super.traverse(tree)

    def ownersIsCls(sym: Symbol): Boolean =
      sym.owner match
        case owner: Symbol =>
          if owner == cls then true
          else ownersIsCls(owner)
        case null => false
