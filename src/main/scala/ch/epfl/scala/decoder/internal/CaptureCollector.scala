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
  def collectCaptures(tree: Tree)(using Context, ThrowOrWarn): Set[TermSymbol] =
    val collector = CaptureCollector()
    collector.traverse(tree)
    collector.capture.toSet

class CaptureCollector(using Context, ThrowOrWarn) extends TreeTraverser:
  private val capture: mutable.Set[TermSymbol] = mutable.Set.empty
  private val alreadySeen: mutable.Set[Symbol] = mutable.Set.empty

  def loopCollect(symbol: Symbol)(collect: => Unit): Unit =
    if !alreadySeen.contains(symbol) then
      alreadySeen += symbol
      collect
  override def traverse(tree: Tree): Unit =
    tree match
      case _: TypeTree => ()
      case valDef: ValDef =>
        alreadySeen += valDef.symbol
        traverse(valDef.rhs)
      case bind: Bind =>
        alreadySeen += bind.symbol
        traverse(bind.body)
      case ident: Ident =>
        for sym <- ident.safeSymbol.collect { case sym: TermSymbol => sym } do
          if !alreadySeen.contains(sym) && sym.isLocal then
            if !sym.isMethod then capture += sym
            if sym.isMethod || sym.isLazyVal then loopCollect(sym)(sym.tree.foreach(traverse))
            else if sym.isModuleVal then loopCollect(sym)(sym.moduleClass.flatMap(_.tree).foreach(traverse))
      case _ => super.traverse(tree)
