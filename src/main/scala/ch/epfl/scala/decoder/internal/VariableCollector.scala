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

object VariableCollector:
  def collectVariables(mtd: TermSymbol)(using Context, ThrowOrWarn): Set[TermSymbol] =
    val collector = VariableCollector(mtd)
    collector.traverse(mtd.tree)
    collector.variable.toSet

class VariableCollector(mtd: TermSymbol)(using Context, ThrowOrWarn) extends TreeTraverser:
  val variable: mutable.Set[TermSymbol] = mutable.Set.empty

  override def traverse(tree: Tree): Unit =
    tree match
      case _: TypeTree => ()
      case ident: Ident =>
        for sym <- ident.safeSymbol.collect { case sym: TermSymbol => sym } do
          if !variable.contains(sym) then
            if sym.isLocal then
              variable += sym
              if sym.isMethod || sym.isLazyVal then sym.tree.foreach(traverse)
              else if sym.isModuleVal then sym.moduleClass.flatMap(_.tree).foreach(traverse)
      //   case defDef: DefDef =>
      //     // Process each ParamsClause in paramLists, each ParamsClause contains multiple parameters
      //     defDef.paramLists.foreach(paramsClause =>
      //     paramsClause.foreach(list => list.foreach(valDef => {
      //         val sym = valDef.symbol.asInstanceOf[TermSymbol]
      //         if !variable.contains(sym) && sym.isLocal then
      //         variable += sym
      //     }))
      //     )
      //     defDef.paramLists.foreach(edq => )
      //     traverse(defDef.rhs)
      case _ => super.traverse(tree)
