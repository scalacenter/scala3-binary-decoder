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
import tastyquery.SourceFile

object VariableCollector:
  def collectVariables(mtd: TermSymbol)(using Context, ThrowOrWarn): Set[LocalVariable] =
    mtd.tree.toSet.flatMap { tree =>
      val collector = VariableCollector(tree)
      collector.collect()
    }

case class LocalVariable(sym: TermSymbol, sourceFile: SourceFile, startLine: Int, endLine: Int)

class VariableCollector(initialTree: Tree)(using Context, ThrowOrWarn) extends TreeTraverser:
  val variables: mutable.Set[LocalVariable] = mutable.Set.empty
  var previousTree: mutable.Stack[Tree] = mutable.Stack(initialTree)

  def collect(): Set[LocalVariable] =
    super.traverse(initialTree)
    variables.toSet

  override def traverse(tree: Tree): Unit =
    tree match
      case _: TypeTree => ()
      case _: DefDef => ()
      case valDef: ValDef =>
        val sym = valDef.symbol.asInstanceOf[TermSymbol]
        val parentTree = previousTree.top
        variables +=
          LocalVariable(
            sym,
            parentTree.pos.sourceFile,
            valDef.pos.startLine + 1,
            parentTree.pos.endLine + 1
          )
        traverse(valDef.rhs)
      case bind: Bind =>
        val parentTree = previousTree.top
        val sym = bind.symbol.asInstanceOf[TermSymbol]
        variables += LocalVariable(sym, parentTree.pos.sourceFile, bind.pos.startLine + 1, bind.pos.endLine + 1)
        traverse(bind.body)
      case _ =>
        previousTree.push(tree)
        super.traverse(tree)
        previousTree.pop()
