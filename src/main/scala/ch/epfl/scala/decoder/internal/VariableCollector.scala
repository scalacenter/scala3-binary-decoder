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
    val collector = VariableCollector()
    mtd.tree.toSet.flatMap(collector.collect)

trait LocalVariable:
  def sym: TermSymbol
  def startLine: Int
  def endLine: Int

object LocalVariable:
  case class ValDef(sym: TermSymbol, sourceFile: SourceFile, startLine: Int, endLine: Int) extends LocalVariable

  case class InlinedFromDef(underlying: LocalVariable, inlineCall: InlineCall) extends LocalVariable:
    def sym: TermSymbol = underlying.sym
    def startLine: Int = underlying.startLine
    def endLine: Int = underlying.endLine

end LocalVariable

class VariableCollector()(using Context, ThrowOrWarn) extends TreeTraverser:
  private val inlinedTrees = mutable.Map.empty[TermSymbol, Set[LocalVariable]]

  def collect(initialTree: Tree): Set[LocalVariable] =
    val variables: mutable.Set[LocalVariable] = mutable.Set.empty
    var previousTree: mutable.Stack[Tree] = mutable.Stack.empty

    object Traverser extends TreeTraverser:
      override def traverse(tree: Tree): Unit =
        tree match
          case valDefOrBind: (ValDef | Bind) => addValDefOrBind(valDefOrBind)
          case _ => ()

        tree match
          case _: TypeTree => ()
          // case _: DefDef => ()
          case valDef: ValDef => traverse(valDef.rhs)
          case bind: Bind => traverse(bind.body)
          case InlineCall(inlineCall) =>
            val liftedTrees = inlinedTrees.getOrElseUpdate(inlineCall.symbol, collectInlineDef(inlineCall.symbol))
            variables ++= liftedTrees.map(LocalVariable.InlinedFromDef(_, inlineCall))
            previousTree.push(inlineCall.termRefTree)
            super.traverse(inlineCall.termRefTree)
            previousTree.pop()
          case _ =>
            previousTree.push(tree)
            super.traverse(tree)
            previousTree.pop()

      private def addValDefOrBind(valDef: ValDef | Bind): Unit =
        val sym = valDef.symbol.asInstanceOf[TermSymbol]
        val parentTree = previousTree.top
        variables +=
          LocalVariable.ValDef(
            sym,
            parentTree.pos.sourceFile,
            valDef.pos.startLine + 1,
            parentTree.pos.endLine + 1
          )

    Traverser.traverse(initialTree)
    val dfsd = variables.toSet
    val y = dfsd
    dfsd

  private def collectInlineDef(symbol: TermSymbol): Set[LocalVariable] =
    inlinedTrees(symbol) = Set.empty // break recursion
    val x = symbol.tree.toSet.flatMap(collect(_))
    val y = x
    x

  // private def extractRHS(tree: DefTree): Option[TermTree] =
  //   tree match
  //     case tree: DefDef => tree.rhs
  //     case _ => None
