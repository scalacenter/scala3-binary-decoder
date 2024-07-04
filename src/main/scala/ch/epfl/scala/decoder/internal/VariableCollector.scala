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
  def collectVariables(sym: TermSymbol)(using Context, ThrowOrWarn): Set[LocalVariable] =
    val collector = VariableCollector()
    collector.collect(sym)

trait LocalVariable:
  def sym: Symbol
  def startLine: Int
  def endLine: Int

object LocalVariable:
  case class This(sym: ClassSymbol, sourceFile: SourceFile, startLine: Int, endLine: Int) extends LocalVariable
  case class ValDef(sym: TermSymbol, sourceFile: SourceFile, startLine: Int, endLine: Int) extends LocalVariable
  case class InlinedFromDef(underlying: LocalVariable, inlineCall: InlineCall) extends LocalVariable:
    def sym: Symbol = underlying.sym
    def startLine: Int = underlying.startLine
    def endLine: Int = underlying.endLine

end LocalVariable

class VariableCollector()(using Context, ThrowOrWarn) extends TreeTraverser:
  private val inlinedVariables = mutable.Map.empty[TermSymbol, Set[LocalVariable]]

  def collect(sym: TermSymbol): Set[LocalVariable] =
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
            val localVariables =
              inlinedVariables.getOrElseUpdate(inlineCall.symbol, collectInlineDef(inlineCall.symbol))
            variables ++= localVariables.map(LocalVariable.InlinedFromDef(_, inlineCall))
            previousTree.push(inlineCall.termRefTree)
            super.traverse(inlineCall.termRefTree)
            previousTree.pop()
          case _ =>
            previousTree.push(tree)
            super.traverse(tree)
            previousTree.pop()

      private def addValDefOrBind(valDef: ValDef | Bind): Unit =
        val sym = valDef.symbol.asInstanceOf[TermSymbol]
        previousTree
          .collectFirst { case tree: (Block | CaseDef | DefDef) => tree }
          .foreach { parentTree =>
            variables +=
              LocalVariable.ValDef(
                sym,
                parentTree.pos.sourceFile,
                valDef.pos.startLine + 1,
                parentTree.pos.endLine + 1
              )
          }

    allOuterClasses(sym).foreach(cls =>
      variables += LocalVariable.This(
        cls,
        cls.pos.sourceFile,
        if cls.pos.isUnknown then -1 else cls.pos.startLine + 1,
        if cls.pos.isUnknown then -1 else cls.pos.endLine + 1
      )
    )
    sym.tree.foreach(Traverser.traverse)
    variables.toSet
  end collect

  private def collectInlineDef(symbol: TermSymbol): Set[LocalVariable] =
    inlinedVariables(symbol) = Set.empty // break recursion
    collect(symbol)

  private def allOuterClasses(sym: Symbol): List[ClassSymbol] =
    def loop(sym: Symbol, acc: List[ClassSymbol]): List[ClassSymbol] =
      sym.outerClass match
        case Some(cls) => loop(cls, cls :: acc)
        case None => acc
    loop(sym, Nil)
