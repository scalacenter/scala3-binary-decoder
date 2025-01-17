package ch.epfl.scala.decoder.internal

import tastyquery.Trees.*
import tastyquery.Symbols.*
import tastyquery.Types.*
import tastyquery.Contexts.*
import tastyquery.SourcePosition
import tastyquery.decoder.Substituters
import ch.epfl.scala.decoder.ThrowOrWarn

case class InlineCall private (
    termRefTree: TermReferenceTree,
    typeArgs: List[Type],
    args: Seq[TermTree],
    callTree: Tree
):
  def symbol(using Context): TermSymbol = termRefTree.symbol.asTerm

  def pos: SourcePosition = callTree.pos

  def substTypeParams(tpe: TermType)(using Context): TermType =
    Substituters.substLocalTypeParams(tpe, symbol.typeParamSymbols, typeArgs)

  def substTypeParams(tpe: Type)(using Context): Type =
    Substituters.substLocalTypeParams(tpe, symbol.typeParamSymbols, typeArgs)

  def paramsMap(using Context): Map[TermSymbol, TermTree] =
    symbol.paramSymbols.zip(args).toMap

  def paramTypes(using Context): Seq[Type] =
    symbol.declaredType.allParamTypes

object InlineCall:
  def unapply(fullTree: Tree)(using Context, ThrowOrWarn): Option[InlineCall] =
    def rec(tree: Tree, typeArgsAcc: List[Type], argsAcc: Seq[TermTree]): Option[InlineCall] =
      tree match
        case termTree: TermReferenceTree if termTree.safeSymbol.exists(sym => sym.isInline && sym.asTerm.isMethod) =>
          Some(InlineCall(termTree, typeArgsAcc, argsAcc, fullTree))
        case Apply(fun, args) => rec(fun, typeArgsAcc, args ++ argsAcc)
        case TypeApply(fun, typeArgs) => rec(fun, typeArgs.map(_.toType) ++ typeArgsAcc, argsAcc)
        case _ => None
    fullTree match
      case tree: TermTree if tree.safeTpe.exists(!_.isInstanceOf[MethodicType]) => rec(tree, List.empty, Seq.empty)
      case _ => None
