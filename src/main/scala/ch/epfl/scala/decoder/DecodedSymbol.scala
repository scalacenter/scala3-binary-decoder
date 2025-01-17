package ch.epfl.scala.decoder

import ch.epfl.scala.decoder.internal.showBasic
import tastyquery.SourcePosition
import tastyquery.Symbols.*
import tastyquery.Trees.Tree
import tastyquery.Types.*

sealed trait DecodedSymbol:
  def symbolOpt: Option[ClassSymbol | TermSymbol] = None
  def treeOpt: Option[Tree] = symbolOpt.flatMap(_.tree)
  final def pos: SourcePosition = treeOpt.map(_.pos).getOrElse(SourcePosition.NoPosition)

sealed trait DecodedClass extends DecodedSymbol

// TODO equals/hashcode manually
object DecodedClass:
  final class ClassDef(val symbol: ClassSymbol) extends DecodedClass:
    override def symbolOpt: Option[ClassSymbol] = Some(symbol)
    override def toString: String = s"ClassDef(${symbol.showBasic})"

  final class SyntheticCompanionClass(val companionSymbol: ClassSymbol) extends DecodedClass:
    override def toString: String = s"SyntheticCompanionClass(${companionSymbol.showBasic})"

  final class SAMOrPartialFunction(val symbol: TermSymbol, val parentClass: ClassSymbol, val tpe: Type)
      extends DecodedClass:
    override def symbolOpt: Option[TermSymbol] = Some(symbol)
    override def toString: String = s"SAMOrPartialFunction(${symbol.showBasic}, ${tpe.showBasic})"

  final class InlinedClass(val underlying: DecodedClass, val callTree: Tree) extends DecodedClass:
    override def symbolOpt: Option[ClassSymbol | TermSymbol] = underlying.symbolOpt
    def callPos: SourcePosition = callTree.pos
    override def toString: String =
      if underlying.isInstanceOf[InlinedClass] then underlying.toString
      else s"$underlying (inlined)"

sealed trait DecodedMethod extends DecodedSymbol:
  def owner: DecodedClass
  override def symbolOpt: Option[TermSymbol] = None
  def declaredType: TypeOrMethodic

object DecodedMethod:
  final class ValOrDefDef(val owner: DecodedClass, val symbol: TermSymbol) extends DecodedMethod:
    override def declaredType: TypeOrMethodic = symbol.declaredType
    override def symbolOpt: Option[TermSymbol] = Some(symbol)
    override def toString: String = s"ValOrDefDef($owner, ${symbol.showBasic})"

  final class LazyInit(val owner: DecodedClass, val symbol: TermSymbol) extends DecodedMethod:
    override def declaredType: TypeOrMethodic = symbol.declaredType
    override def symbolOpt: Option[TermSymbol] = Some(symbol)
    override def toString: String = s"LazyInit($owner, ${symbol.showBasic})"

  final class TraitParamAccessor(val owner: DecodedClass, val symbol: TermSymbol) extends DecodedMethod:
    override def declaredType: TypeOrMethodic = symbol.declaredType
    override def symbolOpt: Option[TermSymbol] = Some(symbol)
    override def toString: String = s"TraitParamAccessor($owner, ${symbol.showBasic})"

  final class MixinForwarder(val owner: DecodedClass, val target: DecodedMethod) extends DecodedMethod:
    override def declaredType: TypeOrMethodic = target.declaredType
    override def symbolOpt: Option[TermSymbol] = target.symbolOpt
    override def toString: String = s"MixinForwarder($owner, $target)"

  final class TraitStaticForwarder(val target: DecodedMethod) extends DecodedMethod:
    override def owner: DecodedClass = target.owner
    override def declaredType: TypeOrMethodic = target.declaredType
    override def symbolOpt: Option[TermSymbol] = target.symbolOpt
    override def toString: String = s"TraitStaticForwarder($target)"

  final class OuterAccessor(val owner: DecodedClass, val declaredType: Type) extends DecodedMethod:
    override def toString: String = s"OuterAccessor($owner, ${declaredType.showBasic})"

  final class SuperConstructorArg(
      val owner: DecodedClass,
      val treeOwner: ClassSymbol,
      val argTree: Tree,
      val declaredType: Type
  ) extends DecodedMethod:
    override def treeOpt: Option[Tree] = Some(argTree)
    override def toString: String =
      s"SuperConstructorArg($owner, ${treeOwner.showBasic}, ${argTree.pos.showBasic}, ${declaredType.showBasic})"

  final class LiftedTry(val owner: DecodedClass, val treeOwner: Symbol, val tryTree: Tree, val declaredType: Type)
      extends DecodedMethod:
    override def treeOpt: Option[Tree] = Some(tryTree)
    override def toString: String =
      s"LiftedTry($owner, ${treeOwner.showBasic}, ${tryTree.pos.showBasic}, ${declaredType.showBasic})"

  final class ByNameArg(val owner: DecodedClass, val treeOwner: Symbol, val argTree: Tree, val declaredType: Type)
      extends DecodedMethod:
    override def treeOpt: Option[Tree] = Some(argTree)
    override def toString: String =
      s"ByNameArg($owner, ${treeOwner.showBasic}, ${argTree.pos.showBasic}, ${declaredType.showBasic})"

  final class Bridge(val target: DecodedMethod, val declaredType: TypeOrMethodic) extends DecodedMethod:
    override def owner: DecodedClass = target.owner
    override def symbolOpt: Option[TermSymbol] = target.symbolOpt
    override def toString: String = s"Bridge($target, ${declaredType.showBasic})"

  final class SAMOrPartialFunctionImpl(
      val owner: DecodedClass,
      val implementedSymbol: TermSymbol,
      val declaredType: TypeOrMethodic
  ) extends DecodedMethod:
    override def symbolOpt: Option[TermSymbol] = owner.symbolOpt.collect { case sym: TermSymbol => sym }
    override def treeOpt: Option[Tree] = owner.treeOpt
    override def toString: String =
      s"SAMOrPartialFunctionImpl($owner, ${implementedSymbol.showBasic}, ${declaredType.showBasic})"

  final class StaticForwarder(
      val owner: DecodedClass,
      val target: DecodedMethod,
      val declaredType: TypeOrMethodic
  ) extends DecodedMethod:
    override def symbolOpt: Option[TermSymbol] = target.symbolOpt
    override def toString: String = s"StaticForwarder($owner, $target, ${declaredType.showBasic})"

  final class DeserializeLambda(val owner: DecodedClass, val declaredType: TypeOrMethodic) extends DecodedMethod:
    override def toString: String = s"DeserializeLambda($owner, ${declaredType.showBasic})"

  final class SetterAccessor(val owner: DecodedClass, val symbol: TermSymbol, val declaredType: TypeOrMethodic)
      extends DecodedMethod:
    override def symbolOpt: Option[TermSymbol] = Option.when(symbol.isMethod)(symbol)
    override def toString: String = s"SetterAccessor($owner, ${symbol.showBasic}, ${declaredType.showBasic})"

  final class GetterAccessor(val owner: DecodedClass, val symbol: TermSymbol, val declaredType: TypeOrMethodic)
      extends DecodedMethod:
    override def symbolOpt: Option[TermSymbol] = Some(symbol)
    override def toString: String = s"GetterAccessor($owner, ${symbol.showBasic}, ${declaredType.showBasic})"

  final class SuperAccessor(val owner: DecodedClass, val symbol: TermSymbol, val declaredType: TypeOrMethodic)
      extends DecodedMethod:
    override def symbolOpt: Option[TermSymbol] = Some(symbol)
    override def toString: String = s"SuperAccessor($owner, ${symbol.showBasic}, ${declaredType.showBasic})"

  final class SpecializedMethod(val owner: DecodedClass, val symbol: TermSymbol) extends DecodedMethod:
    override def declaredType: TypeOrMethodic = symbol.declaredType
    override def symbolOpt: Option[TermSymbol] = Some(symbol)
    override def toString: String = s"SpecializedMethod($owner, ${symbol.showBasic})"

  final class InlineAccessor(val owner: DecodedClass, val target: DecodedMethod) extends DecodedMethod:
    override def declaredType: TypeOrMethodic = target.declaredType
    override def symbolOpt: Option[TermSymbol] = target.symbolOpt
    override def toString: String = s"InlineAccessor($owner, $target)"

  final class AdaptedFun(val target: DecodedMethod) extends DecodedMethod:
    override def owner: DecodedClass = target.owner
    override def declaredType: TypeOrMethodic = target.declaredType
    override def symbolOpt: Option[TermSymbol] = target.symbolOpt
    override def treeOpt: Option[Tree] = target.treeOpt
    override def toString: String = s"AdaptedFun($target)"

  final class SAMOrPartialFunctionConstructor(val owner: DecodedClass, val declaredType: Type) extends DecodedMethod:
    override def symbolOpt: Option[TermSymbol] = None
    override def toString: String = s"AdaptedFun($owner, ${declaredType.showBasic})"

  final class InlinedMethod(val underlying: DecodedMethod, val callTree: Tree) extends DecodedMethod:
    override def owner: DecodedClass = underlying.owner
    override def declaredType: TypeOrMethodic = underlying.declaredType
    override def symbolOpt: Option[TermSymbol] = underlying.symbolOpt
    def callPos: SourcePosition = callTree.pos
    override def toString: String =
      if underlying.isInstanceOf[InlinedMethod] then underlying.toString
      else s"$underlying (inlined)"

sealed trait DecodedField extends DecodedSymbol:
  def owner: DecodedClass
  override def symbolOpt: Option[TermSymbol] = None
  def declaredType: TypeOrMethodic

object DecodedField:
  final class ValDef(val owner: DecodedClass, val symbol: TermSymbol) extends DecodedField:
    def declaredType: TypeOrMethodic = symbol.declaredType
    override def symbolOpt: Option[TermSymbol] = Some(symbol)
    override def toString: String = s"ValDef($owner, ${symbol.showBasic})"

  final class ModuleVal(val owner: DecodedClass, val symbol: TermSymbol) extends DecodedField:
    def declaredType: TypeOrMethodic = symbol.declaredType
    override def symbolOpt: Option[TermSymbol] = Some(symbol)
    override def toString: String = s"ModuleVal($owner, ${symbol.showBasic})"

  final class LazyValOffset(val owner: DecodedClass, val index: Int, val declaredType: Type) extends DecodedField:
    override def toString: String = s"LazyValOffset($owner, $index)"

  final class Outer(val owner: DecodedClass, val declaredType: Type) extends DecodedField:
    override def toString: String = s"Outer($owner, ${declaredType.showBasic})"

  final class SerialVersionUID(val owner: DecodedClass, val declaredType: Type) extends DecodedField:
    override def toString: String = s"SerialVersionUID($owner)"

  final class Capture(val owner: DecodedClass, val symbol: TermSymbol) extends DecodedField:
    def declaredType: TypeOrMethodic = symbol.declaredType
    override def toString: String = s"Capture($owner, ${symbol.showBasic})"

  final class LazyValBitmap(val owner: DecodedClass, val declaredType: Type, val name: String) extends DecodedField:
    override def toString: String = s"LazyValBitmap($owner, ${declaredType.showBasic})"

sealed trait DecodedVariable extends DecodedSymbol:
  def owner: DecodedMethod
  override def symbolOpt: Option[TermSymbol] = None
  def declaredType: TypeOrMethodic

object DecodedVariable:
  final class ValDef(val owner: DecodedMethod, val symbol: TermSymbol) extends DecodedVariable:
    def declaredType: TypeOrMethodic = symbol.declaredType
    override def symbolOpt: Option[TermSymbol] = Some(symbol)
    override def toString: String = s"ValDef($owner, ${symbol.showBasic})"

  final class CapturedVariable(val owner: DecodedMethod, val symbol: TermSymbol) extends DecodedVariable:
    def declaredType: TypeOrMethodic = symbol.declaredType
    override def symbolOpt: Option[TermSymbol] = Some(symbol)
    override def toString: String = s"VariableCapture($owner, ${symbol.showBasic})"

  final class This(val owner: DecodedMethod, val declaredType: Type) extends DecodedVariable:
    override def toString: String = s"This($owner, ${declaredType.showBasic})"

  final class OuterParam(val owner: DecodedMethod, val declaredType: Type) extends DecodedVariable:
    override def toString: String = s"OuterParam($owner, ${declaredType.showBasic})"

  final class AnyValThis(val owner: DecodedMethod, val symbol: TermSymbol) extends DecodedVariable:
    def declaredType: TypeOrMethodic = symbol.declaredType
    override def symbolOpt: Option[TermSymbol] = Some(symbol)
    override def toString: String = s"AnyValThis($owner, ${declaredType.showBasic})"

  final class SetterParam(val owner: DecodedMethod.SetterAccessor, val declaredType: Type) extends DecodedVariable:
    override def toString: String = s"SetterParam($owner, ${declaredType.showBasic})"
