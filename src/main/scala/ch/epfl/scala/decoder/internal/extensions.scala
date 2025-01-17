package ch.epfl.scala.decoder.internal

import ch.epfl.scala.decoder.*
import ch.epfl.scala.decoder.binary
import ch.epfl.scala.decoder.binary.SourceLines
import tastyquery.Contexts.*
import tastyquery.Modifiers.*
import tastyquery.Names.*
import tastyquery.Signatures.*
import tastyquery.SourceLanguage
import tastyquery.SourcePosition
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*

import scala.annotation.tailrec
import scala.util.control.NonFatal

extension (symbol: Symbol)
  def isTrait = symbol.isClass && symbol.asClass.isTrait
  def isLocal = symbol.owner.isTerm
  def pos: SourcePosition = symbol.tree.map(_.pos).getOrElse(SourcePosition.NoPosition)
  def isInline = symbol.isTerm && symbol.asTerm.isInline
  def nameStr: String = symbol.name.toString

  def outerClass: Option[ClassSymbol] =
    symbol.owner match
      case null => None
      case owner: ClassSymbol => Some(owner)
      case owner => owner.outerClass

  def showBasic =
    val span = symbol.tree.map(_.pos) match
      case Some(pos) if pos.isFullyDefined =>
        if pos.startLine != pos.endLine then s"(${pos.startLine}, ${pos.endLine}))"
        else s"(${pos.startLine})"
      case _ => ""
    s"$symbol $span"

extension (symbol: ClassSymbol)
  def isAnonClass = symbol.name == CommonNames.anonClass

  /** The name of this class in the source, i.e., without the trailing `$` for module classes. */
  def sourceName: String = symbol.name match
    case ObjectClassTypeName(underlying) => underlying.toString()
    case name => name.toString()

extension (symbol: TermSymbol)
  def isGetter = !symbol.isMethod
  def isModuleOrLazyVal: Boolean = symbol.isLazyVal || symbol.isModuleVal
  def isLazyVal: Boolean = symbol.kind == TermSymbolKind.LazyVal
  def isVal: Boolean = symbol.kind == TermSymbolKind.Val
  def isDef: Boolean = symbol.kind == TermSymbolKind.Def
  def isVar: Boolean = symbol.kind == TermSymbolKind.Var
  def isAnonFun = symbol.name == CommonNames.anonFun
  def targetNameStr(using Context): String = symbol.targetName.toString
  def overridingSymbolInLinearization(siteClass: ClassSymbol)(using Context): TermSymbol =
    siteClass.linearization.iterator.flatMap(inClass => symbol.matchingSymbol(inClass, siteClass)).next
  def isOverridingSymbol(siteClass: DecodedClass)(using Context): Boolean =
    siteClass.classSymbol.exists(symbol.isOverridingSymbol)
  def isOverridingSymbol(siteClass: ClassSymbol)(using Context): Boolean =
    overridingSymbolInLinearization(siteClass) == symbol
  def isConstructor =
    symbol.owner.isClass && symbol.isMethod && symbol.name == nme.Constructor
  def isParamInInlineMethod =
    symbol.owner.isInline && symbol.owner.asTerm.paramSymbols.contains(symbol)

  def paramSymbols: List[TermSymbol] =
    symbol.tree.toList
      .collect { case tree: DefDef => tree.paramLists }
      .flatten
      .collect { case Left(params) => params }
      .flatten
      .map(_.symbol)

  def typeParamSymbols: List[LocalTypeParamSymbol] =
    symbol.tree.toList
      .collect { case tree: DefDef => tree.paramLists }
      .flatten
      .collect { case Right(typeParams) => typeParams }
      .flatten
      .map(_.symbol)
      .collect { case sym: LocalTypeParamSymbol => sym }

extension [A, S[+X] <: IterableOnce[X]](xs: S[A])
  def singleOpt: Option[A] =
    Option.when(xs.size == 1)(xs.iterator.next)

  def singleOrElse[B >: A](x: => B): B =
    if xs.size == 1 then xs.iterator.next else x

  def orIfEmpty[B >: A](ys: => S[B]): S[B] =
    if xs.nonEmpty then xs else ys

extension [T <: DecodedSymbol](xs: Seq[T])
  inline def singleOrThrow(symbol: binary.Symbol): T =
    singleOptOrThrow(symbol).getOrElse(notFound(symbol))

  inline def singleOrThrow(symbol: binary.Symbol, decodedOwner: DecodedSymbol): T =
    singleOptOrThrow(symbol).getOrElse(notFound(symbol, Some(decodedOwner)))

  inline def singleOptOrThrow(symbol: binary.Symbol): Option[T] =
    if xs.size > 1 then ambiguous(symbol, xs) else xs.headOption

extension (name: TermName)
  def isPackageObject: Boolean =
    val nameStr = name.toString
    nameStr == "package" || nameStr.endsWith("$package")

extension (name: ClassTypeName)
  def isPackageObjectClass: Boolean = name match
    case SimpleTypeName(_) => false
    case ObjectClassTypeName(underlying) => underlying.toTermName.isPackageObject

extension (tpe: TermType)
  def allParamTypes: List[Type] = tpe match
    case t: MethodType => t.paramTypes ++ t.resultType.allParamTypes
    case t: PolyType => t.resultType.allParamTypes
    case _ => Nil

  def allParamNames: List[UnsignedTermName] = tpe match
    case t: MethodType => t.paramNames ++ t.resultType.allParamNames
    case t: PolyType => t.resultType.allParamNames
    case _ => Nil

  def returnType: Type = tpe match
    case t: (MethodType | PolyType) => t.resultType.returnType
    case t: Type => t
    case t: PackageRef => unexpected("return type on package ref")

extension (tpe: Type)
  def safeDealias(using Context, ThrowOrWarn): Option[Type] =
    tryOrNone(tpe.dealias)

  def isOperatorLike: Boolean =
    tpe match
      case ref: TypeRef =>
        val operatorChars = "\\+\\-\\*\\/\\%\\&\\|\\^\\<\\>\\=\\!\\~\\#\\:\\@\\?"
        val regex = s"[^$operatorChars]".r
        !regex.findFirstIn(ref.name.toString()).isDefined
      case _ => false

  def erasedAsReturnType(using Context, ThrowOrWarn): Option[ErasedTypeRef] = erased(isReturnType = true)
  def erasedAsArgType(asJavaVarargs: Boolean = false)(using Context, ThrowOrWarn): Option[ErasedTypeRef] =
    tpe match
      case tpe: RepeatedType if asJavaVarargs =>
        ctx.defn.ArrayTypeOf(tpe.elemType).erased(isReturnType = false)
      case _ => tpe.erased(isReturnType = false)

  def expandContextFunctions(using Context, ThrowOrWarn): (List[Type], Type) =
    @tailrec def rec(tpe: Type, acc: List[Type]): (List[Type], Type) =
      tpe.safeDealias match
        case Some(tpe: AppliedType) if tpe.tycon.isContextFunction =>
          val argsAsTypes = tpe.args.map(_.highIfWildcard)
          rec(argsAsTypes.last, acc ::: argsAsTypes.init)
        case _ => (acc, tpe)
    rec(tpe, List.empty)

  private def erased(isReturnType: Boolean)(using Context, ThrowOrWarn): Option[ErasedTypeRef] =
    tryOrNone(ErasedTypeRef.erase(tpe, SourceLanguage.Scala3, keepUnit = isReturnType))

extension (tpe: TermType)
  def isNumberedTypeRefInScalaPackage(namePrefix: String): Boolean =
    tpe match
      case ref: TypeRef =>
        ref.prefix.isScalaPackage &&
        ref.name.match
          case SimpleTypeName(nameStr) => nameStr.startsWith(namePrefix)
          case _: ObjectClassTypeName | _: UniqueTypeName => false
      case tpe: AppliedType => tpe.tycon.isNumberedTypeRefInScalaPackage(namePrefix)
      case _ => false

  def isTuple: Boolean = isNumberedTypeRefInScalaPackage("Tuple")
  def isFunction: Boolean = isNumberedTypeRefInScalaPackage("Function")
  def isContextFunction: Boolean = isNumberedTypeRefInScalaPackage("ContextFunction")

extension (ref: TermRef)
  def isScalaPredef: Boolean =
    isScalaPackage(ref.prefix) && ref.name == CommonNames.Predef

extension (prefix: Prefix)
  def isScalaPackage: Boolean =
    prefix match
      case p: PackageRef => p.symbol.name == nme.scalaPackageName && p.symbol.owner.isRootPackage
      case _ => false

extension (tree: Tree)
  def matchLines(sourceLines: binary.SourceLines)(using Context): Boolean =
    tree match
      case lambda: Lambda => lambda.meth.symbol.pos.matchLines(sourceLines)
      case tree => tree.pos.matchLines(sourceLines)

extension (tree: Apply)
  def allArgsFlatten: Seq[TermTree] =
    def rec(fun: Tree): Seq[TermTree] = fun match
      case TypeApply(fun, _) => rec(fun)
      case Apply(fun, args) => rec(fun) ++ args
      case _ => Seq.empty
    rec(tree.fun) ++ tree.args

extension (tree: Apply)
  def safeMethodType(using Context, ThrowOrWarn): Option[MethodType] =
    tryOrNone(tree.methodType)

  def safeFunSymbol(using Context, ThrowOrWarn): Option[TermSymbol] =
    def rec(tree: Tree): Option[TermSymbol] =
      tree match
        case tree: Apply => rec(tree.fun)
        case tree: TypeApply => rec(tree.fun)
        case tree: TermReferenceTree => tree.safeSymbol.collect { case sym: TermSymbol => sym }
        case _ => None
    rec(tree)

extension (tree: TermReferenceTree)
  def safeSymbol(using Context, ThrowOrWarn): Option[PackageSymbol | TermSymbol] =
    tryOrNone(tree.symbol)

extension (tree: TermTree)
  def safeTpe(using Context, ThrowOrWarn): Option[TermType] =
    tryOrNone(tree.tpe)

extension (pos: SourcePosition)
  def isFullyDefined: Boolean =
    !pos.isUnknown && pos.hasLineColumnInformation

  def enclose(other: SourcePosition) =
    pos.isFullyDefined &&
      other.isFullyDefined &&
      pos.sourceFile == other.sourceFile && (
        (pos.startOffset <= other.startOffset && pos.endOffset > other.endOffset) ||
          (pos.startOffset < other.startOffset && pos.endOffset >= other.endOffset)
      )

  def containsLine(line: Int): Boolean =
    pos.isFullyDefined
      && pos.startLine <= line
      && pos.endLine >= line

  def matchLines(sourceLines: binary.SourceLines): Boolean =
    !pos.isFullyDefined
      || // we use endsWith instead of == because of tasty-query#434
        !pos.sourceFile.name.endsWith(sourceLines.sourceName)
        || sourceLines.tastySpan.forall(line => pos.startLine <= line && pos.endLine >= line)

  def showBasic =
    pos match
      case pos if pos.isFullyDefined =>
        if pos.startLine != pos.endLine then s"(${pos.startLine}, ${pos.endLine}))"
        else s"(${pos.startLine})"
      case _ => ""

extension (self: DecodedClass)
  def classSymbol: Option[ClassSymbol] =
    self match
      case self: DecodedClass.ClassDef => Some(self.symbol)
      case self: DecodedClass.InlinedClass => self.underlying.classSymbol
      case _ => None

  def isJava: Boolean = classSymbol.exists(_.sourceLanguage == SourceLanguage.Java)

  def isTrait: Boolean = classSymbol.exists(_.isTrait)

  def isModuleClass: Boolean = classSymbol.exists(_.isModuleClass)

  def declarations(using Context): Seq[Symbol] = classSymbol.toSeq.flatMap(_.declarations)

  def linearization(using Context): Seq[ClassSymbol] = classSymbol.toSeq.flatMap(_.linearization)

  def thisType(using Context): Option[Type] = self match
    case self: DecodedClass.SAMOrPartialFunction => Some(self.tpe)
    case inlined: DecodedClass.InlinedClass => inlined.underlying.thisType
    case _ => classSymbol.map(_.thisType)

  def companionClass(using Context): Option[DecodedClass] =
    self.companionClassSymbol.map(DecodedClass.ClassDef(_))

  def companionClassSymbol(using Context): Option[ClassSymbol] = self match
    case self: DecodedClass.ClassDef => self.symbol.companionClass
    case self: DecodedClass.SyntheticCompanionClass => Some(self.companionSymbol)
    case self: DecodedClass.InlinedClass => self.underlying.companionClassSymbol
    case _ => None

extension (self: binary.Symbol | binary.Instruction.Field | binary.Instruction.Method)
  def name: String = self match
    case self: binary.Symbol => self.name
    case self: binary.Instruction.Field => self.name
    case self: binary.Instruction.Method => self.name

  def isExpanded: Boolean =
    name.matches(".+\\$\\$(_\\$)*(.+)")

  def unexpandedDecodedNames: Seq[String] =
    val expanded = ".+\\$\\$(_\\$)*(.+)".r
    def unexpand(name: String) =
      name match
        case expanded(_, name) => name
        case _ => name
    Seq(name, NameTransformer.decode(unexpand(name)), unexpand(decodedName)).distinct

  def decodedName: String =
    NameTransformer.decode(name)

extension (field: binary.Instruction.Field)
  def isPut: Boolean =
    field.opcode == 0xb5 || field.opcode == 0xb3

extension (method: DecodedMethod)
  def isGenerated(using ctx: Context): Boolean =
    method match
      case method: DecodedMethod.ValOrDefDef =>
        val sym = method.symbol
        def isThreadUnsafe = sym.hasAnnotation(Definitions.threadUnsafeClass)
        (sym.isGetter && (!sym.isModuleOrLazyVal || !sym.owner.isTrait) && !isThreadUnsafe) || // getter
        (sym.isLocal && sym.isModuleOrLazyVal) || // local def
        sym.isSetter ||
        (sym.isSynthetic && !sym.isLocal) ||
        sym.isExport
      case method: DecodedMethod.LazyInit => method.symbol.owner.isTrait
      case _: DecodedMethod.TraitStaticForwarder => true
      case _: DecodedMethod.TraitParamAccessor => true
      case _: DecodedMethod.MixinForwarder => true
      case _: DecodedMethod.Bridge => true
      case _: DecodedMethod.StaticForwarder => true
      case _: DecodedMethod.OuterAccessor => true
      case _: DecodedMethod.SetterAccessor => true
      case _: DecodedMethod.GetterAccessor => true
      case _: DecodedMethod.SuperAccessor => true
      case _: DecodedMethod.SpecializedMethod => true
      case _: DecodedMethod.InlineAccessor => true
      case _: DecodedMethod.AdaptedFun => true
      case _: DecodedMethod.SAMOrPartialFunctionConstructor => true
      case method: DecodedMethod.InlinedMethod => method.underlying.isGenerated
      case _ => false
