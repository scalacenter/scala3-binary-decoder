package ch.epfl.scala.decoder

import ch.epfl.scala.decoder.internal.*
import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.SourcePosition
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*

import scala.util.matching.Regex

trait BinaryMethodDecoder(using Context, ThrowOrWarn):
  self: BinaryClassDecoder =>
  def decode(method: binary.Method): DecodedMethod =
    val decodedClass = decode(method.declaringClass)
    decode(decodedClass, method)

  def decode(decodedClass: DecodedClass, method: binary.Method): DecodedMethod =
    def tryDecode(f: PartialFunction[binary.Method, Seq[DecodedMethod]]): Seq[DecodedMethod] =
      f.applyOrElse(method, _ => Seq.empty[DecodedMethod])

    extension (xs: Seq[DecodedMethod])
      def orTryDecode(f: PartialFunction[binary.Method, Seq[DecodedMethod]]): Seq[DecodedMethod] =
        if xs.nonEmpty then xs else f.applyOrElse(method, _ => Seq.empty[DecodedMethod])
    val candidates =
      tryDecode {
        // static and/or bridge
        case Patterns.AdaptedAnonFun() => decodeAdaptedAnonFun(decodedClass, method)
        // bridge or standard
        case Patterns.SpecializedMethod(names) => decodeSpecializedMethod(decodedClass, method, names)
        // bridge only
        case m if m.isBridge => decodeBridgesAndMixinForwarders(decodedClass, method).toSeq
        // static or standard
        case Patterns.AnonFun() => decodeAnonFunsAndReduceAmbiguity(decodedClass, method)
        case Patterns.ByNameArgProxy() => decodeByNameArgsProxy(decodedClass, method)
        case Patterns.SuperArg() => decodeSuperArgs(decodedClass, method)
        case Patterns.LiftedTree() => decodeLiftedTries(decodedClass, method)
        case Patterns.LocalLazyInit(names) => decodeLocalLazyInit(decodedClass, method, names)
        // static only
        case Patterns.TraitInitializer() => decodeTraitInitializer(decodedClass, method)
        case Patterns.DeserializeLambda() =>
          Seq(DecodedMethod.DeserializeLambda(decodedClass, Definitions.DeserializeLambdaType))
        case Patterns.TraitStaticForwarder() => decodeTraitStaticForwarder(decodedClass, method).toSeq
        case m if m.isStatic && decodedClass.isJava => decodeStaticJavaMethods(decodedClass, method)
        // cannot be static anymore
        case Patterns.LazyInit(name) => decodeLazyInit(decodedClass, name)
        case Patterns.Outer() => decodeOuter(decodedClass).toSeq
        case Patterns.ParamForwarder(names) => decodeParamForwarder(decodedClass, method, names)
        case Patterns.TraitSetter(name) => decodeTraitSetter(decodedClass, method, name)
        case Patterns.Setter(names) =>
          decodeStandardMethods(decodedClass, method).orIfEmpty(decodeSetter(decodedClass, method, names))
        case Patterns.SuperAccessor(names) => decodeSuperAccessor(decodedClass, method, names)
      }
        .orTryDecode { case Patterns.ValueClassExtension() => decodeValueClassExtension(decodedClass, method) }
        .orTryDecode { case Patterns.InlineAccessor(names) => decodeInlineAccessor(decodedClass, method, names).toSeq }
        .orTryDecode { case Patterns.LocalMethod(names) => decodeLocalMethods(decodedClass, method, names) }
        .orTryDecode {
          case m if m.isStatic => decodeStaticForwarder(decodedClass, method)
          case _ => decodeStandardMethods(decodedClass, method)
        }

    candidates.singleOrThrow(method)
  end decode

  private def decodeStaticJavaMethods(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    decodedClass.companionClassSymbol.toSeq
      .flatMap(_.declarations)
      .collect {
        case sym: TermSymbol
            if matchTargetName(method, sym) && matchSignature(method, sym.declaredType, checkParamNames = false) =>
          DecodedMethod.ValOrDefDef(decodedClass, sym)
      }

  private def decodeStandardMethods(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    def rec(underlying: DecodedClass): Seq[DecodedMethod] =
      underlying match
        case anonFun: DecodedClass.SAMOrPartialFunction =>
          if method.isConstructor then Seq(DecodedMethod.SAMOrPartialFunctionConstructor(decodedClass, anonFun.tpe))
          else if anonFun.parentClass == Definitions.PartialFunctionClass then
            decodePartialFunctionImpl(decodedClass, anonFun.tpe, method).toSeq
          else decodeSAMFunctionImpl(decodedClass, anonFun.symbol, anonFun.parentClass, method).toSeq
        case underlying: DecodedClass.ClassDef => decodeInstanceMethods(decodedClass, underlying.symbol, method)
        case _: DecodedClass.SyntheticCompanionClass => Seq.empty
        case inlined: DecodedClass.InlinedClass => rec(inlined.underlying)
    rec(decodedClass)

  private def decodeParamForwarder(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod.ValOrDefDef] =
    decodedClass.declarations.collect {
      case sym: TermSymbol if names.contains(sym.targetNameStr) && matchSignature(method, sym.declaredType) =>
        DecodedMethod.ValOrDefDef(decodedClass, sym)
    }

  private def decodeTraitSetter(
      decodedClass: DecodedClass,
      method: binary.Method,
      name: String
  ): Seq[DecodedMethod.SetterAccessor] =
    for
      traitSym <- decodedClass.linearization.filter(_.isTrait)
      if method.decodedName.contains("$" + traitSym.nameStr + "$")
      sym <- traitSym.declarations.collect {
        case sym: TermSymbol if sym.targetNameStr == name && !sym.isMethod && !sym.isAbstractMember => sym
      }
      paramType <- decodedClass.thisType.map(sym.typeAsSeenFrom).collect { case tpe: Type => tpe }
    yield
      val tpe = MethodType(List(SimpleName("x$1")), List(paramType), defn.UnitType)
      DecodedMethod.SetterAccessor(decodedClass, sym, tpe)

  private def decodeSetter(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod.SetterAccessor] =
    for
      param <- method.parameters.lastOption.toSeq
      sym <- decodeFields(decodedClass, param.`type`, names)
    yield
      val tpe = MethodType(List(SimpleName("x$1")), List(sym.declaredType.asInstanceOf[Type]), defn.UnitType)
      DecodedMethod.SetterAccessor(decodedClass, sym, tpe)

  private def decodeFields(
      decodedClass: DecodedClass,
      binaryType: binary.Type,
      names: Seq[String]
  ): Seq[TermSymbol] =
    def matchType0(sym: TermSymbol): Boolean = matchSetterArgType(sym.declaredType, binaryType)
    decodedClass.declarations.collect {
      case sym: TermSymbol if !sym.isMethod && names.exists(sym.targetNameStr == _) && matchType0(sym) =>
        sym
    }

  private def decodeSuperAccessor(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod] =
    for
      traitSym <- decodedClass.linearization.filter(_.isTrait)
      if method.decodedName.contains("$" + traitSym.nameStr + "$")
      sym <- traitSym.declarations.collect {
        case sym: TermSymbol if names.contains(sym.targetNameStr) && !sym.isAbstractMember => sym
      }
      expectedTpe <- decodedClass.thisType.map(sym.typeAsSeenFrom(_))
      if matchSignature(method, expectedTpe)
    yield DecodedMethod.SuperAccessor(decodedClass, sym, expectedTpe)

  private def decodeSpecializedMethod(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod.SpecializedMethod] =
    decodedClass.declarations.collect {
      case sym: TermSymbol
          if names.contains(sym.targetNameStr) &&
            matchSignature(
              method,
              sym.declaredType,
              captureAllowed = false,
              checkParamNames = false,
              checkTypeErasure = false
            ) &&
            // hack: in Scala 3 only overriding symbols can be specialized (Function and Tuple)
            sym.allOverriddenSymbols.nonEmpty =>
        DecodedMethod.SpecializedMethod(decodedClass, sym)
    }

  private def decodeInlineAccessor(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod] =
    val classLoader = method.declaringClass.classLoader
    val methodAccessors = method.instructions
      .collect { case binary.Instruction.Method(_, owner, name, descriptor, _) =>
        classLoader.loadClass(owner).method(name, descriptor)
      }
      .singleOpt
      .flatten
      .map { binaryTarget =>
        val target = decode(binaryTarget)
        // val tpe = target.declaredType.asSeenFrom(fromType, fromClass)
        DecodedMethod.InlineAccessor(decodedClass, target)
      }
    def singleFieldInstruction(f: binary.Instruction.Field => Boolean) = method.instructions
      .collect { case instr: binary.Instruction.Field => instr }
      .singleOpt
      .filter(f)
      .toSeq
    def fieldSetters =
      val expectedNames = names.map(_.stripSuffix("_=")).distinct
      for
        instr <- singleFieldInstruction(f => f.isPut && f.unexpandedDecodedNames.exists(expectedNames.contains))
        binaryField <- classLoader.loadClass(instr.owner).declaredField(instr.name).toSeq
        fieldOwner = decode(binaryField.declaringClass)
        sym <- decodeFields(fieldOwner, binaryField.`type`, instr.unexpandedDecodedNames)
      yield
        val tpe = MethodType(List(SimpleName("x$1")), List(sym.declaredType.asInstanceOf[Type]), defn.UnitType)
        val decodedTarget = DecodedMethod.SetterAccessor(fieldOwner, sym, tpe)
        DecodedMethod.InlineAccessor(decodedClass, decodedTarget)
    def fieldGetters =
      for
        instr <- singleFieldInstruction(f => !f.isPut && f.unexpandedDecodedNames.exists(names.contains))
        binaryField <- classLoader.loadClass(instr.owner).declaredField(instr.name).toSeq
        fieldOwner = decode(binaryField.declaringClass)
        sym <- decodeFields(fieldOwner, binaryField.`type`, instr.unexpandedDecodedNames)
      yield DecodedMethod.InlineAccessor(decodedClass, DecodedMethod.ValOrDefDef(fieldOwner, sym))
    def moduleAccessors =
      for
        instr <- singleFieldInstruction(_.name == "MODULE$")
        targetClass = decode(classLoader.loadClass(instr.owner))
        targetClassSym <- targetClass.classSymbol
        targetTermSym <- targetClassSym.moduleValue
      yield DecodedMethod.InlineAccessor(decodedClass, DecodedMethod.ValOrDefDef(targetClass, targetTermSym))
    def valueClassAccessors =
      if method.instructions.isEmpty && method.isExtensionMethod then
        for
          companionClass <- decodedClass.companionClass.toSeq
          param <- method.parameters.lastOption.toSeq
          sym <- decodeFields(companionClass, param.`type`, names.map(_.stripSuffix("$extension")))
        yield
          val decodedTarget = DecodedMethod.ValOrDefDef(decodedClass, sym)
          DecodedMethod.InlineAccessor(decodedClass, decodedTarget)
      else Seq.empty
    methodAccessors.toSeq
      .orIfEmpty(fieldSetters)
      .orIfEmpty(fieldGetters)
      .orIfEmpty(moduleAccessors.toSeq)
      .orIfEmpty(valueClassAccessors)

  private def decodeInstanceMethods(
      decodedClass: DecodedClass,
      classSymbol: ClassSymbol,
      method: binary.Method
  ): Seq[DecodedMethod] =
    if method.isConstructor && classSymbol.isSubClass(defn.AnyValClass) then
      classSymbol.getAllOverloadedDecls(SimpleName("<init>")).map(DecodedMethod.ValOrDefDef(decodedClass, _))
    else
      val isJava = decodedClass.isJava
      val fromClass = classSymbol.declarations
        .collect { case sym: TermSymbol if matchTargetName(method, sym) => sym }
        .collect {
          case sym
              if matchSignature(
                method,
                sym.declaredType,
                asJavaVarargs = isJava,
                captureAllowed = !isJava,
                checkParamNames = !isJava
              ) =>
            DecodedMethod.ValOrDefDef(decodedClass, sym)
          case sym if !isJava && matchSignature(method, sym.declaredType, asJavaVarargs = true) =>
            DecodedMethod.Bridge(DecodedMethod.ValOrDefDef(decodedClass, sym), sym.declaredType)
        }
      fromClass.orIfEmpty(decodeAccessorsFromTraits(decodedClass, classSymbol, method))

  private def decodeAccessorsFromTraits(
      decodedClass: DecodedClass,
      classSymbol: ClassSymbol,
      method: binary.Method
  ): Seq[DecodedMethod] =
    if classSymbol.isTrait then Seq.empty
    else decodeAccessorsFromTraits(decodedClass, classSymbol, classSymbol.thisType, method)

  private def decodeAccessorsFromTraits(
      decodedClass: DecodedClass,
      fromClass: ClassSymbol,
      fromType: Type,
      method: binary.Method
  ): Seq[DecodedMethod] =
    for
      traitSym <- fromClass.linearization.filter(_.isTrait)
      if !method.isExpanded || method.decodedName.contains("$" + traitSym.nameStr + "$")
      sym <- traitSym.declarations
        .collect {
          case sym: TermSymbol if matchTargetName(method, sym) && matchSignature(method, sym.declaredType) => sym
        }
      if method.isExpanded == sym.isPrivate
      if sym.isParamAccessor || sym.isSetter || !sym.isMethod
      if sym.isOverridingSymbol(fromClass)
    yield
      val tpe = sym.typeAsSeenFrom(fromType)
      if sym.isParamAccessor then DecodedMethod.TraitParamAccessor(decodedClass, sym)
      else if sym.isSetter then DecodedMethod.SetterAccessor(decodedClass, sym, tpe)
      else DecodedMethod.GetterAccessor(decodedClass, sym, tpe)

  private def decodeLazyInit(decodedClass: DecodedClass, name: String): Seq[DecodedMethod] =
    val matcher: PartialFunction[Symbol, TermSymbol] =
      case sym: TermSymbol if sym.isModuleOrLazyVal && sym.nameStr == name => sym
    val fromClass = decodedClass.declarations.collect(matcher).map(DecodedMethod.LazyInit(decodedClass, _))
    def fromTraits =
      for
        traitSym <- decodedClass.linearization.filter(_.isTrait)
        term <- traitSym.declarations.collect(matcher)
        if term.isOverridingSymbol(decodedClass)
      yield DecodedMethod.LazyInit(decodedClass, term)
    fromClass.orIfEmpty(fromTraits)

  private def decodeTraitStaticForwarder(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Option[DecodedMethod.TraitStaticForwarder] =
    method.instructions
      .collect {
        case binary.Instruction.Method(_, owner, name, descriptor, _) if owner == method.declaringClass.name =>
          method.declaringClass.declaredMethod(name, descriptor)
      }
      .singleOpt
      .flatten
      .map(target => DecodedMethod.TraitStaticForwarder(decode(decodedClass, target)))

  private def decodeOuter(decodedClass: DecodedClass): Option[DecodedMethod.OuterAccessor] =
    decodedClass.symbolOpt
      .flatMap(_.outerClass)
      .map(outerClass => DecodedMethod.OuterAccessor(decodedClass, outerClass.thisType))

  private def decodeTraitInitializer(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod.ValOrDefDef] =
    decodedClass.declarations.collect {
      case sym: TermSymbol if sym.name == nme.Constructor => DecodedMethod.ValOrDefDef(decodedClass, sym)
    }

  private def decodeValueClassExtension(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod.ValOrDefDef] =
    val names = method.unexpandedDecodedNames.map(_.stripSuffix("$extension"))
    decodedClass.companionClassSymbol.toSeq.flatMap(_.declarations).collect {
      case sym: TermSymbol if names.contains(sym.targetNameStr) && matchSignature(method, sym.declaredType) =>
        DecodedMethod.ValOrDefDef(decodedClass, sym)
    }

  private def decodeStaticForwarder(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod.StaticForwarder] =
    decodedClass.companionClassSymbol.toSeq.flatMap(decodeStaticForwarder(decodedClass, _, method))

  private def decodeStaticForwarder(
      decodedClass: DecodedClass,
      companionObject: ClassSymbol,
      method: binary.Method
  ): Seq[DecodedMethod.StaticForwarder] =
    method.instructions
      .collect { case binary.Instruction.Method(_, owner, name, descriptor, _) =>
        method.declaringClass.classLoader.loadClass(owner).method(name, descriptor)
      }
      .flatten
      .singleOpt
      .toSeq
      .map(decode)
      .collect {
        case mixin: DecodedMethod.MixinForwarder => mixin.target
        case target => target
      }
      .map { target =>
        val declaredType = target.symbolOpt
          .map(_.typeAsSeenFrom(companionObject.thisType))
          .getOrElse(target.declaredType)
        DecodedMethod.StaticForwarder(decodedClass, target, declaredType)
      }

  private def decodeSAMFunctionImpl(
      decodedClass: DecodedClass,
      symbol: TermSymbol,
      parentClass: ClassSymbol,
      method: binary.Method
  ): Option[DecodedMethod] =
    val types =
      for
        parentCls <- parentClass.linearization.iterator
        overridden <- parentCls.declarations.collect { case term: TermSymbol if matchTargetName(method, term) => term }
        if overridden.overridingSymbol(parentClass).exists(_.isAbstractMember)
      yield DecodedMethod.SAMOrPartialFunctionImpl(decodedClass, overridden, symbol.declaredType)
    types.nextOption

  private def decodePartialFunctionImpl(
      decodedClass: DecodedClass,
      tpe: Type,
      method: binary.Method
  ): Option[DecodedMethod] =
    for sym <- Definitions.PartialFunctionClass.getNonOverloadedDecl(SimpleName(method.name)) yield
      val implTpe = sym.typeAsSeenFrom(SkolemType(tpe))
      DecodedMethod.SAMOrPartialFunctionImpl(decodedClass, sym, implTpe)

  private def decodeBridgesAndMixinForwarders(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Option[DecodedMethod] =
    def rec(underlying: DecodedClass): Option[DecodedMethod] =
      underlying match
        case underlying: DecodedClass.ClassDef =>
          if !underlying.symbol.isTrait then
            decodeBridgesAndMixinForwarders(decodedClass, underlying.symbol, underlying.symbol.thisType, method)
          else None
        case underlying: DecodedClass.SAMOrPartialFunction =>
          decodeBridgesAndMixinForwarders(decodedClass, underlying.parentClass, SkolemType(underlying.tpe), method)
        case underlying: DecodedClass.InlinedClass => rec(underlying.underlying)
        case _: DecodedClass.SyntheticCompanionClass => None
    rec(decodedClass)

  private def decodeBridgesAndMixinForwarders(
      decodedClass: DecodedClass,
      fromClass: ClassSymbol,
      fromType: Type,
      method: binary.Method
  ): Option[DecodedMethod] =
    decodeBridges(decodedClass, fromClass, fromType, method)
      .orIfEmpty(decodeMixinForwarder(decodedClass, method))

  private def decodeBridges(
      decodedClass: DecodedClass,
      fromClass: ClassSymbol,
      fromType: Type,
      method: binary.Method
  ): Option[DecodedMethod] =
    method.instructions
      .collect {
        case binary.Instruction.Method(_, owner, name, descriptor, _) if name == method.name =>
          method.declaringClass.classLoader.loadClass(owner).method(name, descriptor)
      }
      .singleOpt
      .flatten
      .map { binaryTarget =>
        val target = decode(binaryTarget)
        val tpe = target.declaredType.asSeenFrom(fromType, fromClass)
        DecodedMethod.Bridge(target, tpe)
      }

  private def decodeMixinForwarder(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Option[DecodedMethod.MixinForwarder] =
    method.instructions
      .collect { case binary.Instruction.Method(_, owner, name, descriptor, _) =>
        method.declaringClass.classLoader.loadClass(owner).declaredMethod(name, descriptor)
      }
      .singleOpt
      .flatten
      .filter(target => target.isStatic && target.declaringClass.isInterface)
      .map(decode)
      .collect { case staticForwarder: DecodedMethod.TraitStaticForwarder =>
        DecodedMethod.MixinForwarder(decodedClass, staticForwarder.target)
      }

  private def withCompanionIfExtendsAnyVal(decodedClass: DecodedClass): Seq[Symbol] = decodedClass match
    case classDef: DecodedClass.ClassDef =>
      Seq(classDef.symbol) ++ classDef.symbol.companionClass.filter(_.isSubClass(defn.AnyValClass))
    case _: DecodedClass.SyntheticCompanionClass => Seq.empty
    case anonFun: DecodedClass.SAMOrPartialFunction => Seq(anonFun.symbol)
    case inlined: DecodedClass.InlinedClass => withCompanionIfExtendsAnyVal(inlined.underlying)

  private def decodeAdaptedAnonFun(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    if method.instructions.nonEmpty then
      val underlying = method.instructions
        .collect {
          case binary.Instruction.Method(_, owner, name, descriptor, _) if owner == method.declaringClass.name =>
            method.declaringClass.declaredMethod(name, descriptor)
        }
        .flatten
        .singleOrElse(unexpected(s"$method is not an adapted method: cannot find underlying invocation"))
      decodeAnonFunsAndByNameArgs(decodedClass, underlying).map(DecodedMethod.AdaptedFun(_))
    else Seq.empty

  private def decodeAnonFunsAndReduceAmbiguity(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod] =
    val candidates = decodeAnonFunsAndByNameArgs(decodedClass, method)
    if candidates.size > 1 then
      val clashingMethods = method.declaringClass.declaredMethods
        .filter(m => m.returnType.zip(method.returnType).forall(_ == _) && m.name != method.name)
        .collect { case m @ Patterns.AnonFun() if m.name != method.name => m }
        .map(m => m -> decodeAnonFunsAndByNameArgs(decodedClass, m).toSet)
        .toMap
      def reduceAmbiguity(
          methods: Map[binary.Method, Set[DecodedMethod]]
      ): Map[binary.Method, Set[DecodedMethod]] =
        val found = methods.collect { case (m, syms) if syms.size == 1 => syms.head }
        val reduced = methods.map { case (m, candidates) =>
          if candidates.size > 1 then m -> (candidates -- found)
          else m -> candidates
        }
        if reduced.count { case (_, s) => s.size == 1 } == found.size then methods
        else reduceAmbiguity(reduced)
      reduceAmbiguity(clashingMethods + (method -> candidates.toSet))(method).toSeq
    else candidates

  private def decodeAnonFunsAndByNameArgs(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod] =
    val anonFuns = decodeLocalMethods(decodedClass, method, Seq(CommonNames.anonFun.toString))
    val byNameArgs =
      if method.parameters.forall(_.isCapturedParam) then decodeByNameArgs(decodedClass, method)
      else Seq.empty
    reduceAmbiguity(anonFuns ++ byNameArgs)

  private def decodeLocalMethods(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod] =
    collectLocalMethods(decodedClass, method) {
      case fun if names.contains(fun.symbol.name.toString) && matchLiftedFunSignature(method, fun) =>
        wrapIfInline(fun, DecodedMethod.ValOrDefDef(decodedClass, fun.symbol.asTerm))
    }

  private def decodeByNameArgs(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    collectLiftedTrees(decodedClass, method) { case arg: ByNameArg if !arg.isInline => arg }
      .collect {
        case arg if matchReturnType(arg.tpe, method.returnType) && matchCapture(arg.capture, method.parameters) =>
          wrapIfInline(arg, DecodedMethod.ByNameArg(decodedClass, arg.owner, arg.tree, arg.tpe.asInstanceOf))
      }

  private def decodeByNameArgsProxy(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    val explicitByNameArgs =
      collectLiftedTrees(decodedClass, method) { case arg: ByNameArg if arg.isInline => arg }
        .collect {
          case arg if matchReturnType(arg.tpe, method.returnType) && matchCapture(arg.capture, method.parameters) =>
            wrapIfInline(arg, DecodedMethod.ByNameArg(decodedClass, arg.owner, arg.tree, arg.tpe.asInstanceOf))
        }
    val inlineOverrides =
      for
        classSym <- decodedClass.classSymbol.toSeq
        sym <- classSym.declarations.collect {
          case sym: TermSymbol if sym.allOverriddenSymbols.nonEmpty && sym.isInline => sym
        }
        if method.sourceLines.forall(sym.pos.matchLines)
        paramSym <- sym.paramSymbols
        resultType <- Seq(paramSym.declaredType).collect { case tpe: ByNameType => tpe.resultType }
        if matchReturnType(resultType, method.returnType)
      yield
        val argTree = Ident(paramSym.name)(paramSym.localRef)(SourcePosition.NoPosition)
        DecodedMethod.ByNameArg(decodedClass, sym, argTree, resultType)
    explicitByNameArgs ++ inlineOverrides

  private def decodeSuperArgs(
      decodedClass: DecodedClass,
      method: binary.Method
  ): Seq[DecodedMethod.SuperConstructorArg] =
    def matchSuperArg(liftedArg: LiftedTree[Nothing]): Boolean =
      val primaryConstructor = liftedArg.owner.asClass.getAllOverloadedDecls(nme.Constructor).head
      // a super arg takes the same parameters as its constructor
      val sourceParams = extractSourceParams(method, primaryConstructor.declaredType)
      val binaryParams = splitBinaryParams(method, sourceParams)
      matchReturnType(liftedArg.tpe, method.returnType) && matchCapture(liftedArg.capture, binaryParams.capturedParams)
    collectLiftedTrees(decodedClass, method) { case arg: ConstructorArg => arg }
      .collect {
        case liftedArg if matchSuperArg(liftedArg) =>
          DecodedMethod.SuperConstructorArg(
            decodedClass,
            liftedArg.owner.asClass,
            liftedArg.tree,
            liftedArg.tpe.asInstanceOf
          )
      }

  private def decodeLiftedTries(decodedClass: DecodedClass, method: binary.Method): Seq[DecodedMethod] =
    collectLiftedTrees(decodedClass, method) { case tree: LiftedTry => tree }
      .collect {
        case liftedTry if matchReturnType(liftedTry.tpe, method.returnType) =>
          wrapIfInline(
            liftedTry,
            DecodedMethod.LiftedTry(decodedClass, liftedTry.owner, liftedTry.tree, liftedTry.tpe.asInstanceOf)
          )
      }

  private def decodeLocalLazyInit(
      decodedClass: DecodedClass,
      method: binary.Method,
      names: Seq[String]
  ): Seq[DecodedMethod] =
    collectLocalMethods(decodedClass, method) {
      case term if term.symbol.isModuleOrLazyVal && names.contains(term.symbol.nameStr) =>
        wrapIfInline(term, DecodedMethod.LazyInit(decodedClass, term.symbol))
    }

  private def matchTargetName(method: binary.Method, symbol: TermSymbol): Boolean =
    method.unexpandedDecodedNames.map(_.stripSuffix("$")).contains(symbol.targetNameStr)

  private def matchSignature(
      method: binary.Method,
      declaredType: TypeOrMethodic,
      captureAllowed: Boolean = true,
      asJavaVarargs: Boolean = false,
      checkParamNames: Boolean = true,
      checkTypeErasure: Boolean = true
  ): Boolean =
    val sourceParams = extractSourceParams(method, declaredType)
    val binaryParams = splitBinaryParams(method, sourceParams)

    def matchParamNames: Boolean =
      sourceParams.declaredParamNames
        .corresponds(binaryParams.declaredParams)((name, javaParam) => name.toString == javaParam.name)

    def matchTypeErasure: Boolean =
      sourceParams.regularParamTypes
        .corresponds(binaryParams.regularParams)((tpe, javaParam) =>
          matchArgType(tpe, javaParam.`type`, asJavaVarargs)
        ) && matchReturnType(sourceParams.returnType, binaryParams.returnType)

    (captureAllowed || binaryParams.capturedParams.isEmpty) &&
    binaryParams.capturedParams.forall(_.isGeneratedParam) &&
    binaryParams.expandedParams.forall(_.isGeneratedParam) &&
    sourceParams.regularParamTypes.size == binaryParams.regularParams.size &&
    (!checkParamNames || matchParamNames) &&
    (!checkTypeErasure || matchTypeErasure)
  end matchSignature

  private def matchSetterArgType(scalaVarType: TypeOrMethodic, javaSetterParamType: binary.Type): Boolean =
    scalaVarType match
      case scalaVarType: Type =>
        scalaVarType.erasedAsArgType(asJavaVarargs = false).exists(matchType(_, javaSetterParamType))
      case _: MethodicType => false

  private def collectLocalMethods(
      decodedClass: DecodedClass,
      method: binary.Method
  )(
      matcher: PartialFunction[LiftedTree[TermSymbol], DecodedMethod]
  ): Seq[DecodedMethod] =
    collectLiftedTrees(decodedClass, method) { case term: LocalTermDef => term }
      .collect(matcher)

  private def collectLiftedTrees[S](decodedClass: DecodedClass, method: binary.Method)(
      matcher: PartialFunction[LiftedTree[?], LiftedTree[S]]
  ): Seq[LiftedTree[S]] =
    val owners = withCompanionIfExtendsAnyVal(decodedClass)
    val sourceLines =
      if owners.size == 2 && method.parameters.exists(p => p.name.matches("\\$this\\$\\d+")) then
        // workaround of https://github.com/lampepfl/dotty/issues/18816
        method.sourceLines.map(_.last)
      else method.sourceLines
    owners.flatMap(collectLiftedTrees(_, sourceLines)(matcher))

  private def reduceAmbiguity(syms: Seq[DecodedMethod]): Seq[DecodedMethod] =
    if syms.size > 1 then
      val reduced = syms.filterNot(sym => syms.exists(enclose(sym, _)))
      if reduced.size != 0 then reduced else syms
    else syms

  private def wrapIfInline(liftedTree: LiftedTree[?], decodedMethod: DecodedMethod): DecodedMethod =
    liftedTree match
      case InlinedFromDef(liftedTree, inlineCall) =>
        DecodedMethod.InlinedMethod(wrapIfInline(liftedTree, decodedMethod), inlineCall.callTree)
      case _ => decodedMethod

  private def matchLiftedFunSignature(method: binary.Method, tree: LiftedTree[TermSymbol]): Boolean =
    val sourceParams = extractSourceParams(method, tree.tpe)
    val binaryParams = splitBinaryParams(method, sourceParams)

    def matchParamNames: Boolean =
      sourceParams.declaredParamNames
        .corresponds(binaryParams.declaredParams)((name, javaParam) => name.toString == javaParam.name)

    def matchTypeErasure: Boolean =
      sourceParams.regularParamTypes
        .corresponds(binaryParams.regularParams)((tpe, javaParam) => matchArgType(tpe, javaParam.`type`, false)) &&
        matchReturnType(sourceParams.returnType, binaryParams.returnType)

    matchParamNames && matchTypeErasure && matchCapture(tree.capture, binaryParams.capturedParams)
  end matchLiftedFunSignature

  private def matchReturnType(scalaType: TermType, javaType: Option[binary.Type]): Boolean =
    scalaType match
      case scalaType: Type => javaType.forall(jt => scalaType.erasedAsReturnType.exists(matchType(_, jt)))
      case _: MethodicType | _: PackageRef => false

  private def extractSourceParams(method: binary.Method, tpe: TermType): SourceParams =
    val (expandedParamTypes, returnType) =
      if method.isConstructor && method.declaringClass.isJavaLangEnum then
        (List(defn.StringType, defn.IntType), tpe.returnType)
      else if !method.isAnonFun then tpe.returnType.expandContextFunctions
      else (List.empty, tpe.returnType)
    SourceParams(tpe.allParamNames, tpe.allParamTypes, expandedParamTypes, returnType)

  /* After code generation, a method ends up with more than its declared parameters.
   *
   * It has, in order:
   * - captured params,
   * - declared params,
   * - "expanded" params (from java.lang.Enum constructors and uncurried context function types).
   *
   * We can only check the names of declared params.
   * We can check the (erased) type of declared and expanded params; together we call them "regular" params.
   */
  private def splitBinaryParams(method: binary.Method, sourceParams: SourceParams): BinaryParams =
    val (capturedParams, regularParams) =
      method.parameters.splitAt(method.parameters.size - sourceParams.regularParamTypes.size)
    val (declaredParams, expandedParams) = regularParams.splitAt(sourceParams.declaredParamTypes.size)
    BinaryParams(capturedParams, declaredParams, expandedParams, method.returnType)

  private def enclose(enclosing: DecodedMethod, enclosed: DecodedMethod): Boolean =
    (enclosing, enclosed) match
      case (enclosing: DecodedMethod.InlinedMethod, enclosed: DecodedMethod.InlinedMethod) =>
        enclosing.callPos.enclose(enclosed.callPos) || (
          !enclosed.callPos.enclose(enclosing.callPos) &&
            enclose(enclosing.underlying, enclosed.underlying)
        )
      case (enclosing: DecodedMethod.InlinedMethod, enclosed) =>
        enclosing.callPos.enclose(enclosed.pos)
      case (enclosing, enclosed: DecodedMethod.InlinedMethod) =>
        enclosing.pos.enclose(enclosed.callPos)
      case (enclosing, enclosed) =>
        enclosing.pos.enclose(enclosed.pos)

  private case class SourceParams(
      declaredParamNames: Seq[UnsignedTermName],
      declaredParamTypes: Seq[Type],
      expandedParamTypes: Seq[Type],
      returnType: Type
  ):
    def regularParamTypes: Seq[Type] = declaredParamTypes ++ expandedParamTypes

  private case class BinaryParams(
      capturedParams: Seq[binary.Parameter],
      declaredParams: Seq[binary.Parameter],
      expandedParams: Seq[binary.Parameter],
      returnType: Option[binary.Type]
  ):
    def regularParams = declaredParams ++ expandedParams

  private def matchCapture(capture: Seq[String], capturedParams: Seq[binary.Parameter]): Boolean =
    val anonymousPattern = "\\$\\d+".r
    val evidencePattern = "evidence\\$\\d+".r
    def toPattern(variable: String): Regex =
      variable match
        case anonymousPattern() => "\\$\\d+\\$\\$\\d+".r
        case evidencePattern() => "evidence\\$\\d+\\$\\d+".r
        case _ =>
          val encoded = NameTransformer.encode(variable)
          s"${Regex.quote(encoded)}(\\$$tailLocal\\d+)?(\\$$lzy\\d+)?\\$$\\d+".r
    val patterns = capture.map(toPattern)
    def isCapture(param: String) =
      patterns.exists(_.unapplySeq(param).nonEmpty)
    def isProxy(param: String) = "(.+)\\$proxy\\d+\\$\\d+".r.unapplySeq(param).nonEmpty
    def isThisOrOuter(param: String) = "(.+_|\\$)(this|outer)\\$\\d+".r.unapplySeq(param).nonEmpty
    def isLazy(param: String) = "(.+)\\$lzy\\d+\\$\\d+".r.unapplySeq(param).nonEmpty
    capturedParams.forall(p => isProxy(p.name) || isCapture(p.name) || isThisOrOuter(p.name) || isLazy(p.name))

  protected def matchArgType(scalaType: Type, javaType: binary.Type, asJavaVarargs: Boolean): Boolean =
    scalaType.erasedAsArgType(asJavaVarargs).exists(matchType(_, javaType))

  private lazy val scalaPrimitivesToJava: Map[ClassSymbol, String] = Map(
    defn.BooleanClass -> "boolean",
    defn.ByteClass -> "byte",
    defn.CharClass -> "char",
    defn.DoubleClass -> "double",
    defn.FloatClass -> "float",
    defn.IntClass -> "int",
    defn.LongClass -> "long",
    defn.ShortClass -> "short",
    defn.UnitClass -> "void",
    defn.NullClass -> "scala.runtime.Null$"
  )

  private def matchType(
      scalaType: ErasedTypeRef,
      javaType: binary.Type
  ): Boolean =
    def rec(scalaType: ErasedTypeRef, javaType: String): Boolean =
      scalaType match
        case ErasedTypeRef.ArrayTypeRef(base, dimensions) =>
          javaType.endsWith("[]" * dimensions) &&
          rec(base, javaType.dropRight(2 * dimensions))
        case ErasedTypeRef.ClassRef(scalaClass) =>
          scalaPrimitivesToJava.get(scalaClass) match
            case Some(javaPrimitive) => javaPrimitive == javaType
            case None => matchClassType(scalaClass, javaType, nested = false)
    rec(scalaType, javaType.name)

  private lazy val dollarDigitsMaybeDollarAtEndRegex = "\\$\\d+\\$?$".r

  private def matchClassType(scalaClass: ClassSymbol, javaType: String, nested: Boolean): Boolean =
    def encodedName(nested: Boolean): String = scalaClass.name match
      case ObjectClassTypeName(underlying) if nested => NameTransformer.encode(underlying.toString())
      case name => NameTransformer.encode(name.toString())
    scalaClass.owner match
      case owner: PackageSymbol =>
        javaType == owner.fullName.toString() + "." + encodedName(nested)
      case owner: ClassSymbol =>
        val encodedName1 = encodedName(nested)
        javaType.endsWith("$" + encodedName1) &&
        matchClassType(owner, javaType.dropRight(1 + encodedName1.length()), nested = true)
      case owner: TermOrTypeSymbol =>
        dollarDigitsMaybeDollarAtEndRegex.findFirstIn(javaType).exists { suffix =>
          val prefix = javaType.stripSuffix(suffix)
          val encodedName1 = encodedName(nested = true)
          prefix.endsWith("$" + encodedName1) && {
            val ownerJavaType = prefix.dropRight(1 + encodedName1.length())
            enclosingClassOwners(owner).exists(matchClassType(_, ownerJavaType, nested = true))
          }
        }

  private def enclosingClassOwners(sym: TermOrTypeSymbol): List[ClassSymbol] =
    sym.owner match
      case owner: ClassSymbol => owner :: enclosingClassOwners(owner)
      case owner: TermOrTypeSymbol => enclosingClassOwners(owner)
      case owner: PackageSymbol => Nil
