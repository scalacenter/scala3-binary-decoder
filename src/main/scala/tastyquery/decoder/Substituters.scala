package tastyquery.decoder

import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Types.*
import tastyquery.decoder.TypeMaps.*

object Substituters:
  def substLocalTypeParams(tp: TypeMappable, from: List[LocalTypeParamSymbol], to: List[TypeOrWildcard])(using
      Context
  ): tp.ThisTypeMappableType =
    new SubstLocalTypeParamsMap(from, to).apply(tp)

  private final class SubstLocalTypeParamsMap(from: List[LocalTypeParamSymbol], to: List[TypeOrWildcard])(using Context)
      extends NormalizingTypeMap:
    protected def transform(tp: TypeMappable): TypeMappable =
      tp match
        case tp: NamedType =>
          tp.prefix match
            case NoPrefix =>
              var fs = from
              var ts = to
              while fs.nonEmpty && ts.nonEmpty do
                if tp.isLocalRef(fs.head) then return ts.head
                fs = fs.tail
                ts = ts.tail
              tp
            case prefix: Type =>
              tp.normalizedDerivedSelect(apply(prefix))
            case _: PackageRef =>
              tp
        case _: ThisType | _: BoundType =>
          tp
        case _ =>
          mapOver(tp)
    end transform
