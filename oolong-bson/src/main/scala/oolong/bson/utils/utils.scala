package oolong.bson.utils

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

object AsIterable {
  def unapply[T: Type](using q: Quotes)(
      expr: Expr[Iterable[T]]
  ): Option[Iterable[Expr[T]]] = {
    import q.reflect.*
    def rec(tree: Term): Option[Iterable[Expr[T]]] = tree match {
      case Repeated(elems, _) => Some(elems.map(x => x.asExprOf[T]))
      case Typed(e, _)        => rec(e)
      case Block(Nil, e)      => rec(e)
      case Apply(_, List(e))  => rec(e)
      case Inlined(_, Nil, e) => rec(e)
      case _                  => None
    }
    rec(expr.asTerm)
  }
}

object AsTerm {
  def unapply(using quotes: Quotes)(
      expr: Expr[Any]
  ): Option[quotes.reflect.Term] = {
    import quotes.reflect.*
    Some(expr.asTerm)
  }
}

object TupleLambda {
  def unapply(using quotes: Quotes)(expr: Expr[Any]): Option[(String, String)] =
    import quotes.reflect.*
    expr match
      case AsTerm(
            Block(
              List(
                DefDef(
                  "$anonfun",
                  _,
                  _,
                  Some(
                    Apply(
                      TypeApply(Select(Apply(_, List(Select(_, first))), "->"), _),
                      List(Literal(StringConstant(second)))
                    )
                  )
                )
              ),
              _
            )
          ) =>
        Some(first -> second)
      case _ => None
}

object Projection:
  def checkIfProjection[Doc: Type, Proj: Type](using
      quotes: Quotes
  ): Boolean =
    import quotes.reflect.*
    val baseTypeRepr       = TypeRepr.of[Doc]
    val projectionTypeRepr = TypeRepr.of[Proj]
    val fieldsAndTypesBase: Map[String, TypeRepr] = baseTypeRepr.typeSymbol.caseFields
      .map(field => field.name -> baseTypeRepr.memberType(field))
      .toMap

    val fieldsAndTypesProjection: Map[String, TypeRepr] = projectionTypeRepr.typeSymbol.caseFields
      .map(field => field.name -> projectionTypeRepr.memberType(field))
      .toMap

    fieldsAndTypesProjection.forall { case (field, fieldType) =>
      fieldsAndTypesBase.get(field).exists { baseFieldType =>
        if baseFieldType.typeSymbol.flags.is(Flags.Case) &&
          baseFieldType.typeSymbol.caseFields.nonEmpty
        then
          (baseFieldType.asType, fieldType.asType) match
            case ('[base], '[proj]) =>
              checkIfProjection[base, proj]
        else baseFieldType =:= fieldType
      }
    }

  def projectionPaths[Doc: Type, Proj: Type](using quotes: Quotes): Vector[String] =
    def projectionPathsInner[Doc1: Type, Proj1: Type](using quotes: Quotes): Vector[Vector[String]] =
      import quotes.reflect.*
      val baseTypeRepr       = TypeRepr.of[Doc1]
      val projectionTypeRepr = TypeRepr.of[Proj1]
      val fieldsAndTypesBase: Map[String, TypeRepr] = baseTypeRepr.typeSymbol.caseFields
        .map(field => field.name -> baseTypeRepr.memberType(field))
        .toMap

      val fieldsAndTypesProjection: Map[String, TypeRepr] = projectionTypeRepr.typeSymbol.caseFields
        .map(field => field.name -> projectionTypeRepr.memberType(field))
        .toMap

      val paths = fieldsAndTypesBase.toVector.flatMap { case (field, baseType) =>
        fieldsAndTypesProjection.get(field).map { projFieldType =>
          if baseType =:= projFieldType then Vector(Vector(field))
          else
            (baseType.asType, projFieldType.asType) match
              case ('[base], '[proj]) =>
                val innerPaths = projectionPathsInner[base, proj].map(_.prepended(field))
                if innerPaths.isEmpty then Vector(Vector(field))
                else innerPaths
        }
      }.flatten
      if paths.size == fieldsAndTypesBase.size && paths.forall(_.size == 1) then Vector.empty
      else paths

    projectionPathsInner[Doc, Proj].map(_.mkString("."))

object QueryPath:
  def allPaths[T: Type](using q: Quotes): Vector[String] =
    import q.reflect.*
    val typeRepr                          = TypeRepr.of[T]
    val caseFieldsNames: Vector[String]   = typeRepr.typeSymbol.caseFields.map(_.name).toVector
    val caseFieldsTypes: Vector[TypeRepr] = typeRepr.typeSymbol.caseFields.map(s => typeRepr.memberType(s)).toVector
    caseFieldsNames
      .lazyZip(caseFieldsTypes)
      .flatMap { case (name, typ) =>
        if (typ.typeSymbol.flags.is(Flags.Case) && typ.typeSymbol.caseFields.nonEmpty)
          typ.asType match
            case '[fieldType] =>
              Vector(name, (name +: allPaths[fieldType]).mkString("."))
        else Vector(name)
      }
