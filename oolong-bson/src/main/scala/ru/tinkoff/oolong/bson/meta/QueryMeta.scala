package ru.tinkoff.oolong.bson.meta

import java.util.regex.Pattern
import scala.quoted.*

import ru.tinkoff.oolong.bson.utils.*

case class QueryMeta[T](map: Map[String, String])

extension [T](inline q: QueryMeta[T])
  inline def withRenaming(inline fields: (T => (Any, String))*): QueryMeta[T] = ${
    withRenamingImpl('q, 'fields)
  }

end extension

object QueryMeta:
  given [T: Type]: FromExpr[QueryMeta[T]] = new FromExpr[QueryMeta[T]]:
    def unapply(expr: Expr[QueryMeta[T]])(using q: Quotes): Option[QueryMeta[T]] =
      import q.reflect.*
      expr match
        case '{ QueryMeta(${ map }: Map[String, String]) } =>
          map.value.map(QueryMeta[T].apply)
        case _ =>
          None

  // Adapted from enumeratum: https://github.com/lloydmeta/enumeratum

  private val regexp1: Pattern    = Pattern.compile("([A-Z]+)([A-Z][a-z])")
  private val regexp2: Pattern    = Pattern.compile("([a-z\\d])([A-Z])")
  private val replacement: String = "$1_$2"

  private def field2WordArray(name: String): Array[String] =
    val first = regexp1.matcher(name).replaceAll(replacement)
    regexp2.matcher(first).replaceAll(replacement).split("_")
  end field2WordArray

  private def capitalize(word: String) =
    word.take(1).toUpperCase + word.tail

  private def uncapitalize(word: String) =
    word.take(1).toLowerCase + word.tail

  inline def identity[T]: QueryMeta[T] = ${ identityImpl[T] }
  private def identityImpl[T: Type](using q: Quotes): Expr[QueryMeta[T]] =
    renamingMacro[T](scala.Predef.identity)

  inline def upperCamelCase[T]: QueryMeta[T] = ${ upperCamelCaseImpl[T] }
  private def upperCamelCaseImpl[T: Type](using q: Quotes): Expr[QueryMeta[T]] =
    renamingMacro[T](_.map(name => field2WordArray(name).map(capitalize).mkString))

  inline def camelCase[T]: QueryMeta[T] = ${ camelCaseImpl[T] }
  private def camelCaseImpl[T: Type](using q: Quotes): Expr[QueryMeta[T]] =
    renamingMacro[T](
      _.map(name => field2WordArray(name).map(capitalize).mkString)
        .map(uncapitalize)
    )

  inline def snakeCase[T]: QueryMeta[T] = ${ snakeCaseImpl[T] }
  private def snakeCaseImpl[T: Type](using q: Quotes): Expr[QueryMeta[T]] =
    renamingMacro[T](_.map(name => field2WordArray(name).map(_.toLowerCase).mkString("_")))

  private def renamingMacro[T: Type](fun: List[String] => List[String])(using q: Quotes): Expr[QueryMeta[T]] =
    import q.reflect.*
    val typeRepr        = TypeRepr.of[T]
    val caseFieldsNames = typeRepr.typeSymbol.caseFields.map(_.name)
    val caseFieldsTypes = typeRepr.typeSymbol.caseFields.map(s => typeRepr.memberType(s).asType)
    val renames         = fun(caseFieldsNames)
    val caseSnakeMeta   = caseFieldsNames.zip(renames).toMap[String, String]
    val childrenMeta    = extractFieldsMap(caseFieldsNames, caseFieldsTypes)
    val resultMap       = merge(caseSnakeMeta, childrenMeta).filter { case (key, value) => key != value }
    '{ QueryMeta.apply[T](${ Expr(resultMap) }) }

end QueryMeta

inline def queryMeta[T](inline fields: (T => (Any, String))*): QueryMeta[T] = ${ queryMetaImpl[T]('fields) }

private def queryMetaImpl[T: Type](expr: Expr[Seq[(T => (Any, String))]])(using q: Quotes): Expr[QueryMeta[T]] = {
  import q.reflect.*

  val typeRepr        = TypeRepr.of[T]
  val caseFieldsNames = typeRepr.typeSymbol.caseFields.map(_.name)
  val caseFieldsTypes = typeRepr.typeSymbol.caseFields.map(s => typeRepr.memberType(s).asType)

  val childrenMap = extractFieldsMap(caseFieldsNames, caseFieldsTypes)

  val exprList: List[Expr[(T => (Any, String))]] = expr match
    case AsIterable(list) => list.toList
    case _                => report.errorAndAbort("Wrong input: " + expr.asTerm.show(using Printer.TreeCode))

  val res = exprList
    .map { expr =>
      expr.asTerm.underlyingArgument.asExpr match
        case TupleLambda(rename) => rename
        case _ => report.errorAndAbort("Incorrect lambda: " + expr.asTerm.show(using Printer.TreeCode))
    }
    .toMap[String, String]

  val identityNames = caseFieldsNames.zip(caseFieldsNames).toMap

  val resultMap = merge(identityNames ++ res, childrenMap).filter { case (key, value) => key != value }

  '{ QueryMeta.apply[T](${ Expr(resultMap) }) }

}

private def withRenamingImpl[T: Type](meta: Expr[QueryMeta[T]], exprs: Expr[Seq[(T => (Any, String))]])(using
    q: Quotes
): Expr[QueryMeta[T]] =
  import q.reflect.*
  val oldMeta =
    meta.value.getOrElse(
      report.errorAndAbort(s"Please, add `inline` to given QueryMeta[${TypeRepr.of[T].typeSymbol.name}]")
    )

  val newMeta = queryMetaImpl[T](exprs).valueOrAbort

  val resultMap = oldMeta.map ++ newMeta.map

  '{ QueryMeta.apply[T](${ Expr(resultMap) }) }
end withRenamingImpl

def extractFieldsMap(fieldsNames: List[String], fieldsTypes: List[Type[?]])(using
    q: Quotes
): Map[String, Map[String, String]] =
  import q.reflect.*
  fieldsNames
    .zip(fieldsTypes)
    .map { case (name, s) =>
      s match
        case '[Option[t]] =>
          Expr.summon[QueryMeta[t]] match
            case Some(expr0) =>
              Some(
                name -> expr0.value
                  .getOrElse(
                    report.errorAndAbort(s"Please, add `inline` to given QueryMeta[${TypeRepr.of[t].typeSymbol.name}]")
                  )
                  .map
              )
            case _
                if TypeRepr.of[t].typeSymbol.flags.is(Flags.Case) &&
                  TypeRepr.of[t].typeSymbol.caseFields.nonEmpty =>
              Some(name -> summonChildMeta[t])
            case _ => None
        case '[t] =>
          Expr.summon[QueryMeta[t]] match
            case Some(expr0) =>
              Some(
                name -> expr0.value
                  .getOrElse(
                    report.errorAndAbort(s"Please, add `inline` to given QueryMeta[${TypeRepr.of[t].typeSymbol.name}]")
                  )
                  .map
              )
            case _
                if TypeRepr.of[t].typeSymbol.flags.is(Flags.Case) &&
                  TypeRepr.of[t].typeSymbol.caseFields.nonEmpty =>
              Some(name -> summonChildMeta[t])
            case _ => None
    }
    .flatten
    .toMap
end extractFieldsMap

def summonChildMeta[T: Type](using q: Quotes): Map[String, String] =
  import q.reflect.*
  val typeRepr                       = TypeRepr.of[T]
  val typeSymbol                     = typeRepr.typeSymbol
  val caseFieldsNames                = typeSymbol.caseFields.map(_.name)
  val identityMeta                   = caseFieldsNames.zip(caseFieldsNames).toMap[String, String]
  val caseFieldsTypes: List[Type[?]] = typeSymbol.caseFields.map(s => typeRepr.memberType(s).asType)
  val childrenMeta                   = extractFieldsMap(caseFieldsNames, caseFieldsTypes)
  merge(identityMeta, childrenMeta)

end summonChildMeta

def merge(first: Map[String, String], children: Map[String, Map[String, String]]) =
  first.flatMap { case (first, second) =>
    children.getOrElse(first, Map.empty).map { case (firstA, secondA) =>
      (s"$first.$firstA" -> s"$second.$secondA")
    } + (first -> second)
  }
