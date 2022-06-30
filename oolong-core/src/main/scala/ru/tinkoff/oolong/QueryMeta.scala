package ru.tinkoff.oolong

import scala.quoted.*

import ru.tinkoff.oolong.Utils.*

case class QueryMeta[T](map: Map[String, String])

inline def queryMeta[T](inline fields: (T => (Any, String))*): QueryMeta[T] = ${ queryMetaImpl[T]('fields) }

def queryMetaImpl[T: Type](expr: Expr[Seq[(T => (Any, String))]])(using q: Quotes): Expr[QueryMeta[T]] = {
  import q.reflect.*

  val typeRepr        = TypeRepr.of[T]
  val caseFieldsNames = typeRepr.typeSymbol.caseFields.map(_.name)
  val caseFieldsTypes = typeRepr.typeSymbol.caseFields.map(s => typeRepr.memberType(s).asType)

  val childrenMap: Map[String, Map[String, String]] = caseFieldsNames
    .zip(caseFieldsTypes)
    .map { case (name, s) =>
      s match
        case '[Option[t]] =>
          Expr.summon[QueryMeta[t]] match
            case Some('{ ${ expr0 }: QueryMeta[t] }) =>
              expr0 match
                case AsQueryMeta(map) => Some(map).map(name -> _)
                case _                => None
            case _ => None
        case '[t] =>
          Expr.summon[QueryMeta[t]] match
            case Some('{ ${ expr0 }: QueryMeta[t] }) =>
              expr0 match
                case AsQueryMeta(map) => Some(map).map(name -> _)
                case _                => None
            case _ => None
    }
    .flatten
    .toMap

  val exprList: List[Expr[(T => (Any, String))]] = expr match
    case AsIterable(list) => list.toList
    case _                => report.errorAndAbort("Wrong lambda")

  val res = exprList
    .map { expr =>
      expr.asTerm.underlyingArgument.asExpr match
        case AsTerm(
              Block(
                List(
                  DefDef(
                    "$anonfun",
                    _,
                    _,
                    Some(
                      Apply(
                        TypeApply(Select(Apply(_, List(Select(_, old))), "->"), _),
                        List(Literal(StringConstant(newName)))
                      )
                    )
                  )
                ),
                _
              )
            ) =>
          old -> newName
    }
    .toMap[String, String]

  val identityNames = caseFieldsNames.zip(caseFieldsNames).toMap

  val resultMap = merge(identityNames ++ res, childrenMap).filter { case (key, value) => key != value }

  '{ QueryMeta.apply[T](${ Expr(resultMap) }) }

}

def merge(first: Map[String, String], children: Map[String, Map[String, String]]) =
  first.flatMap { case (first, second) =>
    children.find { case (k, _) => k == first }.map(_._2).getOrElse(Map.empty).map { case (firstA, secondA) =>
      (s"$first.$firstA" -> s"$second.$secondA")
    } + (first -> second)
  }

object AsQueryMeta:
  def unapply[T](expr: Expr[QueryMeta[T]])(using q: Quotes): Option[Map[String, String]] =
    import q.reflect.*
    def rec(tree: Term): Option[Map[String, String]] =
      tree match
        case Typed(elem, _)      => rec(elem)
        case Inlined(_, _, elem) => rec(elem)
        case Apply(_, List(Inlined(_, _, Inlined(_, _, Apply(_, List(Typed(Inlined(_, _, repeated), _))))))) =>
          rec(repeated)
        case Repeated(list, _) =>
          list.map(rec).foldLeft(Some(Map.empty[String, String]): Option[Map[String, String]]) { case (a, b) =>
            a.flatMap(s => b.map(s ++ _))
          }
        case Apply(
              TypeApply(Select(Ident("Tuple2"), "apply"), _),
              List(
                Inlined(None, Nil, Literal(StringConstant(key))),
                Inlined(None, Nil, Literal(StringConstant(value)))
              )
            ) =>
          Some(Map(key -> value))
        case _ => None

    rec(expr.asTerm)

end AsQueryMeta
