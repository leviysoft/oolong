package ru.tinkoff.oolong.bson.utils

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
