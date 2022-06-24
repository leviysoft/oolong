package ru.tinkoff.oolong

import scala.quoted.*

import ru.tinkoff.oolong.Utils.AnonfunBlock
import ru.tinkoff.oolong.Utils.PropSelector

object PropSelectorTestMacro {
  inline def prop[A](inline expr: A => Any): Option[(String, List[String])] =
    ${ propImpl('expr) }

  def propImpl[A](expr: Expr[A => Any])(using quotes: Quotes): Expr[Option[(String, List[String])]] = {
    import quotes.reflect.*

    def preprocess(term: Term): Term = term match {
      case Inlined(_, _, next)   => preprocess(next)
      case AnonfunBlock(_, body) => preprocess(body)
      case _                     => term
    }

    val inlined = preprocess(expr.asTerm).asExpr

    Expr(PropSelector.unapply(inlined))
  }

}
