package ru.tinkoff.oolong

import scala.quoted.Expr

import ru.tinkoff.oolong.QExpr

sealed private[oolong] trait QExpr

private[oolong] object QExpr {

  case class Gte(x: QExpr, y: QExpr) extends QExpr

  case class Lte(x: QExpr, y: QExpr) extends QExpr

  case class Gt(x: QExpr, y: QExpr) extends QExpr

  case class Lt(x: QExpr, y: QExpr) extends QExpr

  case class Eq(x: QExpr, y: QExpr) extends QExpr

  case class Ne(x: QExpr, y: QExpr) extends QExpr

  case class Not(x: QExpr) extends QExpr

  case class In(x: QExpr, y: List[QExpr] | QExpr) extends QExpr
  case class Nin(x: QExpr, y: List[QExpr] | QExpr) extends QExpr

  case class And(children: List[QExpr]) extends QExpr

  case class Or(children: List[QExpr]) extends QExpr

  case class Prop(path: List[String]) extends QExpr

  case class Constant[T](s: T) extends QExpr

  case class ScalaCode(code: Expr[Any]) extends QExpr

  case class ScalaCodeIterable(code: Expr[Iterable[Any]]) extends QExpr

  case class Subquery(code: Expr[Any]) extends QExpr

  case class Exists(x: QExpr, y: QExpr) extends QExpr

  case class Size(x: QExpr, y: QExpr) extends QExpr
}
