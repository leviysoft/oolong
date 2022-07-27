package ru.tinkoff.oolong

import scala.quoted.Expr

import ru.tinkoff.oolong.UExpr

sealed private[oolong] trait UExpr

private[oolong] object UExpr {

  case class Update(ops: List[FieldUpdateExpr]) extends UExpr

  case class Prop(path: String) extends UExpr

  case class Constant[T](t: T) extends UExpr

  case class ScalaCode(code: Expr[Any]) extends UExpr

  sealed abstract class FieldUpdateExpr(prop: Prop)

  object FieldUpdateExpr {

    case class Set(prop: Prop, expr: UExpr) extends FieldUpdateExpr(prop: Prop)

    case class Inc(prop: Prop, expr: UExpr) extends FieldUpdateExpr(prop)

    case class Unset(prop: Prop) extends FieldUpdateExpr(prop)

    case class Min(prop: Prop, expr: UExpr) extends FieldUpdateExpr(prop)

    case class Max(prop: Prop, expr: UExpr) extends FieldUpdateExpr(prop)

    case class Mul(prop: Prop, expr: UExpr) extends FieldUpdateExpr(prop)

    case class Rename(prop: Prop, expr: UExpr) extends FieldUpdateExpr(prop)

    case class SetOnInsert(prop: Prop, expr: UExpr) extends FieldUpdateExpr(prop)
  }

}
