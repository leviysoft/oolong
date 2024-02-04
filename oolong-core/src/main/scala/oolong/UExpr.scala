package oolong

import scala.annotation.nowarn
import scala.quoted.Expr

sealed private[oolong] trait UExpr

private[oolong] object UExpr {

  case class Update(ops: List[FieldUpdateExpr]) extends UExpr

  case class Prop(path: String) extends UExpr

  case class Constant[T](t: T) extends UExpr
  case class UIterable[T](t: List[UExpr]) extends UExpr

  case class ScalaCode(code: Expr[Any]) extends UExpr

  case class ScalaCodeIterable(code: Expr[Iterable[Any]]) extends UExpr

  @nowarn("msg=unused explicit parameter") // used in macro
  sealed abstract class FieldUpdateExpr(prop: Prop)

  object FieldUpdateExpr {

    // field update operators
    case class Set(prop: Prop, expr: UExpr) extends FieldUpdateExpr(prop: Prop)

    case class Inc(prop: Prop, expr: UExpr) extends FieldUpdateExpr(prop)

    case class Unset(prop: Prop) extends FieldUpdateExpr(prop)

    case class Min(prop: Prop, expr: UExpr) extends FieldUpdateExpr(prop)

    case class Max(prop: Prop, expr: UExpr) extends FieldUpdateExpr(prop)

    case class Mul(prop: Prop, expr: UExpr) extends FieldUpdateExpr(prop)

    case class Rename(prop: Prop, expr: UExpr) extends FieldUpdateExpr(prop)

    case class SetOnInsert(prop: Prop, expr: UExpr) extends FieldUpdateExpr(prop)

    // array update operators
    case class AddToSet(prop: Prop, expr: UExpr, multipleValues: Boolean) extends FieldUpdateExpr(prop)

    case class Pop(prop: Prop, remove: Pop.Remove) extends FieldUpdateExpr(prop)

    object Pop {
      enum Remove {
        case First, Last
      }
    }

  }

}
