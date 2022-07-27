package ru.tinkoff.oolong.mongo

import scala.quoted.Expr

import ru.tinkoff.oolong.mongo.MongoUpdateNode as MU

sealed trait MongoUpdateNode

case object MongoUpdateNode {
  case class Prop(path: String) extends MU

  case class Update(setters: List[MongoUpdateOp]) extends MU

  case class Constant[T](t: T) extends MU

  case class ScalaCode(code: Expr[Any]) extends MU

  sealed abstract class MongoUpdateOp(val prop: Prop, val value: MU) extends MU
  object MongoUpdateOp {
    case class Set(override val prop: Prop, override val value: MU) extends MongoUpdateOp(prop, value)
    case class Inc(override val prop: Prop, override val value: MU) extends MongoUpdateOp(prop, value)
    case class Unset(override val prop: Prop) extends MongoUpdateOp(prop, MU.Constant(""))
    case class Max(override val prop: Prop, override val value: MU) extends MongoUpdateOp(prop, value)
    case class Min(override val prop: Prop, override val value: MU) extends MongoUpdateOp(prop, value)
    case class Mul(override val prop: Prop, override val value: MU) extends MongoUpdateOp(prop, value)
    case class Rename(override val prop: Prop, override val value: MU) extends MongoUpdateOp(prop, value)
    case class SetOnInsert(override val prop: Prop, override val value: MU) extends MongoUpdateOp(prop, value)
  }
}
