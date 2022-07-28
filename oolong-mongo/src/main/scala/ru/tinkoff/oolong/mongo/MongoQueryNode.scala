package ru.tinkoff.oolong.mongo

import java.util.regex.Pattern
import scala.quoted.Expr

import org.mongodb.scala.bson.BsonDocument

import ru.tinkoff.oolong.mongo.MongoQueryNode as MQ

sealed trait MongoQueryNode

case object MongoQueryNode {
  case class Field(path: String) extends MQ

  case class OnField(field: Field, expr: MQ) extends MQ

  // add super class for Gte, Lte, Eq, etc?
  case class Not(x: MQ) extends MQ

  case class Gte(x: MQ) extends MQ
  case class Lte(x: MQ) extends MQ
  case class Gt(x: MQ) extends MQ
  case class Lt(x: MQ) extends MQ
  case class Eq(x: MQ) extends MQ
  case class Ne(x: MQ) extends MQ
  case class In(x: List[MQ] | MQ) extends MQ
  case class Nin(x: List[MQ] | MQ) extends MQ
  // add super class for OR and AND?

  case class And(exprs: List[MQ]) extends MQ
  case class Or(exprs: List[MQ]) extends MQ
  case class Constant[T](s: T) extends MQ
  case class ScalaCode(code: Expr[Any]) extends MQ
  case class ScalaCodeIterable(code: Expr[Iterable[Any]]) extends MQ
  case class Subquery(code: Expr[BsonDocument]) extends MQ

  case class Exists(x: MQ) extends MQ

  case class Size(x: MQ) extends MQ

  case class Regex(pattern: Pattern | Expr[Pattern]) extends MQ
}
