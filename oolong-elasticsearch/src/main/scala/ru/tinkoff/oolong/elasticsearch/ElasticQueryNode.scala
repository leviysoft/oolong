package ru.tinkoff.oolong.elasticsearch

sealed trait ElasticQueryNode

object ElasticQueryNode {
  case class Field(path: List[String]) extends ElasticQueryNode

  case class Term(field: Field, expr: ElasticQueryNode) extends ElasticQueryNode

  case class Bool(
      must: List[ElasticQueryNode] = Nil,
      should: List[ElasticQueryNode] = Nil,
      mustNot: List[ElasticQueryNode] = Nil
  ) extends ElasticQueryNode
  case class Constant[T](s: T) extends ElasticQueryNode
}
