package ru.tinkoff.oolong.elasticsearch

sealed trait ElasticQueryNode

object ElasticQueryNode {
  case class Field(path: List[String]) extends ElasticQueryNode

  case class Term(field: Field, expr: ElasticQueryNode) extends ElasticQueryNode

  case class And(exprs: List[ElasticQueryNode]) extends ElasticQueryNode
  case class Or(exprs: List[ElasticQueryNode]) extends ElasticQueryNode
  case class Constant[T](s: T) extends ElasticQueryNode
}
