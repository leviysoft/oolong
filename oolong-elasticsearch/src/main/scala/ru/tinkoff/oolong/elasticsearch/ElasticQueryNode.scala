package ru.tinkoff.oolong.elasticsearch

sealed trait ElasticQueryNode

object ElasticQueryNode {
  case class Field(path: List[String]) extends ElasticQueryNode

  case class Term(field: Field, expr: ElasticQueryNode) extends ElasticQueryNode
  case class Constant[T](s: T) extends ElasticQueryNode

  case class Exists(x: ElasticQueryNode) extends ElasticQueryNode

  case class Bool(
      must: List[ElasticQueryNode] = Nil,
      should: List[ElasticQueryNode] = Nil,
      mustNot: List[ElasticQueryNode] = Nil
  ) extends ElasticQueryNode

  object Bool {
    object And {
      def unapply(bool: Bool): Option[List[ElasticQueryNode]] = bool match {
        case Bool(and, Nil, Nil) => Some(and)
        case _                   => None
      }
    }

    object Or {
      def unapply(bool: Bool): Option[List[ElasticQueryNode]] = bool match {
        case Bool(Nil, or, Nil) => Some(or)
        case _                  => None
      }
    }

    object Not {
      def unapply(bool: Bool): Option[ElasticQueryNode] = bool match {
        case Bool(Nil, Nil, List(not)) => Some(not)
        case _                         => None
      }
    }
  }

  case class Range(
      field: Field,
      gt: Option[ElasticQueryNode] = None,
      gte: Option[ElasticQueryNode] = None,
      lt: Option[ElasticQueryNode] = None,
      lte: Option[ElasticQueryNode] = None
  ) extends ElasticQueryNode
}
