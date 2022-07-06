package ru.tinkoff.oolong

import scala.compiletime.ops.boolean

import org.apache.commons.text.StringEscapeUtils

sealed private[oolong] trait JsonNode {
  def render: String
}

private[oolong] object JsonNode {
  case object Null extends JsonNode {
    override def render: String = "null"
  }
  case class Bool(value: Boolean) extends JsonNode {
    override def render: String = value.toString
  }
  case class Num(value: BigDecimal) extends JsonNode {
    override def render: String = value.toString
  }
  case class Str(value: String) extends JsonNode {
    override def render: String = s"\"${StringEscapeUtils.escapeJson(value)}\""
  }
  case class Arr(value: Seq[JsonNode]) extends JsonNode {
    override def render: String = value.map(_.render).mkString("[", ",", "]")
  }
  case class Obj(value: Map[String, JsonNode]) extends JsonNode {
    override def render: String = value.map((k, v) => s"\"$k\":${v.render}").mkString("{", ",", "}")
  }

  val `null`: JsonNode = Null

  def obj(head: (String, JsonNode), tail: (String, JsonNode)*): Obj =
    Obj((head +: tail).to(Map))
}
