package ru.tinkoff.oolong.elasticsearch

import scala.quoted.Expr
import scala.quoted.Quotes

import org.bson.json.JsonMode

import ru.tinkoff.oolong.*
import ru.tinkoff.oolong.elasticsearch.ElasticQueryNode as EQN

object ElasticQueryCompiler extends Backend[QExpr, ElasticQueryNode, JsonNode] {
  override def opt(ast: QExpr)(using quotes: Quotes): ElasticQueryNode = {
    import quotes.reflect.*

    ast match {
      case QExpr.Prop(path)               => EQN.Field(path)
      case QExpr.Eq(x, QExpr.Constant(s)) => EQN.Term(getField(x), EQN.Constant(s))
      case QExpr.And(exprs)               => EQN.And(exprs map opt)
      case QExpr.Or(exprs)                => EQN.Or(exprs map opt)
      case unhandled                      => report.errorAndAbort("Unprocessable")
    }
  }

  def getField(f: QExpr)(using quotes: Quotes): EQN.Field =
    import quotes.reflect.*
    f match
      case QExpr.Prop(path) => EQN.Field(path)
      case _                => report.errorAndAbort("Field is of wrong type")

  override def render(node: ElasticQueryNode)(using quotes: Quotes): String = {
    import quotes.reflect.*

    node match {
      case EQN.Term(EQN.Field(path), x) =>
        "{ \"term\": {" + "\"" + path.mkString(".") + "\"" + ": " + render(x) + " } }"
      case EQN.And(exprs)          => "{ \"must\": [ " + exprs.map(render).mkString(", ") + " ] }"
      case EQN.Or(exprs)           => "{ \"should\": [ " + exprs.map(render).mkString(", ") + " ] }"
      case EQN.Constant(s: String) => "\"" + s + "\""
      case EQN.Constant(s: Any)    => s.toString
      case EQN.Field(field)        =>
        // TODO: adjust error message
        report.errorAndAbort(s"There is no filter condition on field ${field.mkString(".")}")
    }
  }

  override def target(optRepr: ElasticQueryNode)(using quotes: Quotes): Expr[JsonNode] = {
    import quotes.reflect.*

    optRepr match {
      case and: EQN.And =>
        '{
          JsonNode.obj(
            "must" ->
              JsonNode.Arr(${ Expr.ofSeq(and.exprs.map(target)) })
          )
        }
      case or: EQN.Or =>
        '{
          JsonNode.obj(
            "should" ->
              JsonNode.Arr(${ Expr.ofSeq(or.exprs.map(target)) })
          )
        }
      case EQN.Term(EQN.Field(path), x) =>
        '{ JsonNode.obj("term" -> JsonNode.obj(${ Expr(path.mkString(".")) } -> ${ handleValues(x) })) }
      case _ => report.errorAndAbort("given node can't be in that position")
    }
  }

  def handleValues(expr: ElasticQueryNode)(using q: Quotes): Expr[JsonNode] =
    import q.reflect.*

    expr match {
      case EQN.Constant(i: Long) =>
        '{ JsonNode.Num.apply(BigDecimal.apply(${ Expr(i: Long) })) }
      case EQN.Constant(i: Int) =>
        '{ JsonNode.Num.apply(BigDecimal.apply(${ Expr(i: Int) })) }
      case EQN.Constant(i: Short) =>
        '{ JsonNode.Num.apply(BigDecimal.apply(${ Expr(i: Short) })) }
      case EQN.Constant(i: Byte) =>
        '{ JsonNode.Num.apply(BigDecimal.apply(${ Expr(i: Byte) })) }
      case EQN.Constant(i: Double) =>
        '{ JsonNode.Num.apply(BigDecimal.apply(${ Expr(i: Double) })) }
      case EQN.Constant(i: Float) =>
        '{ JsonNode.Num.apply(BigDecimal.apply(${ Expr(i: Float) })) }
      case EQN.Constant(i: String) =>
        '{ JsonNode.Str.apply(${ Expr(i: String) }) }
      case EQN.Constant(i: Boolean) =>
        '{ JsonNode.Bool.apply(${ Expr(i: Boolean) }) }
      case _ => report.errorAndAbort(s"Given type is not literal constant")
    }
}
