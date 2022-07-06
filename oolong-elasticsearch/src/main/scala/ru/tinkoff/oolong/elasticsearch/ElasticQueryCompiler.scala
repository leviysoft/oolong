package ru.tinkoff.oolong.elasticsearch

import scala.annotation.nowarn
import scala.quoted.Expr
import scala.quoted.Quotes

import org.bson.json.JsonMode

import ru.tinkoff.oolong.*
import ru.tinkoff.oolong.Utils.*
import ru.tinkoff.oolong.elasticsearch.ElasticQueryNode as EQN

object ElasticQueryCompiler extends Backend[QExpr, ElasticQueryNode, JsonNode] {
  override def opt(ast: QExpr)(using quotes: Quotes): ElasticQueryNode = {
    import quotes.reflect.*

    ast match {
      case QExpr.Prop(path)                              => EQN.Field(path)
      case QExpr.Eq(QExpr.Prop(path), QExpr.Constant(s)) => EQN.Term(EQN.Field(path), EQN.Constant(s))
      case QExpr.Ne(QExpr.Prop(path), QExpr.Constant(s)) =>
        EQN.Bool(mustNot = EQN.Term(EQN.Field(path), EQN.Constant(s)) :: Nil)
      case QExpr.Gte(QExpr.Prop(path), QExpr.Constant(s)) => EQN.Range(EQN.Field(path), gte = Some(EQN.Constant(s)))
      case QExpr.Lte(QExpr.Prop(path), QExpr.Constant(s)) => EQN.Range(EQN.Field(path), lte = Some(EQN.Constant(s)))
      case QExpr.Gt(QExpr.Prop(path), QExpr.Constant(s))  => EQN.Range(EQN.Field(path), gt = Some(EQN.Constant(s)))
      case QExpr.Lt(QExpr.Prop(path), QExpr.Constant(s))  => EQN.Range(EQN.Field(path), lt = Some(EQN.Constant(s)))
      case QExpr.And(exprs)                               => EQN.Bool(must = exprs map opt)
      case QExpr.Or(exprs)                                => EQN.Bool(should = exprs map opt)
      case QExpr.Not(expr)                                => EQN.Bool(mustNot = opt(expr) :: Nil)
      case QExpr.Exists(QExpr.Prop(path), QExpr.Constant(true)) => EQN.Exists(EQN.Field(path))
      case QExpr.Exists(QExpr.Prop(path), QExpr.Constant(false)) =>
        EQN.Bool(mustNot = EQN.Exists(EQN.Field(path)) :: Nil)
      case unhandled => report.errorAndAbort("Unprocessable")
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
        s"""{ "term": {"${path.mkString(".")}": ${render(x)} } }"""
      case EQN.Bool(must, should, mustNot) =>
        s"""{"must": [${must.map(render).mkString(", ")}], "should": [${should
          .map(render)
          .mkString(", ")}], "must_not": [${mustNot.map(render).mkString(", ")}]}"""
      case EQN.Constant(s: String) => "\"" + s + "\""
      case EQN.Constant(s: Any)    => s.toString
      case EQN.Exists(EQN.Field(path)) =>
        s"""{ "exists": { "field": "${path.mkString(".")}" }}"""
      case EQN.Range(EQN.Field(path), gt, gte, lt, lte) =>
        val bounds = Seq(
          renderKeyMap(""""gt"""", gt),
          renderKeyMap(""""gte"""", gte),
          renderKeyMap(""""lt"""", lt),
          renderKeyMap(""""lte"""", lte)
        ).flatten
        s"""{"range": {"${path.mkString(".")}": {${bounds.mkString(",")}}}}"""
      case EQN.Field(field) =>
        // TODO: adjust error message
        report.errorAndAbort(s"There is no filter condition on field ${field.mkString(".")}")
      case _ => "AST can't be rendered"
    }
  }

  private def renderKeyMap(key: String, node: Option[ElasticQueryNode])(using quotes: Quotes): Option[String] =
    node.map(render(_)).map(v => s"""$key:$v""")

  override def target(optRepr: ElasticQueryNode)(using quotes: Quotes): Expr[JsonNode] = {
    import quotes.reflect.*

    optRepr match {
      case bool: EQN.Bool =>
        '{
          JsonNode.obj(
            "bool" -> JsonNode.obj(
              "must"     -> JsonNode.Arr(${ Expr.ofSeq(bool.must.map(target)) }),
              "should"   -> JsonNode.Arr(${ Expr.ofSeq(bool.should.map(target)) }),
              "must_not" -> JsonNode.Arr(${ Expr.ofSeq(bool.mustNot.map(target)) }),
            )
          )
        }
      case EQN.Term(EQN.Field(path), x) =>
        '{ JsonNode.obj("term" -> JsonNode.obj(${ Expr(path.mkString(".")) } -> ${ handleValues(x) })) }
      case EQN.Exists(EQN.Field(path)) =>
        '{ JsonNode.obj("exists" -> JsonNode.obj("field" -> JsonNode.Str(${ Expr(path.mkString(".")) }))) }
      case EQN.Range(EQN.Field(path), gt, gte, lt, lte) =>
        '{
          JsonNode.obj(
            "range" -> JsonNode.obj(
              ${ Expr(path.mkString(".")) } -> JsonNode.obj(
                "gt"  -> ${ gt.map(handleValues(_)).getOrElse('{ JsonNode.`null` }) },
                "gte" -> ${ gte.map(handleValues(_)).getOrElse('{ JsonNode.`null` }) },
                "lt"  -> ${ lt.map(handleValues(_)).getOrElse('{ JsonNode.`null` }) },
                "lte" -> ${ lte.map(handleValues(_)).getOrElse('{ JsonNode.`null` }) }
              )
            )
          )
        }
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

  override def optimize(query: ElasticQueryNode): ElasticQueryNode = query match {
    case EQN.Bool(must, should, mustNot) =>
      val mustBuilder    = List.newBuilder[ElasticQueryNode]
      val shouldBuilder  = List.newBuilder[ElasticQueryNode].addAll(should.map(optimize))
      val mustNotBuilder = List.newBuilder[ElasticQueryNode].addAll(mustNot.map(optimize))

      lazy val orCount = must.count {
        case EQN.Bool.Or(_) => true
        case _              => false
      }

      for (mp <- must.map(optimize)) mp match {
        case EQN.Bool.And(must2) =>
          must2.foreach(mustBuilder += _)
        case EQN.Bool.Or(should2) if should.isEmpty && orCount == 1 =>
          should2.foreach(shouldBuilder += _)
        // !(a || b || c) => !a && !b && !c
        case EQN.Bool.Or(should2) if should2.pforall { case EQN.Bool.Not(_) => true } =>
          should2.foreach { case EQN.Bool.Not(p) =>
            mustNotBuilder += p
          }: @nowarn
        case EQN.Bool.Not(not) =>
          mustNotBuilder += not
        case other => mustBuilder += other
      }

      EQN.Bool(mustBuilder.result, shouldBuilder.result, mustNotBuilder.result)

    case other => other
  }
}
