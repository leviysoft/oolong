package ru.tinkoff.oolong.mongo

import scala.quoted.Expr
import scala.quoted.Quotes

import org.mongodb.scala.bson.BsonArray
import org.mongodb.scala.bson.BsonBoolean
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.bson.BsonDouble
import org.mongodb.scala.bson.BsonInt32
import org.mongodb.scala.bson.BsonInt64
import org.mongodb.scala.bson.BsonString
import org.mongodb.scala.bson.BsonValue

import ru.tinkoff.oolong.*
import ru.tinkoff.oolong.bson.*
import ru.tinkoff.oolong.mongo.MongoQueryNode as MQ

object MongoQueryCompiler extends Backend[QExpr, MQ, BsonDocument] {

  override def opt(ast: QExpr)(using quotes: Quotes): MongoQueryNode = {
    import quotes.reflect.*

    ast match {
      case QExpr.Prop(path) => MQ.Field(path)
      case QExpr.Gte(x, y)  => MQ.OnField(getField(x), MQ.Gte(opt(y)))
      case QExpr.Lte(x, y)  => MQ.OnField(getField(x), MQ.Lte(opt(y)))
      case QExpr.Gt(x, y)   => MQ.OnField(getField(x), MQ.Gt(opt(y)))
      case QExpr.Lt(x, y)   => MQ.OnField(getField(x), MQ.Lt(opt(y)))
      case QExpr.Eq(x, y)   => MQ.OnField(getField(x), MQ.Eq(opt(y)))
      case QExpr.Ne(x, y)   => MQ.OnField(getField(x), MQ.Ne(opt(y)))
      case QExpr.In(x, exprs) =>
        MQ.OnField(
          getField(x),
          MQ.In(handleArrayConds(exprs))
        )
      case QExpr.Nin(x, exprs) =>
        MQ.OnField(
          getField(x),
          MQ.Nin(handleArrayConds(exprs))
        )
      case QExpr.And(exprs)              => MQ.And(exprs map opt)
      case QExpr.Or(exprs)               => MQ.Or(exprs map opt)
      case QExpr.Constant(s)             => MQ.Constant(s)
      case QExpr.Exists(x, y)            => MQ.OnField(getField(x), MQ.Exists(opt(y)))
      case QExpr.Size(x, y)              => MQ.OnField(getField(x), MQ.Size(opt(y)))
      case QExpr.ScalaCode(code)         => MQ.ScalaCode(code)
      case QExpr.ScalaCodeIterable(iter) => MQ.ScalaCodeIterable(iter)
      case QExpr.Subquery(code) =>
        code match {
          case '{ $doc: BsonDocument } => MQ.Subquery(doc)
          case _ =>
            report.errorAndAbort(s"Expected the subquery inside 'unchecked(...)' to have 'org.mongodb.scala.bson.BsonDocument' type, but the subquery is '${code.show}'")
        }
      case not: QExpr.Not => handleInnerNot(not)
    }
  }

  def getField(f: QExpr)(using quotes: Quotes): MQ.Field =
    import quotes.reflect.*
    f match
      case QExpr.Prop(path) => MQ.Field(path)
      case _                => report.errorAndAbort("Field is of wrong type")

  def handleInnerNot(not: QExpr.Not)(using quotes: Quotes): MongoQueryNode =
    import quotes.reflect.*
    not.x match
      case QExpr.Gte(x, y)  => MQ.OnField(getField(x), MQ.Not(MQ.Gte(opt(y))))
      case QExpr.Lte(x, y)  => MQ.OnField(getField(x), MQ.Not(MQ.Lte(opt(y))))
      case QExpr.Gt(x, y)   => MQ.OnField(getField(x), MQ.Not(MQ.Gt(opt(y))))
      case QExpr.Lt(x, y)   => MQ.OnField(getField(x), MQ.Not(MQ.Lt(opt(y))))
      case QExpr.Eq(x, y)   => MQ.OnField(getField(x), MQ.Not(MQ.Eq(opt(y))))
      case QExpr.Ne(x, y)   => MQ.OnField(getField(x), MQ.Not(MQ.Ne(opt(y))))
      case QExpr.Size(x, y) => MQ.OnField(getField(x), MQ.Not(MQ.Size(opt(y))))
      case QExpr.In(x, y)   => MQ.OnField(getField(x), MQ.Not(MQ.In(handleArrayConds(y))))
      case QExpr.Nin(x, y)  => MQ.OnField(getField(x), MQ.Not(MQ.Nin(handleArrayConds(y))))
      case _                => report.errorAndAbort("Wrong operator inside $not")

  def handleArrayConds(x: List[QExpr] | QExpr)(using quotes: Quotes): List[MQ] | MQ =
    x match
      case list: List[QExpr @unchecked] => list map opt
      case expr: QExpr                  => opt(expr)

  override def render(node: MongoQueryNode)(using quotes: Quotes): String =
    import quotes.reflect.*
    node match
      case MQ.OnField(prop, x)     => "{ " + "\"" + prop.path.mkString(".") + "\"" + ": " + render(x) + " }"
      case MQ.Gte(x)               => "{ $gte: " + render(x) + " }"
      case MQ.Lte(x)               => "{ $lte: " + render(x) + " }"
      case MQ.Gt(x)                => "{ $gt: " + render(x) + " }"
      case MQ.Lt(x)                => "{ $lt: " + render(x) + " }"
      case MQ.Eq(x)                => "{ $eq: " + render(x) + " }"
      case MQ.Ne(x)                => "{ $ne: " + render(x) + " }"
      case MQ.Not(x)               => "{ $not: " + render(x) + " }"
      case MQ.Size(x)              => "{ $size: " + render(x) + " }"
      case MQ.In(exprs)            => "{ $in: [" + renderArrays(exprs) + "] }"
      case MQ.Nin(exprs)           => "{ $nin: [" + renderArrays(exprs) + "] }"
      case MQ.And(exprs)           => "{ $and: [ " + exprs.map(render).mkString(", ") + " ] }"
      case MQ.Or(exprs)            => "{ $or: [ " + exprs.map(render).mkString(", ") + " ] }"
      case MQ.Exists(x)            => " { $exists: " + render(x) + " }"
      case MQ.Constant(s: String)  => "\"" + s + "\""
      case MQ.Constant(s: Any)     => s.toString // also limit
      case MQ.ScalaCode(_)         => "?"
      case MQ.ScalaCodeIterable(_) => "?"
      case MQ.Subquery(doc)        => "{...}"
      case MQ.Field(field) =>
        report.errorAndAbort(s"There is no filter condition on field ${field.mkString(".")}")

  def renderArrays(x: List[MQ] | MQ)(using Quotes): String = x match
    case list: List[MQ @unchecked] => list.map(render).mkString(", ")
    case node: MQ                  => render(node)

  override def target(optRepr: MongoQueryNode)(using quotes: Quotes): Expr[BsonDocument] =
    import quotes.reflect.*
    optRepr match {
      case and: MQ.And         => handleAnd(and)
      case or: MQ.Or           => handleOr(or)
      case MQ.OnField(prop, x) => '{ BsonDocument(${ Expr(prop.path.mkString(".")) } -> ${ target(x) }) }
      case MQ.Gte(x) =>
        '{ BsonDocument("$gte" -> ${ handleValues(x) }) }
      case MQ.Lte(x) =>
        '{ BsonDocument("$lte" -> ${ handleValues(x) }) }
      case MQ.Gt(x) =>
        '{ BsonDocument("$gt" -> ${ handleValues(x) }) }
      case MQ.Lt(x) =>
        '{ BsonDocument("$lt" -> ${ handleValues(x) }) }
      case MQ.Eq(x) =>
        '{ BsonDocument("$eq" -> ${ handleValues(x) }) }
      case MQ.Ne(x) =>
        '{ BsonDocument("$ne" -> ${ handleValues(x) }) }
      case MQ.Size(x) =>
        '{ BsonDocument("$size" -> ${ handleValues(x) }) }
      case MQ.In(exprs) =>
        '{
          BsonDocument("$in" -> ${ handleArrayCond(exprs) })
        }
      case MQ.Nin(exprs) =>
        '{
          BsonDocument("$nin" -> ${ handleArrayCond(exprs) })
        }
      case MQ.Not(x) =>
        '{ BsonDocument("$not" -> ${ target(x) }) }
      case MQ.Exists(x) =>
        '{ BsonDocument("$exists" -> ${ handleValues(x) }) }
      case MQ.Subquery(doc) => doc
      case _                => report.errorAndAbort("given node can't be in that position")
    }

  def handleArrayCond(x: List[MQ] | MQ)(using q: Quotes): Expr[BsonValue] =
    import q.reflect.*
    x match
      case list: List[MQ @unchecked] =>
        '{
          BsonArray.fromIterable(${
            Expr.ofList(list.map(handleValues))
          })
        }
      case MQ.ScalaCodeIterable(expr) =>
        expr match
          case '{ $l: Iterable[t] } =>
            Expr.summon[BsonEncoder[t]] match {
              case Some(encoder) => '{ BsonArray.fromIterable(${ l } map (s => ${ encoder }.bson(s))) }
              case _             => report.errorAndAbort(s"Didn't find bson encoder for type ${TypeRepr.of[t].show}")
            }
      case _ => report.errorAndAbort("Incorrect condition for array")

  def handleAnd(and: MQ.And)(using q: Quotes): Expr[BsonDocument] =
    '{
      BsonDocument("$and" -> BsonArray.fromIterable(${
        Expr.ofList(and.exprs.map(target))
      }))
    }

  def handleOr(or: MQ.Or)(using q: Quotes): Expr[BsonDocument] =
    '{
      BsonDocument("$or" -> BsonArray.fromIterable(${
        Expr.ofList(or.exprs.map(target))
      }))
    }

  def handleValues(expr: MongoQueryNode)(using q: Quotes): Expr[BsonValue] =
    import q.reflect.*
    expr match {
      case MQ.Constant(i: Long) =>
        '{ BsonInt64.apply(${ Expr(i: Long) }) }
      case MQ.Constant(i: Int) =>
        '{ BsonInt32.apply(${ Expr(i: Int) }) }
      case MQ.Constant(i: Short) =>
        '{ BsonInt32.apply(${ Expr(i: Short) }) }
      case MQ.Constant(i: Byte) =>
        '{ BsonInt32.apply(${ Expr(i: Byte) }) }
      case MQ.Constant(s: Double) =>
        '{ BsonDouble.apply(${ Expr(s: Double) }) }
      case MQ.Constant(s: Float) =>
        '{ BsonDouble.apply(${ Expr(s: Float) }) }
      case MQ.Constant(s: String) =>
        '{ BsonString.apply(${ Expr(s: String) }) }
      case MQ.Constant(s: Char) =>
        '{ BsonString.apply(${ Expr((s: Char).toString) }) }
      case MQ.Constant(b: Boolean) =>
        '{ BsonBoolean.apply(${ Expr(b: Boolean) }) }
      case MQ.ScalaCode(code) => BsonUtils.extractLifted(code)
      case _                  => report.errorAndAbort(s"Given type is not literal constant")
    }

  def extractField(expr: MongoQueryNode)(using q: Quotes): Expr[String] =
    import q.reflect.*
    expr match
      case MQ.Field(path) => Expr(path.mkString("."))
      case _              => report.errorAndAbort("field should be string")

}
