package ru.tinkoff.oolong.mongo

import java.util.regex.Pattern
import scala.jdk.CollectionConverters.*
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

import org.bson.BsonType
import org.mongodb.scala.bson.BsonArray
import org.mongodb.scala.bson.BsonBoolean
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.bson.BsonDouble
import org.mongodb.scala.bson.BsonInt32
import org.mongodb.scala.bson.BsonInt64
import org.mongodb.scala.bson.BsonString
import org.mongodb.scala.bson.BsonValue

import ru.tinkoff.oolong.*
import ru.tinkoff.oolong.TypeInfo
import ru.tinkoff.oolong.Utils.PatternInstance.given
import ru.tinkoff.oolong.bson.*
import ru.tinkoff.oolong.bson.meta.QueryMeta
import ru.tinkoff.oolong.mongo.MongoQueryNode as MQ

object MongoQueryCompiler extends Backend[QExpr, MQ, BsonDocument] {

  override def opt[Doc: Type](ast: QExpr)(using quotes: Quotes): MongoQueryNode = {
    import quotes.reflect.*

    val meta: Map[String, String] = Expr.summon[QueryMeta[Doc]] match
      case Some(meta) => meta.valueOrAbort.map
      case None       => Map.empty[String, String]

    def rec(ast: QExpr, renames: Map[String, String] = Map.empty): MongoQueryNode =
      ast match {
        case QExpr.Prop(path) => MQ.Field(renames.getOrElse(path, path))
        case QExpr.Gte(x, y)  => MQ.OnField(getField(x)(renames), MQ.Gte(rec(y)))
        case QExpr.Lte(x, y)  => MQ.OnField(getField(x)(renames), MQ.Lte(rec(y)))
        case QExpr.Gt(x, y)   => MQ.OnField(getField(x)(renames), MQ.Gt(rec(y)))
        case QExpr.Lt(x, y)   => MQ.OnField(getField(x)(renames), MQ.Lt(rec(y)))
        case QExpr.Eq(x, y)   => MQ.OnField(getField(x)(renames), MQ.Eq(rec(y)))
        case QExpr.Ne(x, y)   => MQ.OnField(getField(x)(renames), MQ.Ne(rec(y)))
        case QExpr.In(x, exprs) =>
          MQ.OnField(
            getField(x)(renames),
            MQ.In(handleArrayConds(exprs))
          )
        case QExpr.Nin(x, exprs) =>
          MQ.OnField(
            getField(x)(renames),
            MQ.Nin(handleArrayConds(exprs))
          )
        case QExpr.And(exprs)              => MQ.And(exprs.map(rec(_, renames)))
        case QExpr.Or(exprs)               => MQ.Or(exprs.map(rec(_, renames)))
        case QExpr.Constant(s)             => MQ.Constant(s)
        case QExpr.Exists(x, y)            => MQ.OnField(getField(x)(renames), MQ.Exists(rec(y)))
        case QExpr.Size(x, y)              => MQ.OnField(getField(x)(renames), MQ.Size(rec(y)))
        case QExpr.Regex(x, pattern)       => MQ.OnField(getField(x)(renames), MQ.Regex(pattern))
        case QExpr.ScalaCode(code)         => MQ.ScalaCode(code)
        case QExpr.ScalaCodeIterable(iter) => MQ.ScalaCodeIterable(iter)
        case QExpr.TypeCheck(x, y) =>
          import y.quotedType
          val tpe                = TypeRepr.of[y.Type]
          def checkType[T: Type] = tpe =:= TypeRepr.of[T]
          val bsonType = tpe match {
            case _ if checkType[MongoType.DOUBLE]                => BsonType.DOUBLE
            case _ if checkType[MongoType.STRING]                => BsonType.STRING
            case _ if checkType[MongoType.DOCUMENT]              => BsonType.DOCUMENT
            case _ if checkType[MongoType.ARRAY]                 => BsonType.ARRAY
            case _ if checkType[MongoType.BINARY]                => BsonType.BINARY
            case _ if checkType[MongoType.UNDEFINED]             => BsonType.UNDEFINED
            case _ if checkType[MongoType.OBJECT_ID]             => BsonType.OBJECT_ID
            case _ if checkType[MongoType.BOOLEAN]               => BsonType.BOOLEAN
            case _ if checkType[MongoType.DATE_TIME]             => BsonType.DATE_TIME
            case _ if checkType[MongoType.NULL]                  => BsonType.NULL
            case _ if checkType[MongoType.REGULAR_EXPRESSION]    => BsonType.REGULAR_EXPRESSION
            case _ if checkType[MongoType.DB_POINTER]            => BsonType.DB_POINTER
            case _ if checkType[MongoType.JAVASCRIPT]            => BsonType.JAVASCRIPT
            case _ if checkType[MongoType.SYMBOL]                => BsonType.SYMBOL
            case _ if checkType[MongoType.JAVASCRIPT_WITH_SCOPE] => BsonType.JAVASCRIPT_WITH_SCOPE
            case _ if checkType[MongoType.INT32]                 => BsonType.INT32
            case _ if checkType[MongoType.TIMESTAMP]             => BsonType.TIMESTAMP
            case _ if checkType[MongoType.INT64]                 => BsonType.INT64
            case _ if checkType[MongoType.DECIMAL128]            => BsonType.DECIMAL128
            case _ if checkType[MongoType.MIN_KEY]               => BsonType.MIN_KEY
            case _ if checkType[MongoType.MAX_KEY]               => BsonType.MAX_KEY
            case _ => report.errorAndAbort(s"Unsupported bson type for ${tpe.show}")
          }
          MQ.OnField(getField(x)(renames), MQ.TypeCheck(MQ.Constant(bsonType.getValue)))
        case QExpr.Mod(x, d, r) =>
          MQ.OnField(getField(x)(renames), MQ.Mod(rec(d), rec(r)))
        case QExpr.Subquery(code) =>
          code match {
            case '{ $doc: BsonDocument } => MQ.Subquery(doc)
            case _ =>
              report.errorAndAbort(s"Expected the subquery inside 'unchecked(...)' to have 'org.mongodb.scala.bson.BsonDocument' type, but the subquery is '${code.show}'")
          }
        case not: QExpr.Not => handleInnerNot(not)(renames)
      }

    rec(ast, meta)

  }

  def getField(f: QExpr)(renames: Map[String, String])(using quotes: Quotes): MQ.Field =
    import quotes.reflect.*
    f match
      case QExpr.Prop(path) => MQ.Field(renames.getOrElse(path, path))
      case _                => report.errorAndAbort("Field is of wrong type")

  def handleInnerNot(not: QExpr.Not)(renames: Map[String, String])(using quotes: Quotes): MongoQueryNode =
    import quotes.reflect.*
    not.x match
      case QExpr.Gte(x, y)   => MQ.OnField(getField(x)(renames), MQ.Not(MQ.Gte(opt(y))))
      case QExpr.Lte(x, y)   => MQ.OnField(getField(x)(renames), MQ.Not(MQ.Lte(opt(y))))
      case QExpr.Gt(x, y)    => MQ.OnField(getField(x)(renames), MQ.Not(MQ.Gt(opt(y))))
      case QExpr.Lt(x, y)    => MQ.OnField(getField(x)(renames), MQ.Not(MQ.Lt(opt(y))))
      case QExpr.Eq(x, y)    => MQ.OnField(getField(x)(renames), MQ.Not(MQ.Eq(opt(y))))
      case QExpr.Ne(x, y)    => MQ.OnField(getField(x)(renames), MQ.Not(MQ.Ne(opt(y))))
      case QExpr.Size(x, y)  => MQ.OnField(getField(x)(renames), MQ.Not(MQ.Size(opt(y))))
      case QExpr.Regex(x, p) => MQ.OnField(getField(x)(renames), MQ.Regex(p))
      case QExpr.In(x, y)    => MQ.OnField(getField(x)(renames), MQ.Not(MQ.In(handleArrayConds(y))))
      case QExpr.Nin(x, y)   => MQ.OnField(getField(x)(renames), MQ.Not(MQ.Nin(handleArrayConds(y))))
      case _                 => report.errorAndAbort("Wrong operator inside $not")

  def handleArrayConds(x: List[QExpr] | QExpr)(using quotes: Quotes): List[MQ] | MQ =
    x match
      case list: List[QExpr @unchecked] => list map opt
      case expr: QExpr                  => opt(expr)

  override def render(node: MongoQueryNode)(using quotes: Quotes): String =
    import quotes.reflect.*
    def rec(node: MongoQueryNode)(using quotes: Quotes): String =
      node match
        case MQ.OnField(prop, x) => "\"" + prop.path + "\"" + ": " + rec(x)
        case MQ.Gte(x)           => "{ \"$gte\": " + rec(x) + " }"
        case MQ.Lte(x)           => "{ \"$lte\": " + rec(x) + " }"
        case MQ.Gt(x)            => "{ \"$gt\": " + rec(x) + " }"
        case MQ.Lt(x)            => "{ \"$lt\": " + rec(x) + " }"
        case MQ.Eq(x)            => rec(x)
        case MQ.Ne(x)            => "{ \"$ne\": " + rec(x) + " }"
        case MQ.Not(x)           => "{ \"$not\": " + rec(x) + " }"
        case MQ.Size(x)          => "{ \"$size\": " + rec(x) + " }"
        case MQ.Regex(pattern) =>
          pattern.value match
            case Some(p: Pattern) =>
              val (pattern, options) = parsePattern(p)
              "{ \"$regex\": \"" + pattern + "\"" + options.map(", \"$options\": \"" + _ + "\"").getOrElse("") + " }"
            case _ => "{ \"$regex\": \"?\" \"$options\": \"?\" }"

        case MQ.In(exprs)  => "{ \"$in\": [" + renderArrays(exprs) + "] }"
        case MQ.Nin(exprs) => "{ \"$nin\": [" + renderArrays(exprs) + "] }"
        case MQ.And(exprs) =>
          val fields = exprs.collect { case q: MQ.OnField => q.field.path.mkString(".") }
          if (fields.distinct.size < fields.size)
            "\"$and\": [ " + exprs.map(rec).map("{ " + _ + " }").mkString(", ") + " ]"
          else exprs.map(rec).mkString(", ")
        case MQ.Or(exprs)               => "\"$or\": [ " + exprs.map(rec).map("{ " + _ + " }").mkString(", ") + " ]"
        case MQ.Exists(x)               => " { \"$exists\": " + rec(x) + " }"
        case MQ.Constant(s: String)     => "\"" + s + "\""
        case MQ.Constant(s: Any)        => s.toString // also limit
        case MQ.ScalaCode(code)         => renderCode(code)
        case MQ.ScalaCodeIterable(_)    => "?"
        case MQ.Subquery(_)             => "{...}"
        case MQ.TypeCheck(bsonType)     => "{ \"$type\": " + rec(bsonType) + " }"
        case MQ.Mod(divisor, remainder) => "{ \"$mod\": [" + rec(divisor) + "," + rec(remainder) + "] }"
        case MQ.Field(field) =>
          report.errorAndAbort(s"There is no filter condition on field ${field.mkString(".")}")
    end rec

    def renderArrays(x: List[MQ] | MQ)(using Quotes): String = x match
      case list: List[MQ @unchecked] => list.map(rec).mkString(", ")
      case node: MQ                  => rec(node)

    def renderCode(expr: Expr[Any])(using Quotes) = expr match
      case '{ ${ x }: t } => RenderUtils.renderCaseClass[t](x)
      case _              => "?"

    "{ " + rec(node) + " }"
  end render

  override def target(optRepr: MongoQueryNode)(using quotes: Quotes): Expr[BsonDocument] =
    import quotes.reflect.*
    optRepr match {
      case and: MQ.And         => handleAnd(and)
      case or: MQ.Or           => handleOr(or)
      case MQ.OnField(prop, x) => '{ BsonDocument(${ Expr(prop.path) } -> ${ parseOperators(x) }) }
      case MQ.Subquery(doc)    => doc
      case _                   => report.errorAndAbort("given node can't be in that position")
    }

  def parseOperators(optRepr: MongoQueryNode)(using quotes: Quotes): Expr[BsonValue] =
    import quotes.reflect.*
    optRepr match
      case MQ.Eq(x) => handleValues(x)
      case MQ.Gte(x) =>
        '{ BsonDocument("$gte" -> ${ handleValues(x) }) }
      case MQ.Lte(x) =>
        '{ BsonDocument("$lte" -> ${ handleValues(x) }) }
      case MQ.Gt(x) =>
        '{ BsonDocument("$gt" -> ${ handleValues(x) }) }
      case MQ.Lt(x) =>
        '{ BsonDocument("$lt" -> ${ handleValues(x) }) }
      case MQ.Ne(x) =>
        '{ BsonDocument("$ne" -> ${ handleValues(x) }) }
      case MQ.Size(x) =>
        '{ BsonDocument("$size" -> ${ handleValues(x) }) }
      case MQ.Regex(p) =>
        p.value match
          case Some(pattern) =>
            val (regex, options) = parsePattern(pattern)
            '{
              BsonDocument(
                (Map("$regex"     -> BsonString(${ Expr(regex) })) ++ ${ Expr(options) }
                  .map("$options" -> BsonString(_))).toList
              )
            }
          case _ =>
            '{
              val (regex, options) = parsePattern(${ p })
              BsonDocument((Map("$regex" -> BsonString(regex)) ++ options.map("$options" -> BsonString(_))).toList)
            }
      case MQ.In(exprs) =>
        '{
          BsonDocument("$in" -> ${ handleArrayCond(exprs) })
        }
      case MQ.Nin(exprs) =>
        '{
          BsonDocument("$nin" -> ${ handleArrayCond(exprs) })
        }
      case MQ.Not(x) =>
        '{ BsonDocument("$not" -> ${ parseOperators(x) }) }
      case MQ.Exists(x) =>
        '{ BsonDocument("$exists" -> ${ handleValues(x) }) }
      case MQ.TypeCheck(bsonType) =>
        '{ BsonDocument("$type" -> ${ handleValues(bsonType) }) }
      case MQ.Mod(divisor, remainder) =>
        '{
          BsonDocument("$mod" -> BsonArray.fromIterable(List(${ handleValues(divisor) }, ${ handleValues(remainder) })))
        }
      case _ => report.errorAndAbort(s"Wrong operator: ${optRepr}")
  end parseOperators

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
      val exprs: List[BsonDocument] = ${ Expr.ofList(and.exprs.map(target)) }
      if (exprs.flatMap(_.keySet().asScala).distinct.size < exprs.size)
        BsonDocument("$and" -> BsonArray.fromIterable(exprs))
      else BsonDocument(exprs.map(_.asScala.toList).foldLeft(List.empty[(String, BsonValue)])(_ ++ _))
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
      case MQ.Field(path) => Expr(path)
      case _              => report.errorAndAbort("field should be string")

  def parsePattern(pattern: Pattern): (String, Option[String]) =
    val flags = List(
      if (pattern.flags & Pattern.CASE_INSENSITIVE) != 0 then Some("i") else None,
      if (pattern.flags & Pattern.MULTILINE) != 0 then Some("m") else None,
      if (pattern.flags & Pattern.COMMENTS) != 0 then Some("x") else None,
      if (pattern.flags & Pattern.DOTALL) != 0 then Some("s") else None,
    ).flatten

    val options = if (flags.isEmpty) None else Some(flags.reduce(_ + _))
    val matcher = Pattern.compile("(\\(\\?([a-z]*)\\))?(.*)").matcher(pattern.pattern)
    matcher.matches()
    matcher.group(3) -> options
  end parsePattern

}
