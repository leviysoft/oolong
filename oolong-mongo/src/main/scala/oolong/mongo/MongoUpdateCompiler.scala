package oolong.mongo

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

import oolong.*
import oolong.UExpr.FieldUpdateExpr
import oolong.bson.meta.QueryMeta
import oolong.mongo.MongoUpdateNode.MongoUpdateOp
import oolong.mongo.MongoUpdateNode.MongoUpdateOp.Pop.Remove
import oolong.mongo.MongoUpdateNode as MU
import org.mongodb.scala.bson.BsonArray
import org.mongodb.scala.bson.BsonBoolean
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.bson.BsonDouble
import org.mongodb.scala.bson.BsonInt32
import org.mongodb.scala.bson.BsonInt64
import org.mongodb.scala.bson.BsonString
import org.mongodb.scala.bson.BsonValue

object MongoUpdateCompiler extends Backend[UExpr, MU, BsonDocument] {

  def opt[Doc: Type](ast: UExpr)(using quotes: Quotes): MU = {
    import quotes.reflect.*

    val meta: Map[String, String] = Expr.summon[QueryMeta[Doc]] match
      case Some(meta) => meta.valueOrAbort.map
      case None       => Map.empty[String, String]

    def rec(ast: UExpr, renames: Map[String, String] = Map.empty): MU =
      ast match {
        case UExpr.Update(ops) =>
          MU.Update(ops.map {
            case FieldUpdateExpr.Set(prop, expr) =>
              MU.MongoUpdateOp.Set(MU.Prop(renames.getOrElse(prop.path, prop.path)), rec(expr))
            case FieldUpdateExpr.Inc(prop, expr) =>
              MU.MongoUpdateOp.Inc(MU.Prop(renames.getOrElse(prop.path, prop.path)), rec(expr))
            case FieldUpdateExpr.Max(prop, expr) =>
              MU.MongoUpdateOp.Max(MU.Prop(renames.getOrElse(prop.path, prop.path)), rec(expr))
            case FieldUpdateExpr.Min(prop, expr) =>
              MU.MongoUpdateOp.Min(MU.Prop(renames.getOrElse(prop.path, prop.path)), rec(expr))
            case FieldUpdateExpr.Mul(prop, expr) =>
              MU.MongoUpdateOp.Mul(MU.Prop(renames.getOrElse(prop.path, prop.path)), rec(expr))
            case FieldUpdateExpr.Rename(prop, expr) =>
              MU.MongoUpdateOp.Rename(MU.Prop(renames.getOrElse(prop.path, prop.path)), rec(expr))
            case FieldUpdateExpr.SetOnInsert(prop, expr) =>
              MU.MongoUpdateOp.SetOnInsert(MU.Prop(renames.getOrElse(prop.path, prop.path)), rec(expr))
            case FieldUpdateExpr.Unset(prop) => MU.MongoUpdateOp.Unset(MU.Prop(renames.getOrElse(prop.path, prop.path)))
            case FieldUpdateExpr.AddToSet(prop, expr, each) =>
              MU.MongoUpdateOp.AddToSet(MU.Prop(renames.getOrElse(prop.path, prop.path)), rec(expr), each)
            case FieldUpdateExpr.Pop(prop, remove) =>
              val muRemove = remove match
                case FieldUpdateExpr.Pop.Remove.First => Remove.First
                case FieldUpdateExpr.Pop.Remove.Last  => Remove.Last
              MU.MongoUpdateOp.Pop(MU.Prop(renames.getOrElse(prop.path, prop.path)), muRemove)
          })
        case UExpr.ScalaCode(code)      => MU.ScalaCode(code)
        case UExpr.Constant(t)          => MU.Constant(t)
        case UExpr.UIterable(t)         => MU.UIterable(t.map(rec(_)))
        case UExpr.ScalaCodeIterable(t) => MU.ScalaCodeIterable(t)
        case _                          => report.errorAndAbort("Unexpected expr " + pprint(ast))
      }

    rec(ast, meta)

  }

  def render(query: MU)(using quotes: Quotes): String =
    import quotes.reflect.*
    query match {
      case MU.Update(ops) =>
        List(
          renderOps(
            ops.collect { case s: MU.MongoUpdateOp.Set => s }.map(op => render(op.prop) + ": " + render(op.value))
          )("$set"),
          renderOps(
            ops.collect { case s: MU.MongoUpdateOp.Inc => s }.map(op => render(op.prop) + ": " + render(op.value))
          )("$inc"),
          renderOps(ops.collect { case s: MU.MongoUpdateOp.Unset => s }.map(op => render(op.prop) + ": " + "\"\""))(
            "$unset"
          ),
          renderOps(
            ops.collect { case s: MU.MongoUpdateOp.Max => s }.map(op => render(op.prop) + ": " + render(op.value))
          )("$max"),
          renderOps(
            ops.collect { case s: MU.MongoUpdateOp.Min => s }.map(op => render(op.prop) + ": " + render(op.value))
          )("$min"),
          renderOps(
            ops.collect { case s: MU.MongoUpdateOp.Mul => s }.map(op => render(op.prop) + ": " + render(op.value))
          )("$mul"),
          renderOps(
            ops.collect { case s: MU.MongoUpdateOp.Rename => s }.map(op => render(op.prop) + ": " + render(op.value))
          )("$rename"),
          renderOps(
            ops.collect { case s: MU.MongoUpdateOp.SetOnInsert => s }.map(op => render(op.prop) + ": " + render(op.value))
          )("$setOnInsert"),
          renderOps(
            ops.collect { case s: MU.MongoUpdateOp.AddToSet => s }.map(renderAddToSet)
          )("$addToSet"),
          renderOps(
            ops.collect { case s: MU.MongoUpdateOp.Pop => s }.map(op => render(op.prop) + ": " + render(op.value))
          )("$pop")
        ).flatten
          .mkString("{\n", ",\n", "\n}")

      case MU.Prop(path) =>
        "\"" + path + "\""

      case MU.Constant(s: String) =>
        "\"" + s + "\""

      case MU.Constant(s: Any) =>
        s.toString // also limit

      case MU.ScalaCode(expr) =>
        expr match
          case '{ ${ x }: t } => RenderUtils.renderCaseClass[t](x)
          case _              => "?"

      case MU.UIterable(iterable)  => iterable.map(render).mkString("[", ",", "]")
      case MU.ScalaCodeIterable(_) => "[ ? ]"

      case _ => report.errorAndAbort(s"Wrong term: $query")
    }

  private def renderAddToSet(op: MU.MongoUpdateOp.AddToSet)(using Quotes): String =
    val renderOfValue = render(op.value)
    val finalRenderOfValue =
      if op.each then s"""{ "$$each" : $renderOfValue }"""
      else renderOfValue
    render(op.prop) + ": " + finalRenderOfValue

  private def renderOps(ops: List[String])(op: String) =
    ops match
      case Nil  => None
      case list => Some(s"\t \"$op\": { " + list.mkString(", ") + " }")

  def target(optRepr: MU)(using quotes: Quotes): Expr[BsonDocument] = {
    import quotes.reflect.*

    def targetOps(setters: List[MU.MongoUpdateOp]): List[Expr[(String, BsonValue)]] =
      setters.map { op =>
        val key       = op.prop.path
        val valueExpr = handleValues(op.value)
        val finalValueExpr = op match
          case addToSet: MongoUpdateOp.AddToSet =>
            if addToSet.each then '{ BsonDocument("$each" -> $valueExpr) }
            else valueExpr
          case _ => valueExpr
        '{ ${ Expr(key) } -> $finalValueExpr }
      }

    optRepr match {
      case MU.Update(ops) =>
        val tSetters      = targetOps(ops.collect { case s: MU.MongoUpdateOp.Set => s })
        val tUnsets       = targetOps(ops.collect { case s: MU.MongoUpdateOp.Unset => s })
        val tIncs         = targetOps(ops.collect { case s: MU.MongoUpdateOp.Inc => s })
        val tMaxs         = targetOps(ops.collect { case s: MU.MongoUpdateOp.Max => s })
        val tMins         = targetOps(ops.collect { case s: MU.MongoUpdateOp.Min => s })
        val tMuls         = targetOps(ops.collect { case s: MU.MongoUpdateOp.Mul => s })
        val tRenames      = targetOps(ops.collect { case s: MU.MongoUpdateOp.Rename => s })
        val tSetOnInserts = targetOps(ops.collect { case s: MU.MongoUpdateOp.SetOnInsert => s })
        val tAddToSets    = targetOps(ops.collect { case s: MU.MongoUpdateOp.AddToSet => s })
        val tPops         = targetOps(ops.collect { case s: MU.MongoUpdateOp.Pop => s })

        // format: off
        def updaterGroup(groupName: String, updaters: List[Expr[(String, BsonValue)]]): Option[Expr[(String, BsonDocument)]] =
          if (updaters.isEmpty)
            None
          else
            Some('{
              ${ Expr(groupName) } -> BsonDocument(${ Expr.ofList(updaters)} )
            })

        val updateList: List[Expr[(String, BsonDocument)]] = List(
          updaterGroup("$set", tSetters),
          updaterGroup("$unset", tUnsets),
          updaterGroup("$inc", tIncs),
          updaterGroup("$max", tMaxs),
          updaterGroup("$min", tMins),
          updaterGroup("$mul", tMuls),
          updaterGroup("$rename", tRenames),
          updaterGroup("$setOnInsert", tSetOnInserts),
          updaterGroup("$addToSet", tAddToSets),
          updaterGroup("$pop", tPops),
        ).flatten

        '{
          BsonDocument(
            ${ Expr.ofList(updateList) }
          )
        }
        //format: on
      case _ => report.errorAndAbort(s"Unexpected expr " + pprint(optRepr))
    }
  }

  def handleValues(expr: MongoUpdateNode)(using q: Quotes): Expr[BsonValue] =
    import q.reflect.*
    expr match {
      case MU.Constant(i: Long) =>
        '{ BsonInt64.apply(${ Expr(i: Long) }) }
      case MU.Constant(i: Int) =>
        '{ BsonInt32.apply(${ Expr(i: Int) }) }
      case MU.Constant(i: Short) =>
        '{ BsonInt32.apply(${ Expr(i: Short) }) }
      case MU.Constant(i: Byte) =>
        '{ BsonInt32.apply(${ Expr(i: Byte) }) }
      case MU.Constant(s: Double) =>
        '{ BsonDouble.apply(${ Expr(s: Double) }) }
      case MU.Constant(s: Float) =>
        '{ BsonDouble.apply(${ Expr(s: Float) }) }
      case MU.Constant(s: String) =>
        '{ BsonString.apply(${ Expr(s: String) }) }
      case MU.Constant(s: Char) =>
        '{ BsonString.apply(${ Expr((s: Char).toString) }) }
      case MU.Constant(b: Boolean) =>
        '{ BsonBoolean.apply(${ Expr(b: Boolean) }) }
      case MU.ScalaCode(code) => BsonUtils.extractLifted(code)
      case MU.UIterable(list) =>
        '{
          BsonArray.fromIterable(${
            Expr.ofList(list.map(handleValues))
          })
        }
      case MU.ScalaCodeIterable(exprList) => BsonUtils.extractLifted(exprList)
      case _                              => report.errorAndAbort(s"Given type is not literal constant")
    }
}
