package oolong.mongo

import scala.quoted.*

import oolong.dsl.Updater

inline def renderUpdate[Doc](inline input: Updater[Doc] => Updater[Doc]): String =
  ${ renderUpdateImpl('input) }

private def renderUpdateImpl[Doc: Type](
    input: Expr[Updater[Doc] => Updater[Doc]]
)(using quotes: Quotes) =
  import MongoUpdateCompiler.*
  val (_, query) = buildUpdateAst(input)
  val result     = render(query)
  Expr(result)

inline def renderQuery[Doc](inline input: Doc => Boolean): String =
  ${ renderQueryImpl('input) }

private def renderQueryImpl[Doc: Type](
    input: Expr[Doc => Boolean]
)(using quotes: Quotes) =
  import MongoQueryCompiler.*
  val (_, query) = buildQueryAst(input)
  val result     = render(query)
  Expr(result)

inline def renderProjection[Doc, Projection]: String = ${ renderProjectionImpl[Doc, Projection] }

private def renderProjectionImpl[Doc: Type, Projection: Type](using quotes: Quotes): Expr[String] =
  import MongoQueryCompiler.*
  val (_, query) = buildProjectionAst[Doc, Projection]
  val result     = render(query)
  Expr(result)
