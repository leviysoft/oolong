package oolong.mongo

import scala.quoted.*

import oolong.dsl.Updater

inline def renderUpdate[Doc](inline input: Updater[Doc] => Updater[Doc]): String =
  ${ renderUpdateImpl('input) }

def renderUpdateImpl[Doc: Type](
    input: Expr[Updater[Doc] => Updater[Doc]]
)(using quotes: Quotes) =
  import MongoUpdateCompiler.*
  val (_, query) = buildUpdateAst(input)
  val result     = render(query)
  Expr(result)

inline def renderQuery[Doc](inline input: Doc => Boolean): String =
  ${ renderQueryImpl('input) }

def renderQueryImpl[Doc: Type](
    input: Expr[Doc => Boolean]
)(using quotes: Quotes) =
  import MongoQueryCompiler.*
  val (_, query) = buildQueryAst(input)
  val result     = render(query)
  Expr(result)
