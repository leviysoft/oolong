package oolong.mongo

import scala.quoted.*

import oolong.*
import oolong.dsl.*
import org.mongodb.scala.bson.BsonDocument

/**
 * Compile a BSON description of the update.
 *
 * @param input
 *   Description of the update written in oolong DSL.
 */
inline def update[Doc](inline input: Updater[Doc] => Updater[Doc]): BsonDocument = ${ updateImpl('input) }

/**
 * Compile a BSON query.
 * @param input
 *   Scala code describing the query.
 */
inline def query[Doc](inline input: Doc => Boolean): BsonDocument = ${ queryImpl('input) }

inline def projection[Doc, Projection]: BsonDocument = ${ projectionImpl[Doc, Projection] }

private[oolong] def updateImpl[Doc: Type](
    input: Expr[Updater[Doc] => Updater[Doc]]
)(using quotes: Quotes): Expr[BsonDocument] = {
  import quotes.reflect.*
  import MongoUpdateCompiler.*

  val (ast, optimized) = buildUpdateAst(input)

  report.info("AST:\n" + pprint(ast) + "\nGenerated Mongo query:\n" + render(optimized))

  target(optimized)
}

private[oolong] def buildUpdateAst[Doc: Type](input: Expr[Updater[Doc] => Updater[Doc]])(using
    quotes: Quotes
): (UExpr, MongoUpdateNode) =
  import MongoUpdateCompiler.*

  val parser = new DefaultAstParser

  val ast = parser.parseUExpr(input)

  val optRepr = opt(ast)
  ast -> optimize(optRepr)

private[oolong] def buildQueryAst[Doc: Type](input: Expr[Doc => Boolean])(using quotes: Quotes): (QExpr, MongoQueryNode) =
  import MongoQueryCompiler.*
  val parser = new DefaultAstParser

  val ast          = parser.parseQExpr(input)
  val optimizedAst = LogicalOptimizer.optimize(ast)

  val optRepr = opt(optimizedAst)
  optimizedAst -> optimize(optRepr)

private[oolong] def queryImpl[Doc: Type](input: Expr[Doc => Boolean])(using quotes: Quotes): Expr[BsonDocument] = {
  import quotes.reflect.*
  import MongoQueryCompiler.*

  val (optimizedAst, optimized) = buildQueryAst(input)

  report.info("Optimized AST:\n" + pprint(optimizedAst) + "\nGenerated Mongo query:\n" + render(optimized))

  target(optimized)
}

private[oolong] def buildProjectionAst[Doc: Type, Projection: Type](using quotes: Quotes) =
  import MongoQueryCompiler.*

  val parser = new DefaultAstParser
  val ast    = parser.parseProjectionQExpr[Doc, Projection]
  ast -> opt[Doc](ast)

private[oolong] def projectionImpl[Doc: Type, Projection: Type](using quotes: Quotes): Expr[BsonDocument] =
  import quotes.reflect.*
  import MongoQueryCompiler.*

  val (ast, repr) = buildProjectionAst[Doc, Projection]

  report.info("Optimized AST:\n" + pprint(ast) + "\nGenerated Mongo query:\n" + render(repr))

  target(repr)
