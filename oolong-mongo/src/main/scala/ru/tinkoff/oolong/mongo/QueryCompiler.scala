package ru.tinkoff.oolong.mongo

import scala.quoted.*

import org.mongodb.scala.bson.BsonDocument

import ru.tinkoff.oolong.*
import ru.tinkoff.oolong.dsl.*

/**
 * Compile a BSON description of the update.
 * @param input
 *   Description of the update written in oolong DSL.
 */
inline def compileUpdate(inline input: UpdateDslNode[_]): BsonDocument = ${ compileUpdateImpl('input) }

/**
 * Compile a BSON query.
 * @param input
 *   Scala code describing the query.
 */
inline def query[Doc](inline input: Doc => Boolean): BsonDocument = ${ queryImpl('input) }

private[oolong] def compileUpdateImpl(input: Expr[UpdateDslNode[_]])(using quotes: Quotes): Expr[BsonDocument] = {
  import quotes.reflect.*
  import MongoUpdateCompiler.*

  val ast = DefaultAstParser.parseUExpr(input)

  val optRepr   = opt(ast)
  val optimized = optimize(optRepr)

  report.info("AST:\n" + pprint(ast) + "\nGenerated Mongo query:\n" + render(optimized))

  target(optimized)
}

private[oolong] def queryImpl[Doc: Type](input: Expr[Doc => Boolean])(using quotes: Quotes): Expr[BsonDocument] = {
  import quotes.reflect.*
  import MongoQueryCompiler.*

  val ast          = DefaultAstParser.parseQExpr(input)
  val optimizedAst = LogicalOptimizer.optimize(ast)

  val optRepr   = opt(optimizedAst)
  val optimized = optimize(optRepr)

  report.info("Optimized AST:\n" + pprint(optimizedAst) + "\nGenerated Mongo query:\n" + render(optimized))

  target(optimized)
}
