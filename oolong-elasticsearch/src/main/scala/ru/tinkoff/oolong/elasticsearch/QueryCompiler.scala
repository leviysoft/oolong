package ru.tinkoff.oolong.elasticsearch

import scala.quoted.*

import ru.tinkoff.oolong.*
import ru.tinkoff.oolong.dsl.*

/**
 * Compile a ES query.
 * @param input
 *   Scala code describing the query.
 */
inline def query[Doc](inline input: Doc => Boolean): JsonNode = ${ queryImpl('input) }

private[oolong] def queryImpl[Doc: Type](input: Expr[Doc => Boolean])(using quotes: Quotes): Expr[JsonNode] = {
  import quotes.reflect.*
  import ElasticQueryCompiler.*

  val parser = new DefaultAstParser

  val ast          = parser.parseQExpr(input)
  val optimizedAst = LogicalOptimizer.optimize(ast)

  val optRepr   = opt(optimizedAst)
  val optimized = optimize(optRepr)

  report.info("Optimized AST:\n" + pprint(optimizedAst) + "\nGenerated query:\n" + render(optimized))

  '{ JsonNode.obj("query" -> ${ target(optimized) }) }
}
