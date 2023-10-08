package oolong

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

private[oolong] trait Backend[Ast, OptimizableRepr, TargetRepr] {

  /**
   * Translate AST into a form that allows us to do backend optimizations.
   */
  def opt[Doc: Type](ast: Ast)(using quotes: Quotes): OptimizableRepr

  /**
   * Render the final optimized version of a query. Output will be displayed as a compilation message.
   */
  def render(query: OptimizableRepr)(using quotes: Quotes): String

  /**
   * Translate an optimized query into the target represantion; also lift the result into Expr[].
   */
  def target(optimized: OptimizableRepr)(using quotes: Quotes): Expr[TargetRepr]

  /**
   * Perform optimizations that are specific to this backend.
   */
  def optimize(query: OptimizableRepr)(using quotes: Quotes): OptimizableRepr = query
}
