package ru.tinkoff.oolong.bson.annotation

import scala.annotation.StaticAnnotation
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

final case class BsonDiscriminator(name: String, renameValues: String => String = identity[String])
    extends StaticAnnotation

object BsonDiscriminator:

  val ClassNameField = "className"

  private[oolong] def getAnnotations[A: Type](using quotes: Quotes): Expr[List[BsonDiscriminator]] = {
    import quotes.reflect.*

    val tepr = TypeRepr.of[A]

    val annotations = tepr.baseClasses
      .flatMap(_.annotations)
      .map(_.asExpr)
      .collect { case bd @ '{ new BsonDiscriminator($name, $fun) } =>
        bd
      }

    if annotations.length > 1 then
      report.error(s"Found more then one BsonDiscriminator annotation for ${tepr.typeSymbol.name}")

    Expr.ofList(annotations.toSeq)
  }

end BsonDiscriminator
