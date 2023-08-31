package ru.tinkoff.oolong.bson

import java.time.*

import magnolia1.*
import org.bson.BsonNull
import org.mongodb.scala.bson.*
import ru.tinkoff.oolong.bson.annotation.*
import ru.tinkoff.oolong.bson.meta.QueryMeta

/**
 * A type class that provides a conversion from a value of type `T` to a BsonValue
 */
trait BsonEncoder[T]:
  /*
   * Serialize given value to Bson
   */
  extension (value: T) def bson: BsonValue

  /*
   * Create a BsonEncoder that pre-processes value with a given function
   */
  def beforeWrite[U](f: U => T): BsonEncoder[U] =
    (u: U) => f(u).bson

object BsonEncoder {

  import scala.compiletime.*
  import scala.deriving.Mirror
  import scala.quoted.*

  def apply[T](using be: BsonEncoder[T]): BsonEncoder[T] = be

  /*
   * Create BsonEncoder that always returns given BsonValue
   */
  def constant[T](bv: BsonValue): BsonEncoder[T] =
    (_: T) => bv

  def summonAllForSum[T: Type](using Quotes): List[Expr[BsonEncoder[_]]] =
    import quotes.reflect.*
    Type.of[T] match
      case '[t *: tpes] =>
        Expr.summon[BsonEncoder[t]] match
          case Some(expr) => expr :: summonAllForSum[tpes]
          case _          => derivedImpl[t] :: summonAllForSum[tpes]
      case '[EmptyTuple] => Nil

  def summonAllForProduct[T: Type](using Quotes): List[Expr[BsonEncoder[_]]] =
    import quotes.reflect.*
    Type.of[T] match
      case '[t *: tpes] =>
        Expr.summon[BsonEncoder[t]] match
          case Some(expr) => expr :: summonAllForProduct[tpes]
          case _ =>
            report.errorAndAbort(s"No given instance of BsonEncoder[${TypeRepr.of[t].typeSymbol.name}] was found")
      case '[EmptyTuple] => Nil

  def productBody[T: Type](
      mirror: Expr[Mirror.ProductOf[T]],
      elemInstances: List[Expr[BsonEncoder[_]]]
  )(using q: Quotes): Expr[BsonEncoder[T]] =
    import q.reflect.*
    val names        = TypeRepr.of[T].typeSymbol.caseFields.map(_.name)
    val renamingMeta = Expr.summon[QueryMeta[T]]
    val map = renamingMeta match
      case Some(meta) =>
        meta.value
          .getOrElse(report.errorAndAbort(s"Please, add `inline` to given QueryMeta[${TypeRepr.of[T].typeSymbol.name}]"))
          .map
      case _ => Map.empty[String, String]
    '{
      new BsonEncoder[T] {
        extension (value: T)
          def bson: BsonValue = {
            val values = value.asInstanceOf[Product].productIterator.toList

            val tuples: List[(String, (BsonEncoder[_], Any))] =
              ${ Expr(names) } zip (${ Expr.ofList(elemInstances) } zip values)

            BsonDocument(
              tuples
                .map { case (name, (instance, value)) =>
                  ${ Expr(map) }.getOrElse(name, name) -> instance.asInstanceOf[BsonEncoder[Any]].bson(value)
                }
                .filterNot(_._2.isNull)
            )
          }
      }
    }
  end productBody

  def sumBody[T: Type](mirror: Expr[Mirror.SumOf[T]], elemInstances: List[Expr[BsonEncoder[_]]])(using
      q: Quotes
  ): Expr[BsonEncoder[T]] =
    val discriminator = BsonDiscriminator.getAnnotations[T]
    '{
      val mirrorV = ${ mirror }
      new BsonEncoder[T] {
        extension (value: T) {
          def bson: BsonValue = {
            val index = mirrorV.ordinal(value)
            val doc = ${ Expr.ofList(elemInstances) }
              .apply(index)
              .asInstanceOf[BsonEncoder[Any]]
              .bson(value)
              .asInstanceOf[BsonDocument]
            val (discriminatorField, modifyValue) = ${ discriminator }.headOption
              .map(b => (b.name, b.renameValues))
              .getOrElse(BsonDiscriminator.ClassNameField -> identity[String])
            doc.append(
              discriminatorField,
              BsonString(modifyValue(value.getClass.getSimpleName))
            )
          }
        }
      }
    }

  inline given derived[T]: BsonEncoder[T] = ${ derivedImpl[T] }

  def derivedImpl[T: Type](using q: Quotes): Expr[BsonEncoder[T]] =
    import quotes.reflect.*

    val ev: Expr[Mirror.Of[T]] = Expr.summon[Mirror.Of[T]].get

    ev match
      case '{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = elementTypes } } =>
        val elemInstances = summonAllForProduct[elementTypes]
        productBody[T](m, elemInstances)
      case '{ $m: Mirror.SumOf[T] { type MirroredElemTypes = elementTypes } } =>
        val elemInstances = summonAllForSum[elementTypes]
        sumBody[T](m, elemInstances)
  end derivedImpl
}
