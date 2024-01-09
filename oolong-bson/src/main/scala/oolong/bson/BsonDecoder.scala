package oolong.bson

import scala.util.*
import scala.util.Try

import magnolia1.*
import oolong.bson.annotation.*
import oolong.bson.meta.QueryMeta
import org.bson.BsonNull
import org.mongodb.scala.bson.*

/*
 * A type class that provides a way to produce a value of type `T` from a BsonValue
 */
trait BsonDecoder[T]:
  /*
   * Decode given BsonValue
   */
  def fromBson(value: BsonValue): Try[T]

  /*
   * Create a BsonDecoder that post-processes value with a given function
   */
  def afterRead[U](f: T => U): BsonDecoder[U] =
    (value: BsonValue) => fromBson(value).map(f)

  /*
   * Create a BsonDecoder that post-processes value with a given function
   */
  def afterReadTry[U](f: T => Try[U]): BsonDecoder[U] =
    (value: BsonValue) => fromBson(value).flatMap(f)

object BsonDecoder {

  import scala.deriving.Mirror
  import scala.quoted.*

  def apply[T](using bd: BsonDecoder[T]): BsonDecoder[T] = bd

  /*
   * Create a BsonDecoder from a given function
   */
  def ofDocument[T](f: BsonDocument => Try[T]): BsonDecoder[T] =
    (value: BsonValue) => Try(value.asDocument()).flatMap(f)

  /*
   * Create a BsonDecoder from a given function
   */
  def ofArray[T](f: BsonArray => Try[T]): BsonDecoder[T] =
    (value: BsonValue) => Try(value.asArray()).flatMap(f)

  /*
   * Create a BsonDecoder from a given partial function
   */
  def partial[T](pf: PartialFunction[BsonValue, T]): BsonDecoder[T] =
    (value: BsonValue) =>
      Try(
        pf.applyOrElse[BsonValue, T](
          value,
          bv => throw DeserializationError(s"Can't decode $bv")
        )
      )

  def summonAllForSum[T: Type](using Quotes): List[Expr[BsonDecoder[_]]] =
    import quotes.reflect.*
    Type.of[T] match
      case '[t *: tpes] =>
        Expr.summon[BsonDecoder[t]] match
          case Some(expr) => expr :: summonAllForSum[tpes]
          case _          => derivedImpl[t] :: summonAllForSum[tpes]
      case '[EmptyTuple] => Nil

  def summonAllForProduct[T: Type](using Quotes): List[Expr[BsonDecoder[_]]] =
    import quotes.reflect.*
    Type.of[T] match
      case '[t *: tpes] =>
        Expr.summon[BsonDecoder[t]] match
          case Some(expr) => expr :: summonAllForProduct[tpes]
          case _ =>
            report.errorAndAbort(s"No given instance of BsonDecoder[${TypeRepr.of[t].typeSymbol.name}] was found")
      case '[EmptyTuple] => Nil

  def toProduct[T: Type](
      mirror: Expr[Mirror.ProductOf[T]],
      elemInstances: List[Expr[BsonDecoder[_]]]
  )(using q: Quotes): Expr[BsonDecoder[T]] =
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
      new BsonDecoder[T] {
        def fromBson(value: BsonValue): scala.util.Try[T] = {
          val m = ${ mirror }
          for {
            newValues <- ${ Expr.ofList(elemInstances) }
              .zip(${ Expr(names) })
              .partitionMap { case (instance, name) =>
                for {
                  value <- Option(value.asDocument.getFieldOpt(${ Expr(map) }.getOrElse(name, name)).getOrElse(BsonNull()))
                    .toRight(DeserializationError(s"Not found value $name"))
                  defaults = magnolia1.Macro.defaultValue[T]
                  result <- instance.asInstanceOf[BsonDecoder[Any]].fromBson(value).toEither match {
                    case Left(exc) =>
                      defaults
                        .find(f => f._1 == name && f._2.isDefined)
                        .flatMap(_._2)
                        .map(_())
                        .toRight(DeserializationError(s"Unable to decode field ${name}", exc))
                    case otherwise => otherwise
                  }
                } yield result.asInstanceOf[AnyRef]
              } match {
              case (Nil, rights)   => Success(rights)
              case (first :: _, _) => Failure(first)
            }
            result <- scala.util.Try(m.fromProduct(Tuple.fromArray(newValues.toArray)))
          } yield result
        }
      }
    }

  def toSum[T: Type](mirror: Expr[Mirror.SumOf[T]], elemInstances: List[Expr[BsonDecoder[_]]])(using
      q: Quotes
  ): Expr[BsonDecoder[T]] =
    import q.reflect.*
    val discriminator = BsonDiscriminator.getAnnotations[T]
    val names         = TypeRepr.of[T].typeSymbol.children.map(_.name).zipWithIndex
    '{
      new BsonDecoder[T] {
        def fromBson(value: BsonValue): scala.util.Try[T] = {
          val (discriminatorField, modifyValue) = ${ discriminator }.headOption
            .map(b => (b.name, b.renameValues))
            .getOrElse(BsonDiscriminator.ClassNameField -> identity[String])

          val result = for {
            descriminatorFromBsonValue <- Option(value.asDocument.get(discriminatorField))
              .map(_.asString)
              .toRight(DeserializationError(s"Not found discriminator field $discriminatorField"))
              .map(_.getValue)
            index <- ${ Expr(names) }
              .collectFirst {
                case (className, id) if descriminatorFromBsonValue == modifyValue(className) => id
              }
              .toRight(
                DeserializationError(
                  s"$descriminatorFromBsonValue does not match any of ${${ Expr(names) }.map(v => modifyValue(v._1)).mkString("[", ", ", "]")}"
                )
              )
            result <- ${ Expr.ofList(elemInstances) }
              .apply(index)
              .asInstanceOf[BsonDecoder[Any]]
              .fromBson(value)
              .map(_.asInstanceOf[T])
              .toEither
          } yield result
          result.toTry
        }
      }
    }

  inline given derived[T]: BsonDecoder[T] = ${ derivedImpl[T] }

  def derivedImpl[T: Type](using q: Quotes): Expr[BsonDecoder[T]] =
    import quotes.reflect.*

    val ev: Expr[Mirror.Of[T]] = Expr.summon[Mirror.Of[T]].get

    ev match
      case '{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = elementTypes } } =>
        val elemInstances = summonAllForProduct[elementTypes]
        toProduct[T](m, elemInstances)
      case '{ $m: Mirror.SumOf[T] { type MirroredElemTypes = elementTypes } } =>
        val elemInstances = summonAllForSum[elementTypes]
        toSum[T](m, elemInstances)
}
