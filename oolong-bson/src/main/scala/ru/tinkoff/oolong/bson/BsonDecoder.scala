package ru.tinkoff.oolong.bson

import scala.util.*

import magnolia1.*
import org.bson.BsonInvalidOperationException
import org.bson.BsonNull
import org.mongodb.scala.bson.*

import ru.tinkoff.oolong.bson.annotation.*

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

object BsonDecoder extends Derivation[BsonDecoder] {

  import scala.compiletime.*
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
          bv => throw new BsonInvalidOperationException(s"Can't decode $bv")
        )
      )

  override def join[T](caseClass: CaseClass[BsonDecoder, T]): BsonDecoder[T] =
    BsonDecoder.ofDocument { doc =>
      caseClass.constructMonadic { f =>
        val fieldName =
          if (f.annotations.isEmpty) f.label
          else f.annotations.collectFirst { case BsonKey(value) => value }.getOrElse(f.label)

        f.typeclass.fromBson(doc.getFieldOpt(fieldName).getOrElse(BsonNull())) match {
          case Failure(_) if f.default.isDefined => Success(f.default.get)
          case Failure(exc) =>
            Failure(DeserializationError(s"Unable to decode field ${f.label}", exc))
          case otherwise => otherwise
        }
      }
    }

  override def split[T](sealedTrait: SealedTrait[BsonDecoder, T]): BsonDecoder[T] =
    BsonDecoder.ofDocument { doc =>
      val (discriminatorField, renameFun) =
        if (sealedTrait.annotations.isEmpty) BsonDiscriminator.ClassNameField -> identity[String] _
        else
          sealedTrait.annotations
            .collectFirst { case BsonDiscriminator(d, rename) => d -> rename }
            .getOrElse(BsonDiscriminator.ClassNameField -> identity[String] _)

      for {
        discriminator <- doc
          .getFieldOpt(discriminatorField)
          .toRight(
            DeserializationError(
              s"No discriminator field ($discriminatorField) found while decoding ${sealedTrait.typeInfo.short}"
            )
          )
          .toTry
        typeName <- BsonDecoder[String].fromBson(discriminator)
        decoder <- sealedTrait.subtypes
          .find(st => renameFun(st.typeInfo.short) == typeName)
          .toRight(DeserializationError(s"No case $typeName in ${sealedTrait.typeInfo.short}"))
          .toTry
        instance = decoder.typeclass
        result <- instance.fromBson(doc)
      } yield result
    }
}
