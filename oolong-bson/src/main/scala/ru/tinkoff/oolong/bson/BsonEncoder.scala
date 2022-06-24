package ru.tinkoff.oolong.bson

import magnolia1.*
import org.bson.BsonNull
import org.mongodb.scala.bson.*

import ru.tinkoff.oolong.bson.annotation.*

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

object BsonEncoder extends Derivation[BsonEncoder] {

  import scala.compiletime.*
  import scala.deriving.Mirror
  import scala.quoted.*

  def apply[T](using be: BsonEncoder[T]): BsonEncoder[T] = be

  /*
   * Create BsonEncoder that always returns given BsonValue
   */
  def constant[T](bv: BsonValue): BsonEncoder[T] =
    (_: T) => bv

  override def join[T](caseClass: CaseClass[BsonEncoder, T]): BsonEncoder[T] =
    (value: T) =>
      BsonDocument(
        caseClass.params
          .map { p =>
            val fieldName =
              if (p.annotations.isEmpty) p.label
              else p.annotations.collectFirst { case BsonKey(value) => value }.getOrElse(p.label)

            given tEnc: Typeclass[p.PType] = p.typeclass

            fieldName -> p.deref(value).bson
          }
          .filterNot(_._2.isNull)
      )

  override def split[T](sealedTrait: SealedTrait[BsonEncoder, T]): BsonEncoder[T] = (value: T) => {
    val (discriminatorField, renameFun) =
      if (sealedTrait.annotations.isEmpty) BsonDiscriminator.ClassNameField -> identity[String] _
      else
        sealedTrait.annotations
          .collectFirst { case BsonDiscriminator(d, rename) => d -> rename }
          .getOrElse(BsonDiscriminator.ClassNameField -> identity[String] _)

    sealedTrait.choose(value) { st =>
      implicit val tEnc = st.typeclass

      st.cast(value).bson match {
        case BDocument(fields) =>
          BsonDocument(fields + (discriminatorField -> BsonString(renameFun(st.typeInfo.short))))
        case other => other
      }
    }
  }
}
