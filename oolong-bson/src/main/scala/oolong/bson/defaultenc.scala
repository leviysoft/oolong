package oolong.bson

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.Year
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.util.UUID
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

import org.mongodb.scala.bson.*

given BsonEncoder[BsonValue] with
  extension (value: BsonValue) def bson: BsonValue = value

given BsonEncoder[BsonObjectId] with
  extension (value: BsonObjectId) def bson: BsonValue = value

given BsonEncoder[Boolean] with
  extension (value: Boolean) def bson: BsonValue = BsonBoolean(value)

given BsonEncoder[Int] with
  extension (value: Int) def bson: BsonValue = BsonInt32(value)

given BsonEncoder[Long] with
  extension (value: Long) def bson: BsonValue = BsonInt64(value)

given BsonEncoder[Double] with
  extension (value: Double) def bson: BsonValue = BsonDouble(value)

given BsonEncoder[BigDecimal] with
  extension (value: BigDecimal) def bson: BsonValue = BsonDecimal128(value)

given BsonEncoder[String] with
  extension (value: String) def bson: BsonValue = BsonString(value)

given BsonEncoder[Instant] with
  extension (value: Instant) def bson: BsonValue = BsonDateTime(value.toEpochMilli)

given BsonEncoder[ZonedDateTime] = BsonEncoder[Instant].beforeWrite(_.toInstant)

given BsonEncoder[LocalDate] = BsonEncoder[ZonedDateTime].beforeWrite(_.atStartOfDay(ZoneOffset.UTC))

given BsonEncoder[LocalDateTime] = BsonEncoder[ZonedDateTime].beforeWrite(_.atZone(ZoneOffset.UTC))

given BsonEncoder[Year] with
  extension (value: Year) def bson: BsonValue = BsonInt32(value.getValue)

given BsonEncoder[UUID] with
  extension (value: UUID) def bson: BsonValue = BsonString(value.toString)

given [T](using BsonEncoder[T]): BsonEncoder[Option[T]] with
  extension (value: Option[T]) def bson: BsonValue = value.fold[BsonValue](BsonNull())(_.bson)

given [T](using BsonEncoder[T]): BsonEncoder[Seq[T]] with
  extension (value: Seq[T]) def bson: BsonValue = BsonArray.fromIterable(value.map(_.bson))

given [T](using BsonEncoder[T]): BsonEncoder[List[T]] with
  extension (value: List[T]) def bson: BsonValue = BsonArray.fromIterable(value.map(_.bson))

given [T](using BsonEncoder[T]): BsonEncoder[Vector[T]] with
  extension (value: Vector[T]) def bson: BsonValue = BsonArray.fromIterable(value.map(_.bson))

given [T](using BsonEncoder[T]): BsonEncoder[Set[T]] with
  extension (value: Set[T]) def bson: BsonValue = BsonArray.fromIterable(value.map(_.bson))

given StrMapEncoder[T](using BsonEncoder[T]): BsonEncoder[Map[String, T]] with
  extension (value: Map[String, T]) def bson: BsonValue = BsonDocument(value.view.mapValues(_.bson))

given [K, V](using BsonKeyEncoder[K], BsonEncoder[V]): BsonEncoder[Map[K, V]] with
  extension (value: Map[K, V])
    def bson: BsonValue =
      BsonDocument(value.map { case (key, value) =>
        BsonKeyEncoder[K].encode(key) -> value.bson
      })

given [L, R](using BsonEncoder[L], BsonEncoder[R]): BsonEncoder[L Either R] with
  extension (value: L Either R) def bson: BsonValue = value.map(_.bson).left.map(_.bson).merge

given [A, B](using BsonEncoder[A], BsonEncoder[B]): BsonEncoder[(A, B)] with
  extension (value: (A, B)) def bson: BsonValue = BsonArray(value._1.bson, value._2.bson)

given BsonEncoder[FiniteDuration] with
  extension (value: FiniteDuration) def bson: BsonValue = BsonString(value.toString())

given BsonEncoder[Array[Byte]] with
  extension (value: Array[Byte]) def bson: BsonValue = BsonBinary(value)

given BsonEncoder[Regex] with
  extension (value: Regex) def bson: BsonValue = BsonRegularExpression(value)
