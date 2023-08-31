package oolong.bson

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.Year
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.immutable
import scala.collection.immutable.Map
import scala.collection.mutable
import scala.collection.mutable.Builder
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*
import scala.util.Success
import scala.util.Try
import scala.util.matching.Regex

import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.bson.BsonNull
import org.mongodb.scala.bson.BsonObjectId
import org.mongodb.scala.bson.BsonValue

given BsonDecoder[BsonValue] with
  def fromBson(value: BsonValue): Try[BsonValue] = Success(value)

given BsonDecoder[BsonObjectId] with
  def fromBson(value: BsonValue): Try[BsonObjectId] = Try(value.asObjectId)

given BsonDecoder[Boolean] with
  def fromBson(value: BsonValue): Try[Boolean] = Try(value.asBoolean.getValue)

given BsonDecoder[Int] with
  def fromBson(value: BsonValue): Try[Int] = Try(value.asInt32.getValue)

given BsonDecoder[Long] with
  def fromBson(value: BsonValue): Try[Long] = Try(value.asInt64.getValue)

given BsonDecoder[Double] with
  def fromBson(value: BsonValue): Try[Double] = Try(value.asDouble.getValue)

given BsonDecoder[BigDecimal] with
  def fromBson(value: BsonValue): Try[BigDecimal] = Try(value.asDecimal128.getValue.bigDecimalValue)

given BsonDecoder[String] with
  def fromBson(value: BsonValue): Try[String] = Try(value.asString.getValue)

given BsonDecoder[Instant] with
  def fromBson(value: BsonValue): Try[Instant] = Try(Instant.ofEpochMilli(value.asDateTime.getValue))

given BsonDecoder[ZonedDateTime] = BsonDecoder[Instant].afterRead(_.atZone(ZoneOffset.UTC))

given BsonDecoder[LocalDate] = BsonDecoder[ZonedDateTime].afterRead(_.toLocalDate())

given BsonDecoder[LocalDateTime] = BsonDecoder[ZonedDateTime].afterRead(_.toLocalDateTime())

given BsonDecoder[Year] with
  def fromBson(value: BsonValue): Try[Year] = Try(Year.of(value.asInt32.getValue))

given BsonDecoder[UUID] with
  def fromBson(value: BsonValue): Try[UUID] = Try(UUID.fromString(value.asString.getValue))

given [T](using BsonDecoder[T]): BsonDecoder[Option[T]] with
  def fromBson(value: BsonValue): Try[Option[T]] = value match {
    case BNull() => Success(None)
    case bv      => BsonDecoder[T].fromBson(bv).map(Some(_))
  }

protected def buildBsonDecoder[C[_], T: BsonDecoder](builder: => mutable.Builder[T, C[T]]): BsonDecoder[C[T]] =
  BsonDecoder.ofArray(
    _.asScala
      .foldLeft[Try[mutable.Builder[T, C[T]]]](Success(builder))((seq, bv) =>
        seq.flatMap(sqb => BsonDecoder[T].fromBson(bv).map(sqb += _))
      )
      .map(_.result())
  )

given [T](using BsonDecoder[T]): BsonDecoder[Seq[T]] = buildBsonDecoder(Seq.newBuilder[T])

given [T](using BsonDecoder[T]): BsonDecoder[List[T]] = buildBsonDecoder(List.newBuilder[T])

given [T](using BsonDecoder[T]): BsonDecoder[Vector[T]] = buildBsonDecoder(Vector.newBuilder[T])

given [T](using BsonDecoder[T]): BsonDecoder[Set[T]] = buildBsonDecoder(Set.newBuilder[T])

given [T](using BsonDecoder[T]): BsonDecoder[Map[String, T]] =
  BsonDecoder.ofDocument { doc =>
    Try {
      val builder = immutable.Map.newBuilder[String, T]

      for ((k, v) <- doc.asScala)
        builder += k -> BsonDecoder[T].fromBson(v).get

      builder.result()
    }
  }

given [K, V](using BsonKeyDecoder[K], BsonDecoder[V]): BsonDecoder[Map[K, V]] =
  BsonDecoder.ofDocument { doc =>
    Try {
      val builder = immutable.Map.newBuilder[K, V]

      for ((key, value) <- doc.asScala) {
        val k = BsonKeyDecoder[K].decode(key).get
        val v = BsonDecoder[V].fromBson(value).get
        builder += k -> v
      }

      builder.result()
    }
  }

given [L, R](using BsonDecoder[L], BsonDecoder[R]): BsonDecoder[L Either R] with
  def fromBson(value: BsonValue): Try[L Either R] =
    BsonDecoder[L].fromBson(value).map(Left(_)).orElse(BsonDecoder[R].fromBson(value).map(Right(_)))

given [A, B](using BsonDecoder[A], BsonDecoder[B]): BsonDecoder[(A, B)] =
  BsonDecoder.ofArray { arr =>
    for {
      a <- Try(arr.get(0)).flatMap(BsonDecoder[A].fromBson)
      b <- Try(arr.get(1)).flatMap(BsonDecoder[B].fromBson)
    } yield (a, b)
  }

given BsonDecoder[FiniteDuration] with
  def fromBson(value: BsonValue): Try[FiniteDuration] =
    Try(value.asString().getValue).flatMap(str =>
      Try {
        val d = Duration(str)
        FiniteDuration(d.length, d.unit)
      }
    )

given BsonDecoder[Array[Byte]] with
  def fromBson(value: BsonValue): Try[Array[Byte]] =
    Try(value.asBinary().getData)

given BsonDecoder[Regex] with
  def fromBson(value: BsonValue): Try[Regex] =
    Try(value.asRegularExpression()).map(bre => new Regex(bre.getPattern))
