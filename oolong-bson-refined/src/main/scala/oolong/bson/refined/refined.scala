package oolong.bson.refined

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import eu.timepit.refined.api.RefType
import eu.timepit.refined.api.Validate
import oolong.bson.BsonDecoder
import oolong.bson.BsonEncoder
import oolong.bson.BsonKeyDecoder
import oolong.bson.BsonKeyEncoder
import oolong.bson.DeserializationError
import org.mongodb.scala.bson.BsonValue

given [T, P, F[_, _]](using BsonDecoder[T], Validate[T, P], RefType[F]): BsonDecoder[F[T, P]] with
  def fromBson(value: BsonValue): Try[F[T, P]] =
    BsonDecoder[T].fromBson(value) match {
      case Success(t0) =>
        RefType[F].refine(t0) match {
          case Left(err) => Failure(DeserializationError(err))
          case Right(t)  => Success(t)
        }
      case Failure(exc) => Failure(exc)
    }

given [T, P, F[_, _]](using BsonEncoder[T], Validate[T, P], RefType[F]): BsonEncoder[F[T, P]] with
  extension (value: F[T, P]) def bson: BsonValue = RefType[F].unwrap(value).bson

given [T, P, F[_, _]](using BsonKeyDecoder[T], Validate[T, P], RefType[F]): BsonKeyDecoder[F[T, P]] with
  def decode(value: String): Try[F[T, P]] =
    BsonKeyDecoder[T].decode(value) match {
      case Success(t0) =>
        RefType[F].refine(t0) match {
          case Left(err) => Failure(DeserializationError(err))
          case Right(t)  => Success(t)
        }
      case Failure(exc) => Failure(exc)
    }

given [T, P, F[_, _]](using BsonKeyEncoder[T], Validate[T, P], RefType[F]): BsonKeyEncoder[F[T, P]] with
  def encode(t: F[T, P]): String = BsonKeyEncoder[T].encode(RefType[F].unwrap(t))
