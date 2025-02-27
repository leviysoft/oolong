package oolong.bson

import scala.util.Try

/*
 * A type class providing a way to use a value of type `T` as Map key during reading bson
 */
trait BsonKeyDecoder[T]:
  def decode(value: String): Try[T]

  def emapTry[H](f: T => Try[H]): BsonKeyDecoder[H] =
    (value: String) => this.decode(value).flatMap(f)

object BsonKeyDecoder:
  def apply[T](using bkd: BsonKeyDecoder[T]) = bkd
