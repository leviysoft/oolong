package oolong.bson

/*
 * A type class providing a way to use a value of type `T` as Map key during writing bson
 */
trait BsonKeyEncoder[T]:
  def encode(t: T): String

  def beforeWrite[H](f: H => T): BsonKeyEncoder[H] =
    (value: H) => this.encode(f(value))

object BsonKeyEncoder:
  def apply[T](using bke: BsonKeyEncoder[T]) = bke
