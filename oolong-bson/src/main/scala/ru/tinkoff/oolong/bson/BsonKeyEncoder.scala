package ru.tinkoff.oolong.bson

/*
 * A type class providing a way to use a value of type `T` as Map key during writing bson
 */
trait BsonKeyEncoder[T]:
  def encode(t: T): String

object BsonKeyEncoder:
  def apply[T](using bke: BsonKeyEncoder[T]) = bke
