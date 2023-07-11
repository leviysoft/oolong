package ru.tinkoff.oolong

private[oolong] trait TypeInfo[T] {
  type Type = T
  implicit val quotedType: quoted.Type[T]
}
