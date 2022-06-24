package ru.tinkoff.oolong.bson.annotation

import scala.annotation.StaticAnnotation

final case class BsonDiscriminator(name: String, renameValues: String => String = identity[String])
    extends StaticAnnotation

object BsonDiscriminator:
  val ClassNameField = "className"
