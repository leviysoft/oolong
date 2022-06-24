package ru.tinkoff.oolong.mongo

import ru.tinkoff.oolong.bson.*
import ru.tinkoff.oolong.bson.given

case class TestClass(
    field1: String,
    field2: Int,
    field3: InnerClass,
    field4: List[Int],
) derives BsonEncoder,
      BsonDecoder

case class InnerClass(
    innerField: String
) derives BsonEncoder,
      BsonDecoder
