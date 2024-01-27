package oolong.mongo

import oolong.bson.*
import oolong.bson.given

case class TestClass(
    field1: String,
    field2: Int,
    field3: InnerClass,
    field4: List[Int],
    field5: Option[Long],
    field6: List[InnerClass]
) derives BsonEncoder,
      BsonDecoder

case class InnerClass(
    innerField: String,
    otherField: Double
) derives BsonEncoder,
      BsonDecoder
