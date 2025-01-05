package oolong.bson

import org.mongodb.scala.bson.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BsonMergeSpec extends AnyFunSuite with Matchers {
  test(":+ document merge") {
    val doc1 = BsonDocument("field1" -> 10, "field2" -> 30)
    val doc2 = BsonDocument("field1" -> 20, "field3" -> 40)

    val res = doc1 :+ doc2

    res shouldBe BsonDocument("field1" -> 10, "field2" -> 30, "field3" -> 40)
  }

  test("+: document merge") {
    val doc1 = BsonDocument("field1" -> 10, "field2" -> 30)
    val doc2 = BsonDocument("field1" -> 20, "field3" -> 40)

    val res = doc1 +: doc2

    res shouldBe BsonDocument("field1" -> 20, "field2" -> 30, "field3" -> 40)
  }
}
