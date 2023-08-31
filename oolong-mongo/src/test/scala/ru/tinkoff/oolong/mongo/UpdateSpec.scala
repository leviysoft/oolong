package ru.tinkoff.oolong.mongo

import java.time.LocalDate
import java.time.OffsetTime
import java.time.ZoneId
import java.time.ZoneOffset

import org.mongodb.scala.bson.BsonArray
import org.mongodb.scala.bson.BsonBoolean
import org.mongodb.scala.bson.BsonDateTime
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.bson.BsonInt32
import org.mongodb.scala.bson.BsonInt64
import org.mongodb.scala.bson.BsonString
import org.scalatest.funsuite.AnyFunSuite
import ru.tinkoff.oolong.bson.BsonEncoder
import ru.tinkoff.oolong.bson.given
import ru.tinkoff.oolong.dsl.*

class UpdateSpec extends AnyFunSuite {

  case class TestClass(
      intField: Int,
      stringField: String,
      dateField: LocalDate,
      innerClassField: InnerClass,
      optionField: Option[Long],
      optionInnerClassField: Option[InnerClass]
  )

  case class InnerClass(
      fieldOne: String,
      fieldTwo: Int,
      fieldThree: Option[Long] = None,
      fieldFour: LocalDate = LocalDate.now(),
      fieldFive: SecondInnerClass,
      fieldSix: List[String]
  ) derives BsonEncoder

  case class SecondInnerClass(fieldOne: Long = 3L) derives BsonEncoder

  test("$set for regular fields") {
    val q = update[TestClass](_.set(_.intField, 2))

    assert(q == BsonDocument("$set" -> BsonDocument("intField" -> BsonInt32(2))))
  }

  test("$set for Option[_] fields") {
    val q = update[TestClass](_.setOpt(_.optionField, 2L))

    assert(q == BsonDocument("$set" -> BsonDocument("optionField" -> BsonInt64(2))))
  }

  test("$set for Option[_] inner class fields") {
    val q = update[TestClass](
      _.set(
        _.optionInnerClassField.!!,
        lift(InnerClass("some", 2, fieldSix = List("1", "2"), fieldFive = SecondInnerClass()))
      )
    )

    assert(
      q == BsonDocument(
        "$set" ->
          BsonDocument(
            "optionInnerClassField" -> BsonDocument(
              "fieldOne"  -> BsonString("some"),
              "fieldTwo"  -> BsonInt32(2),
              "fieldFour" -> BsonDateTime(LocalDate.now().atStartOfDay(ZoneOffset.UTC).toInstant.toEpochMilli),
              "fieldFive" -> BsonDocument("fieldOne" -> BsonInt64(3)),
              "fieldSix"  -> BsonArray.fromIterable(List(BsonString("1"), BsonString("2"))),
            )
          )
      )
    )
  }

  test("$inc") {
    val q = update[TestClass](_.inc(_.intField, 1))

    assert(q == BsonDocument("$inc" -> BsonDocument("intField" -> BsonInt32(1))))
  }

  test("$mul") {
    val q = update[TestClass](_.mul(_.intField, 10))

    assert(q == BsonDocument("$mul" -> BsonDocument("intField" -> BsonInt32(10))))
  }

  test("$max") {
    val q = update[TestClass](_.max(_.intField, 10))

    assert(q == BsonDocument("$max" -> BsonDocument("intField" -> BsonInt32(10))))
  }

  test("$min") {
    val q = update[TestClass](_.min(_.intField, 10))

    assert(q == BsonDocument("$min" -> BsonDocument("intField" -> BsonInt32(10))))
  }

  test("$rename") {
    val q = update[TestClass](_.rename(_.intField, "newFieldName"))

    assert(q == BsonDocument("$rename" -> BsonDocument("intField" -> BsonString("newFieldName"))))
  }

  test("$unset") {
    val q = update[TestClass](_.unset(_.intField))

    assert(q == BsonDocument("$unset" -> BsonDocument("intField" -> BsonString(""))))
  }

  test("$setOnInsert") {
    val q = update[TestClass](_.setOnInsert(_.intField, 14))

    assert(q == BsonDocument("$setOnInsert" -> BsonDocument("intField" -> BsonInt32(14))))
  }

  test("several update operators combined") {
    val q = update[TestClass](
      _.unset(_.dateField)
        .setOpt(_.optionField, 2L)
        .set(_.intField, 19)
    )

    assert(
      q == BsonDocument(
        ("$set"   -> BsonDocument("optionField" -> BsonInt64(2L), "intField" -> BsonInt32(19))),
        ("$unset" -> BsonDocument("dateField" -> BsonString("")))
      )
    )
  }
}
