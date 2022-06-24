package ru.tinkoff.oolong.mongo

import java.time.LocalDate
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
      fieldTwo: Int
  ) derives BsonEncoder

  test("$set for regular fields") {
    val q = compileUpdate {
      update[TestClass]
        .set(_.intField, 2)
    }

    assert(q == BsonDocument("$set" -> BsonDocument("intField" -> BsonInt32(2))))
  }

  test("$set for Option[_] fields") {
    val q = compileUpdate {
      update[TestClass]
        .setOpt(_.optionField, 2L)
    }

    assert(q == BsonDocument("$set" -> BsonDocument("optionField" -> BsonInt64(2))))
  }

  test("$inc") {
    val q = compileUpdate {
      update[TestClass]
        .inc(_.intField, 1)
    }

    assert(q == BsonDocument("$inc" -> BsonDocument("intField" -> BsonInt32(1))))
  }

  test("$mul") {
    val q = compileUpdate {
      update[TestClass]
        .mul(_.intField, 10)
    }

    assert(q == BsonDocument("$mul" -> BsonDocument("intField" -> BsonInt32(10))))
  }

  test("$max") {
    val q = compileUpdate {
      update[TestClass]
        .max(_.intField, 10)
    }

    assert(q == BsonDocument("$max" -> BsonDocument("intField" -> BsonInt32(10))))
  }

  test("$min") {
    val q = compileUpdate {
      update[TestClass]
        .min(_.intField, 10)
    }

    assert(q == BsonDocument("$min" -> BsonDocument("intField" -> BsonInt32(10))))
  }

  test("$rename") {
    val q = compileUpdate {
      update[TestClass]
        .rename(_.intField, "newFieldName")
    }

    assert(q == BsonDocument("$rename" -> BsonDocument("intField" -> BsonString("newFieldName"))))
  }

  test("$unset") {
    val q = compileUpdate {
      update[TestClass]
        .unset(_.intField)
    }

    assert(q == BsonDocument("$unset" -> BsonDocument("intField" -> BsonString(""))))
  }

  test("$setOnInsert") {
    val q = compileUpdate {
      update[TestClass]
        .setOnInsert(_.intField, 14)
    }

    assert(q == BsonDocument("$setOnInsert" -> BsonDocument("intField" -> BsonInt32(14))))
  }

  test("several update operators combined") {
    val q = compileUpdate {
      update[TestClass]
        .unset(_.dateField)
        .setOpt(_.optionField, 2L)
        .set(_.intField, 19)
    }

    assert(
      q == BsonDocument(
        ("$set"   -> BsonDocument("optionField" -> BsonInt64(2L), "intField" -> BsonInt32(19))),
        ("$unset" -> BsonDocument("dateField" -> BsonString("")))
      )
    )
  }
}
