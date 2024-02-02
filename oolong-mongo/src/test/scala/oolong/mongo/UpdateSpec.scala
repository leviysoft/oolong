package oolong.mongo

import java.time.LocalDate
import java.time.ZoneOffset

import oolong.bson.BsonEncoder
import oolong.bson.given
import oolong.dsl.*
import org.mongodb.scala.bson.BsonArray
import org.mongodb.scala.bson.BsonDateTime
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.bson.BsonInt32
import org.mongodb.scala.bson.BsonInt64
import org.mongodb.scala.bson.BsonString
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

class UpdateSpec extends AnyFunSuite {

  case class TestClass(
      intField: Int,
      stringField: String,
      dateField: LocalDate,
      innerClassField: InnerClass,
      optionField: Option[Long],
      optionInnerClassField: Option[InnerClass],
      listField: List[Int],
      classInnerClassField: List[InnerClass],
      nestedListField: List[List[Int]]
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
    val q    = update[TestClass](_.set(_.intField, 2))
    val repr = renderUpdate[TestClass](_.set(_.intField, 2))

    test(
      q,
      repr,
      BsonDocument("$set" -> BsonDocument("intField" -> BsonInt32(2)))
    )
  }

  test("$set for Option[_] fields") {
    val q    = update[TestClass](_.setOpt(_.optionField, 2L))
    val repr = renderUpdate[TestClass](_.setOpt(_.optionField, 2L))

    test(
      q,
      repr,
      BsonDocument("$set" -> BsonDocument("optionField" -> BsonInt64(2)))
    )
  }

  test("$set for Option[_] inner class fields") {
    val q = update[TestClass](
      _.set(
        _.optionInnerClassField.!!,
        lift(InnerClass("some", 2, fieldSix = List("1", "2"), fieldFive = SecondInnerClass()))
      )
    )

    val repr = renderUpdate[TestClass](
      _.set(
        _.optionInnerClassField.!!,
        lift(InnerClass("some", 2, fieldSix = List("1", "2"), fieldFive = SecondInnerClass()))
      )
    )

    test(
      q,
      repr,
      BsonDocument(
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
      ),
      ignoreRender = true // Function(Location.now())
    )
  }

  test("$inc") {
    val q    = update[TestClass](_.inc(_.intField, 1))
    val repr = renderUpdate[TestClass](_.inc(_.intField, 1))

    test(
      q,
      repr,
      BsonDocument("$inc" -> BsonDocument("intField" -> BsonInt32(1)))
    )
  }

  test("$mul") {
    val q    = update[TestClass](_.mul(_.intField, 10))
    val repr = renderUpdate[TestClass](_.mul(_.intField, 10))

    test(
      q,
      repr,
      BsonDocument("$mul" -> BsonDocument("intField" -> BsonInt32(10)))
    )
  }

  test("$max") {
    val q    = update[TestClass](_.max(_.intField, 10))
    val repr = renderUpdate[TestClass](_.max(_.intField, 10))

    test(
      q,
      repr,
      BsonDocument("$max" -> BsonDocument("intField" -> BsonInt32(10)))
    )
  }

  test("$min") {
    val q    = update[TestClass](_.min(_.intField, 10))
    val repr = renderUpdate[TestClass](_.min(_.intField, 10))

    test(
      q,
      repr,
      BsonDocument("$min" -> BsonDocument("intField" -> BsonInt32(10)))
    )
  }

  test("$rename") {
    val q    = update[TestClass](_.rename(_.intField, "newFieldName"))
    val repr = renderUpdate[TestClass](_.rename(_.intField, "newFieldName"))

    test(
      q,
      repr,
      BsonDocument("$rename" -> BsonDocument("intField" -> BsonString("newFieldName")))
    )
  }

  test("$unset") {
    val q    = update[TestClass](_.unset(_.intField))
    val repr = renderUpdate[TestClass](_.unset(_.intField))

    test(
      q,
      repr,
      BsonDocument("$unset" -> BsonDocument("intField" -> BsonString("")))
    )
  }

  test("$setOnInsert") {
    val q    = update[TestClass](_.setOnInsert(_.intField, 14))
    val repr = renderUpdate[TestClass](_.setOnInsert(_.intField, 14))

    test(
      q,
      repr,
      BsonDocument("$setOnInsert" -> BsonDocument("intField" -> BsonInt32(14)))
    )
  }

  test("$addToSet") {
    val q    = update[TestClass](_.addToSet(_.listField, 1))
    val repr = renderUpdate[TestClass](_.addToSet(_.listField, 1))
    test(
      q,
      repr,
      BsonDocument("$addToSet" -> BsonDocument("listField" -> BsonInt32(1)))
    )
  }

  test("$addToSet nested") {
    val q    = update[TestClass](_.addToSet(_.nestedListField, List(1, 2, 3)))
    val repr = renderUpdate[TestClass](_.addToSet(_.nestedListField, List(1, 2, 3)))
    test(
      q,
      repr,
      BsonDocument("$addToSet" -> BsonDocument("nestedListField" -> BsonArray(BsonInt32(1), BsonInt32(2), BsonInt32(3))))
    )
  }

  test("$addToSet with $each") {
    val q    = update[TestClass](_.addToSetAll(_.listField, List(1)))
    val repr = renderUpdate[TestClass](_.addToSetAll(_.listField, List(1)))
    test(
      q,
      repr,
      BsonDocument("$addToSet" -> BsonDocument("listField" -> BsonDocument("$each" -> BsonArray(BsonInt32(1)))))
    )
  }

  test("$addToSet with $each nested") {
    val q    = update[TestClass](_.addToSetAll(_.nestedListField, lift(List(List(1, 2, 3)))))
    val repr = renderUpdate[TestClass](_.addToSetAll(_.nestedListField, lift(List(List(1, 2, 3)))))
    test(
      q,
      repr,
      BsonDocument(
        "$addToSet" -> BsonDocument(
          "nestedListField" -> BsonDocument("$each" -> BsonArray(BsonArray(BsonInt32(1), BsonInt32(2), BsonInt32(3))))
        )
      ),
      ignoreRender = true
    )
  }

  test("several update operators combined") {
    val q = update[TestClass](
      _.unset(_.dateField)
        .setOpt(_.optionField, 2L)
        .set(_.intField, 19)
    )

    val repr = renderUpdate[TestClass](
      _.unset(_.dateField)
        .setOpt(_.optionField, 2L)
        .set(_.intField, 19)
    )

    test(
      q,
      repr,
      BsonDocument(
        "$set"   -> BsonDocument("optionField" -> BsonInt64(2L), "intField" -> BsonInt32(19)),
        "$unset" -> BsonDocument("dateField" -> BsonString(""))
      )
    )
  }

  private inline def test(
      query: BsonDocument,
      repr: String,
      toCompare: BsonDocument,
      ignoreRender: Boolean = false
  ): Assertion =
    if (!ignoreRender)
      assert(query.toJson.replaceAll("\\s+", "") == repr.replaceAll("\\s+", ""))
    assert(query == toCompare)
}
