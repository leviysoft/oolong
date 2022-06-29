package ru.tinkoff.oolong.mongo

import java.time.LocalDate
import java.time.ZoneId
import java.time.ZoneOffset

import org.mongodb.scala.bson.BsonArray
import org.mongodb.scala.bson.BsonBoolean
import org.mongodb.scala.bson.BsonDateTime
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.bson.BsonDouble
import org.mongodb.scala.bson.BsonInt32
import org.mongodb.scala.bson.BsonInt64
import org.mongodb.scala.bson.BsonString
import org.scalatest.funsuite.AnyFunSuite

import ru.tinkoff.oolong.bson.BsonEncoder
import ru.tinkoff.oolong.bson.given
import ru.tinkoff.oolong.dsl.*

class QuerySpec extends AnyFunSuite {

  trait TestClassAncestor {
    def intField: Int
  }

  case class TestClass(
      intField: Int,
      stringField: String,
      dateField: LocalDate,
      innerClassField: InnerClass,
      optionField: Option[Long],
      optionInnerClassField: Option[InnerClass],
      listField: List[Double]
  ) extends TestClassAncestor

  case class InnerClass(
      fieldOne: String,
      fieldTwo: Int
  ) derives BsonEncoder

  test("$eq") {

    val q = query[TestClass](_.intField == 2)

    assert(q == BsonDocument("intField" -> BsonDocument("$eq" -> BsonInt32(2))))
  }

  test("$gt") {

    val q = query[TestClass](_.intField > 2)

    assert(q == BsonDocument("intField" -> BsonDocument("$gt" -> BsonInt32(2))))
  }

  test("$gte") {

    val q = query[TestClass](_.intField >= 2)

    assert(q == BsonDocument("intField" -> BsonDocument("$gte" -> BsonInt32(2))))
  }

  test("$in") {

    val q = query[TestClass](f => List(1, 2, 3).contains(f.intField))

    val q1 = query[TestClass](_.listField.contains(1.1))

    val q2 = query[TestClass](!_.listField.contains(1.1))

    assert(
      q == BsonDocument(
        "intField" -> BsonDocument("$in" -> BsonArray.fromIterable(List(BsonInt32(1), BsonInt32(2), BsonInt32(3))))
      )
    )
  }

  test("$nin") {

    val q = query[TestClass](f => !List(4, 5, 6).contains(f.intField))

    assert(
      q == BsonDocument(
        "intField" -> BsonDocument("$nin" -> BsonArray.fromIterable(List(BsonInt32(4), BsonInt32(5), BsonInt32(6))))
      )
    )
  }

  test("$lt") {

    val q = query[TestClass](_.intField < 2)

    assert(q == BsonDocument("intField" -> BsonDocument("$lt" -> BsonInt32(2))))
  }

  test("$lte") {

    val q = query[TestClass](_.intField <= 2)

    assert(q == BsonDocument("intField" -> BsonDocument("$lte" -> BsonInt32(2))))
  }

  test("$ne") {

    val q = query[TestClass](_.stringField != "some")

    assert(q == BsonDocument("stringField" -> BsonDocument("$ne" -> BsonString("some"))))
  }

  test("test with lift(...) for custom types") {
    val q = query[TestClass](_.dateField == lift(LocalDate.of(2020, 12, 12)))

    assert(
      q == BsonDocument(
        "dateField" -> BsonDocument(
          "$eq" -> BsonDateTime(
            java.util.Date.from(
              LocalDate
                .of(2020, 12, 12)
                .atStartOfDay()
                .atZone(ZoneOffset.UTC)
                .toInstant
            )
          )
        )
      )
    )
  }

  test("test with lift(...) for case classes ") {
    val q = query[TestClass](_.innerClassField == lift(InnerClass("one", 2)))

    assert(
      q == BsonDocument(
        "innerClassField" -> BsonDocument(
          "$eq" -> BsonDocument("fieldOne" -> BsonString("one"), "fieldTwo" -> BsonInt32(2))
        )
      )
    )
  }

  test("test with lift(...) for arrays") {

    val q = query[TestClass](f => lift(List(1, 2, 3).filter(_ != 2)).contains(f.intField))

    assert(
      q == BsonDocument(
        "intField" -> BsonDocument("$in" -> BsonArray.fromIterable(List(BsonInt32(1), BsonInt32(3))))
      )
    )
  }

  test("test with lift(...) for sets") {

    val q = query[TestClass](f => lift(List(1, 2, 3).filter(_ != 2)).contains(f.intField))

    assert(
      q == BsonDocument(
        "intField" -> BsonDocument("$in" -> BsonArray.fromIterable(List(BsonInt32(1), BsonInt32(3))))
      )
    )
  }

  test("$and") {
    val q = query[TestClass](f => f.intField == 3 && f.stringField != "some")

    assert(
      q == BsonDocument(
        "$and" -> BsonArray.fromIterable(
          List(
            BsonDocument("intField"    -> BsonDocument("$eq" -> BsonInt32(3))),
            BsonDocument("stringField" -> BsonDocument("$ne" -> BsonString("some")))
          )
        )
      )
    )
  }

  test("$or") {
    val q = query[TestClass](f => f.intField == 3 || f.stringField != "some")

    assert(
      q == BsonDocument(
        "$or" -> BsonArray.fromIterable(
          List(
            BsonDocument("intField"    -> BsonDocument("$eq" -> BsonInt32(3))),
            BsonDocument("stringField" -> BsonDocument("$ne" -> BsonString("some")))
          )
        )
      )
    )
  }

  test("$not") {
    val q = query[TestClass](f => !(f.intField == 3))

    assert(
      q == BsonDocument(
        "intField" -> BsonDocument("$not" -> BsonDocument("$eq" -> BsonInt32(3)))
      )
    )
  }

  test("$exists true") {
    val q = query[TestClass](f => f.optionField.isDefined)
    assert(q == BsonDocument("optionField" -> BsonDocument("$exists" -> BsonBoolean(true))))
  }

  test("$exists false") {
    val q = query[TestClass](f => f.optionField.isEmpty)
    assert(q == BsonDocument("optionField" -> BsonDocument("$exists" -> BsonBoolean(false))))
  }

  test("raw Bson in a query") {
    val q = query[TestClass](
      _.intField == 2 && unchecked(
        BsonDocument(
          "innerClassField" -> BsonDocument(
            "$eq" -> BsonDocument("fieldOne" -> BsonString("one"), "fieldTwo" -> BsonInt32(2))
          )
        )
      )
    )

    assert(
      q == BsonDocument(
        "$and" -> BsonArray.fromIterable(
          List(
            BsonDocument("intField" -> BsonDocument("$eq" -> BsonInt32(2))),
            BsonDocument(
              "innerClassField" -> BsonDocument(
                "$eq" -> BsonDocument("fieldOne" -> BsonString("one"), "fieldTwo" -> BsonInt32(2))
              )
            )
          )
        )
      )
    )

  }

  test("query with !! operator for Option[_] fields") {
    val q = query[TestClass](_.optionInnerClassField.!!.fieldTwo == 2)

    assert(q == BsonDocument("optionInnerClassField.fieldTwo" -> BsonDocument("$eq" -> BsonInt32(2))))
  }

  test("$size with .empty") {
    val q = query[TestClass](_.listField.isEmpty)

    assert(q == BsonDocument("listField" -> BsonDocument("$size" -> BsonInt32(0))))
  }

  test("$size with .size == ?") {
    val q = query[TestClass](_.listField.size == 2)

    assert(q == BsonDocument("listField" -> BsonDocument("$size" -> BsonInt32(2))))
  }

  test("$size with .length == ?") {
    val q = query[TestClass](_.listField.length == 2)

    assert(q == BsonDocument("listField" -> BsonDocument("$size" -> BsonInt32(2))))
  }

  test("$eq for element in collection") {

    val q = query[TestClass](_.listField.contains(1.1))

    assert(
      q == BsonDocument(
        "listField" -> BsonDocument("$eq" -> BsonDouble(1.1))
      )
    )
  }

  test("$ne for element in collection") {

    val q = query[TestClass](!_.listField.contains(1.1))

    assert(
      q == BsonDocument(
        "listField" -> BsonDocument("$ne" -> BsonDouble(1.1))
      )
    )
  }

  test("$eq for element in Option[_] field") {

    val q = query[TestClass](_.optionField.contains(2L))

    assert(
      q == BsonDocument(
        "optionField" -> BsonDocument("$eq" -> BsonInt64(2L))
      )
    )
  }

  test("$ne for element in Option[_] field") {

    val q = query[TestClass](!_.optionField.contains(2L))

    assert(
      q == BsonDocument(
        "optionField" -> BsonDocument("$ne" -> BsonInt64(2L))
      )
    )
  }

  inline def mySubquery1(doc: TestClass): Boolean = doc.intField == 123

  test("calling an 'inline def' with the '(_)' syntax") {

    val q = query[TestClass](mySubquery1(_))

    assert(
      q == BsonDocument(
        "intField" -> BsonDocument("$eq" -> BsonInt32(123))
      )
    )
  }

  test("calling an 'inline def' with the '(x => f(x))' syntax") {

    val q = query[TestClass](x => mySubquery1(x))

    assert(
      q == BsonDocument(
        "intField" -> BsonDocument("$eq" -> BsonInt32(123))
      )
    )
  }

  test("'inline def' with '!!'") {

    inline def myFilter(doc: TestClass): Boolean = doc.optionField.!! == 123L

    val q = query[TestClass](myFilter(_))

    assert(
      q == BsonDocument(
        "optionField" -> BsonDocument("$eq" -> BsonInt64(123))
      )
    )
  }

  test("generic 'inline def' with '<:' constraint") {

    inline def genericSubquery[A <: TestClassAncestor](doc: A): Boolean = doc.intField == 123

    val q = query[TestClass](genericSubquery(_))

    assert(
      q == BsonDocument(
        "intField" -> BsonDocument("$eq" -> BsonInt32(123))
      )
    )
  }

  test("'inline def' without explicit return type") {

    inline def myFilter(doc: TestClass) = doc.intField == 123

    val q = query[TestClass](myFilter(_))

    assert(
      q == BsonDocument(
        "intField" -> BsonDocument("$eq" -> BsonInt32(123))
      )
    )
  }

  test("composing queries via 'inline def' #1") {

    val q = query[TestClass](x => x.stringField == "qqq" && mySubquery1(x))

    assert(
      q == BsonDocument(
        "$and" -> BsonArray.fromIterable(
          List(
            BsonDocument("stringField" -> BsonDocument("$eq" -> BsonString("qqq"))),
            BsonDocument("intField"    -> BsonDocument("$eq" -> BsonInt32(123)))
          )
        )
      )
    )
  }

  test("composing queries via 'inline def' #2") {

    inline def mySubquery2(tc: TestClass): Boolean = mySubquery1(tc) || tc.intField == 456

    val q = query[TestClass](mySubquery2(_))

    assert(
      q == BsonDocument(
        "$or" -> BsonArray.fromIterable(
          List(
            BsonDocument("intField" -> BsonDocument("$eq" -> BsonInt32(123))),
            BsonDocument("intField" -> BsonDocument("$eq" -> BsonInt32(456)))
          )
        )
      )
    )
  }
}
