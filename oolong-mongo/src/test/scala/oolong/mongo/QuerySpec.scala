package oolong.mongo

import java.time.LocalDate
import java.time.ZoneOffset
import java.util.regex.Pattern

import oolong.bson.BsonEncoder
import oolong.bson.given
import oolong.dsl.*
import org.mongodb.scala.bson.BsonArray
import org.mongodb.scala.bson.BsonBoolean
import org.mongodb.scala.bson.BsonDateTime
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.bson.BsonDouble
import org.mongodb.scala.bson.BsonInt32
import org.mongodb.scala.bson.BsonInt64
import org.mongodb.scala.bson.BsonString
import org.mongodb.scala.bson.BsonValue
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

class QuerySpec extends AnyFunSuite {

  trait TestClassAncestor {
    def intField: Int
  }

  case class TestClass(
      intField: Int,
      doubleField: Double,
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

  test("$eq is flat bson") {

    val q    = query[TestClass](_.intField == 2)
    val repr = renderQuery[TestClass](_.intField == 2)

    test(q, repr, BsonDocument("intField" -> BsonInt32(2)))
  }

  test("$gt") {

    val q    = query[TestClass](_.intField > 2)
    val repr = renderQuery[TestClass](_.intField > 2)

    test(q, repr, BsonDocument("intField" -> BsonDocument("$gt" -> BsonInt32(2))))
  }

  test("$gte") {

    val q    = query[TestClass](_.intField >= 2)
    val repr = renderQuery[TestClass](_.intField >= 2)

    test(q, repr, BsonDocument("intField" -> BsonDocument("$gte" -> BsonInt32(2))))

  }

  test("$in") {

    val q    = query[TestClass](f => List(1, 2, 3).contains(f.intField))
    val repr = renderQuery[TestClass](f => List(1, 2, 3).contains(f.intField))

    val q1    = query[TestClass](_.listField.contains(1.1))
    val repr1 = renderQuery[TestClass](_.listField.contains(1.1))

    val q2    = query[TestClass](!_.listField.contains(1.1))
    val repr2 = renderQuery[TestClass](!_.listField.contains(1.1))

    test(
      q,
      repr,
      BsonDocument(
        "intField" -> BsonDocument("$in" -> BsonArray.fromIterable(List(BsonInt32(1), BsonInt32(2), BsonInt32(3))))
      )
    )

    test(
      q1,
      repr1,
      BsonDocument("listField" -> BsonDouble(1.1))
    )

    test(
      q2,
      repr2,
      BsonDocument("listField" -> BsonDocument("$ne" -> BsonDouble(1.1)))
    )

  }

  test("$nin") {

    val q    = query[TestClass](f => !List(4, 5, 6).contains(f.intField))
    val repr = renderQuery[TestClass](f => !List(4, 5, 6).contains(f.intField))

    test(
      q,
      repr,
      BsonDocument(
        "intField" -> BsonDocument("$nin" -> BsonArray.fromIterable(List(BsonInt32(4), BsonInt32(5), BsonInt32(6))))
      )
    )
  }

  test("$lt") {

    val q    = query[TestClass](_.intField < 2)
    val repr = renderQuery[TestClass](_.intField < 2)

    test(
      q,
      repr,
      BsonDocument("intField" -> BsonDocument("$lt" -> BsonInt32(2)))
    )

  }

  test("$lte") {

    val q    = query[TestClass](_.intField <= 2)
    val repr = renderQuery[TestClass](_.intField <= 2)

    test(
      q,
      repr,
      BsonDocument("intField" -> BsonDocument("$lte" -> BsonInt32(2)))
    )

  }

  test("$ne") {

    val q    = query[TestClass](_.stringField != "some")
    val repr = renderQuery[TestClass](_.stringField != "some")
    test(
      q,
      repr,
      BsonDocument("stringField" -> BsonDocument("$ne" -> BsonString("some")))
    )

  }

  test("test with lift(...) for custom types") {
    val q    = query[TestClass](_.dateField == lift(LocalDate.of(2020, 12, 12)))
    val repr = renderQuery[TestClass](_.dateField == lift(LocalDate.of(2020, 12, 12)))

    test(
      q,
      repr,
      BsonDocument(
        "dateField" -> BsonDateTime(
          java.util.Date.from(
            LocalDate
              .of(2020, 12, 12)
              .atStartOfDay()
              .atZone(ZoneOffset.UTC)
              .toInstant
          )
        )
      ),
      ignoreRender = true // lift
    )

  }

  test("test with lift(...) for case classes ") {
    val q    = query[TestClass](_.innerClassField == lift(InnerClass("one", 2)))
    val repr = renderQuery[TestClass](_.innerClassField == lift(InnerClass("one", 2)))
    test(
      q,
      repr,
      BsonDocument(
        "innerClassField" -> BsonDocument("fieldOne" -> BsonString("one"), "fieldTwo" -> BsonInt32(2))
      )
    )
  }

  test("test with lift(...) for arrays") {

    val q    = query[TestClass](f => lift(List(1, 2, 3).filter(_ != 2)).contains(f.intField))
    val repr = renderQuery[TestClass](f => lift(List(1, 2, 3).filter(_ != 2)).contains(f.intField))

    test(
      q,
      repr,
      BsonDocument(
        "intField" -> BsonDocument("$in" -> BsonArray.fromIterable(List(BsonInt32(1), BsonInt32(3))))
      ),
      ignoreRender = true // lift
    )

  }

  test("test with lift(...) for sets") {

    val q    = query[TestClass](f => lift(List(1, 2, 3).filter(_ != 2)).contains(f.intField))
    val repr = renderQuery[TestClass](f => lift(List(1, 2, 3).filter(_ != 2)).contains(f.intField))

    test(
      q,
      repr,
      BsonDocument(
        "intField" -> BsonDocument("$in" -> BsonArray.fromIterable(List(BsonInt32(1), BsonInt32(3))))
      ),
      ignoreRender = true // lift
    )
  }

  test("$and condition in queries flattens") {
    val q    = query[TestClass](f => f.intField == 3 && f.stringField != "some")
    val repr = renderQuery[TestClass](f => f.intField == 3 && f.stringField != "some")

    test(
      q,
      repr,
      BsonDocument(
        "intField"    -> BsonInt32(3),
        "stringField" -> BsonDocument("$ne" -> BsonString("some"))
      )
    )

  }

  test("$and with field having more than one condition is in full form") {
    val q    = query[TestClass](f => f.intField > 3 && f.intField != 5 && f.optionField.isEmpty)
    val repr = renderQuery[TestClass](f => f.intField > 3 && f.intField != 5 && f.optionField.isEmpty)

    test(
      q,
      repr,
      BsonDocument(
        "$and" -> BsonArray.fromIterable(
          List(
            BsonDocument("intField"    -> BsonDocument("$gt" -> BsonInt32(3))),
            BsonDocument("intField"    -> BsonDocument("$ne" -> BsonInt32(5))),
            BsonDocument("optionField" -> BsonDocument("$exists" -> BsonBoolean(false))),
          )
        )
      )
    )

  }

  test("$or") {
    val q    = query[TestClass](f => f.intField == 3 || f.stringField != "some")
    val repr = renderQuery[TestClass](f => f.intField == 3 || f.stringField != "some")

    test(
      q,
      repr,
      BsonDocument(
        "$or" -> BsonArray.fromIterable(
          List(
            BsonDocument("intField"    -> BsonInt32(3)),
            BsonDocument("stringField" -> BsonDocument("$ne" -> BsonString("some")))
          )
        )
      )
    )

  }

  test("both $and and $or #1") {
    val q    = query[TestClass](f => (f.intField == 3 || f.stringField != "some") && f.listField.isEmpty)
    val repr = renderQuery[TestClass](f => (f.intField == 3 || f.stringField != "some") && f.listField.isEmpty)

    test(
      q,
      repr,
      BsonDocument(
        "$or" -> BsonArray.fromIterable(
          List(
            BsonDocument("intField"    -> BsonInt32(3)),
            BsonDocument("stringField" -> BsonDocument("$ne" -> BsonString("some")))
          )
        ),
        "listField" -> BsonDocument("$size" -> BsonInt32(0))
      )
    )
  }

  test("both $and and $or #2") {
    val q    = query[TestClass](f => f.intField == 3 || f.stringField != "some" && f.listField.isEmpty)
    val repr = renderQuery[TestClass](f => f.intField == 3 || f.stringField != "some" && f.listField.isEmpty)

    test(
      q,
      repr,
      BsonDocument(
        "$or" -> BsonArray.fromIterable(
          List(
            BsonDocument("intField" -> BsonInt32(3)),
            BsonDocument(
              "stringField" -> BsonDocument("$ne" -> BsonString("some")),
              "listField"   -> BsonDocument("$size" -> BsonInt32(0))
            )
          )
        )
      )
    )

  }

  test("$not $eq transforms into $ne") {
    val q    = query[TestClass](f => !(f.intField == 3))
    val repr = renderQuery[TestClass](f => !(f.intField == 3))

    test(
      q,
      repr,
      BsonDocument(
        "intField" -> BsonDocument("$ne" -> BsonInt32(3))
      )
    )
  }

  test("$not") {
    val q    = query[TestClass](f => !(f.intField > 3))
    val repr = renderQuery[TestClass](f => !(f.intField > 3))

    test(
      q,
      repr,
      BsonDocument(
        "intField" -> BsonDocument("$not" -> BsonDocument("$gt" -> BsonInt32(3)))
      )
    )

  }

  test("$exists true") {
    val q    = query[TestClass](f => f.optionField.isDefined)
    val repr = renderQuery[TestClass](f => f.optionField.isDefined)

    test(
      q,
      repr,
      BsonDocument("optionField" -> BsonDocument("$exists" -> BsonBoolean(true)))
    )
  }

  test("$exists false") {
    val q    = query[TestClass](f => f.optionField.isEmpty)
    val repr = renderQuery[TestClass](f => f.optionField.isEmpty)

    test(
      q,
      repr,
      BsonDocument("optionField" -> BsonDocument("$exists" -> BsonBoolean(false)))
    )
  }

  test("raw Bson in a query") {
    val q = query[TestClass](
      _.intField == 2 && unchecked(
        BsonDocument(
          "innerClassField" -> BsonDocument("fieldOne" -> BsonString("one"), "fieldTwo" -> BsonInt32(2))
        )
      )
    )
    val repr = renderQuery[TestClass](
      _.intField == 2 && unchecked(
        BsonDocument(
          "innerClassField" -> BsonDocument("fieldOne" -> BsonString("one"), "fieldTwo" -> BsonInt32(2))
        )
      )
    )

    test(
      q,
      repr,
      BsonDocument(
        "intField"        -> BsonInt32(2),
        "innerClassField" -> BsonDocument("fieldOne" -> BsonString("one"), "fieldTwo" -> BsonInt32(2))
      ),
      ignoreRender = true // unchecked
    )

  }

  test("query with !! operator for Option[_] fields") {
    val q    = query[TestClass](_.optionInnerClassField.!!.fieldTwo == 2)
    val repr = renderQuery[TestClass](_.optionInnerClassField.!!.fieldTwo == 2)

    test(
      q,
      repr,
      BsonDocument("optionInnerClassField.fieldTwo" -> BsonInt32(2))
    )

  }

  test("$size with .empty") {
    val q    = query[TestClass](_.listField.isEmpty)
    val repr = renderQuery[TestClass](_.listField.isEmpty)

    test(
      q,
      repr,
      BsonDocument("listField" -> BsonDocument("$size" -> BsonInt32(0)))
    )

  }

  test("$size with .size == ?") {
    val q    = query[TestClass](_.listField.size == 2)
    val repr = renderQuery[TestClass](_.listField.size == 2)

    test(
      q,
      repr,
      BsonDocument("listField" -> BsonDocument("$size" -> BsonInt32(2)))
    )

  }

  test("$size with .length == ?") {
    val q    = query[TestClass](_.listField.length == 2)
    val repr = renderQuery[TestClass](_.listField.length == 2)

    test(
      q,
      repr,
      BsonDocument("listField" -> BsonDocument("$size" -> BsonInt32(2)))
    )

  }

  test("$eq for element in collection") {

    val q    = query[TestClass](_.listField.contains(1.1))
    val repr = renderQuery[TestClass](_.listField.contains(1.1))

    test(
      q,
      repr,
      BsonDocument(
        "listField" -> BsonDouble(1.1)
      )
    )

  }

  test("$ne for element in collection") {

    val q    = query[TestClass](!_.listField.contains(1.1))
    val repr = renderQuery[TestClass](!_.listField.contains(1.1))

    test(
      q,
      repr,
      BsonDocument(
        "listField" -> BsonDocument("$ne" -> BsonDouble(1.1))
      )
    )

  }

  test("$eq for element in Option[_] field") {

    val q    = query[TestClass](_.optionField.contains(2L))
    val repr = renderQuery[TestClass](_.optionField.contains(2L))

    test(
      q,
      repr,
      BsonDocument(
        "optionField" -> BsonInt64(2L)
      )
    )

  }

  test("$ne for element in Option[_] field") {

    val q    = query[TestClass](!_.optionField.contains(2L))
    val repr = renderQuery[TestClass](!_.optionField.contains(2L))

    test(
      q,
      repr,
      BsonDocument(
        "optionField" -> BsonDocument("$ne" -> BsonInt64(2L))
      )
    )
  }

  inline def mySubquery1(doc: TestClass): Boolean = doc.intField == 123

  test("calling an 'inline def' with the '(_)' syntax") {

    val q    = query[TestClass](mySubquery1(_))
    val repr = renderQuery[TestClass](mySubquery1(_))

    test(
      q,
      repr,
      BsonDocument(
        "intField" -> BsonInt32(123)
      )
    )
  }

  test("calling an 'inline def' with the '(x => f(x))' syntax") {

    val q    = query[TestClass](x => mySubquery1(x))
    val repr = renderQuery[TestClass](x => mySubquery1(x))

    test(
      q,
      repr,
      BsonDocument(
        "intField" -> BsonInt32(123)
      )
    )
  }

  test("'inline def' with '!!'") {

    inline def myFilter(doc: TestClass): Boolean = doc.optionField.!! == 123L

    val q    = query[TestClass](myFilter(_))
    val repr = renderQuery[TestClass](myFilter(_))

    test(
      q,
      repr,
      BsonDocument(
        "optionField" -> BsonInt64(123)
      )
    )
  }

  test("generic 'inline def' with '<:' constraint") {

    inline def genericSubquery[A <: TestClassAncestor](doc: A): Boolean = doc.intField == 123

    val q    = query[TestClass](genericSubquery(_))
    val repr = renderQuery[TestClass](genericSubquery(_))

    test(
      q,
      repr,
      BsonDocument(
        "intField" -> BsonInt32(123)
      )
    )

  }

  test("'inline def' without explicit return type") {

    inline def myFilter(doc: TestClass) = doc.intField == 123

    val q    = query[TestClass](myFilter(_))
    val repr = renderQuery[TestClass](myFilter(_))

    test(
      q,
      repr,
      BsonDocument(
        "intField" -> BsonInt32(123)
      )
    )
  }

  test("composing queries via 'inline def' #1") {

    val q    = query[TestClass](x => x.stringField == "qqq" && mySubquery1(x))
    val repr = renderQuery[TestClass](x => x.stringField == "qqq" && mySubquery1(x))

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonString("qqq"),
        "intField"    -> BsonInt32(123)
      )
    )

  }

  test("composing queries via 'inline def' #2") {

    inline def mySubquery2(tc: TestClass): Boolean = mySubquery1(tc) || tc.intField == 456

    val q    = query[TestClass](mySubquery2(_))
    val repr = renderQuery[TestClass](mySubquery2(_))

    test(
      q,
      repr,
      BsonDocument(
        "$or" -> BsonArray.fromIterable(
          List(
            BsonDocument("intField" -> BsonInt32(123)),
            BsonDocument("intField" -> BsonInt32(456))
          )
        )
      )
    )
  }

  test("regex #1") {
    val q    = query[TestClass](_.stringField.matches("(?ix)SomeString"))
    val repr = renderQuery[TestClass](_.stringField.matches("(?ix)SomeString"))

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"), "$options" -> BsonString("ix"))
      )
    )

  }

  test("regex #2") {
    val q    = query[TestClass](s => Pattern.compile("(?ix)SomeString").matcher(s.stringField).matches())
    val repr = renderQuery[TestClass](s => Pattern.compile("(?ix)SomeString").matcher(s.stringField).matches())

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"), "$options" -> BsonString("ix"))
      )
    )
  }

  test("regex #3") {
    val q = query[TestClass](s =>
      Pattern.compile("SomeString", Pattern.CASE_INSENSITIVE | Pattern.COMMENTS).matcher(s.stringField).matches()
    )
    val repr = renderQuery[TestClass](s =>
      Pattern.compile("SomeString", Pattern.CASE_INSENSITIVE | Pattern.COMMENTS).matcher(s.stringField).matches()
    )

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"), "$options" -> BsonString("ix"))
      )
    )
  }

  test("regex #4") {
    val q    = query[TestClass](s => Pattern.matches("(?ix)SomeString", s.stringField))
    val repr = renderQuery[TestClass](s => Pattern.matches("(?ix)SomeString", s.stringField))

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"), "$options" -> BsonString("ix"))
      )
    )
  }

  test("regex unknown flags not passed #1") {
    val q    = query[TestClass](_.stringField.matches("(?ixmu)SomeString"))
    val repr = renderQuery[TestClass](_.stringField.matches("(?ixmu)SomeString"))

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"), "$options" -> BsonString("imx"))
      )
    )
  }

  test("regex unknown flags not passed #2") {
    val q    = query[TestClass](s => Pattern.compile("(?ixmu)SomeString").matcher(s.stringField).matches())
    val repr = renderQuery[TestClass](s => Pattern.compile("(?ixmu)SomeString").matcher(s.stringField).matches())

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"), "$options" -> BsonString("imx"))
      )
    )
  }

  test("regex unknown flags not passed #3") {
    val q = query[TestClass](s =>
      Pattern
        .compile("SomeString", Pattern.CASE_INSENSITIVE | Pattern.COMMENTS | Pattern.MULTILINE | Pattern.UNICODE_CASE)
        .matcher(s.stringField)
        .matches()
    )
    val repr = renderQuery[TestClass](s =>
      Pattern
        .compile("SomeString", Pattern.CASE_INSENSITIVE | Pattern.COMMENTS | Pattern.MULTILINE | Pattern.UNICODE_CASE)
        .matcher(s.stringField)
        .matches()
    )

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"), "$options" -> BsonString("imx"))
      )
    )
  }

  test("regex unknown flags not passed #4") {
    val q    = query[TestClass](s => Pattern.matches("(?ixmu)SomeString", s.stringField))
    val repr = renderQuery[TestClass](s => Pattern.matches("(?ixmu)SomeString", s.stringField))

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"), "$options" -> BsonString("imx"))
      )
    )
  }

  test("regex without flags #1") {
    val q    = query[TestClass](s => Pattern.matches("SomeString", s.stringField))
    val repr = renderQuery[TestClass](s => Pattern.matches("SomeString", s.stringField))

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"))
      )
    )
  }

  test("regex without flags #2") {
    val q    = query[TestClass](s => Pattern.compile("SomeString").matcher(s.stringField).matches())
    val repr = renderQuery[TestClass](s => Pattern.compile("SomeString").matcher(s.stringField).matches())

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"))
      )
    )
  }

  test("regex without flags #3") {
    val q    = query[TestClass](s => Pattern.matches("SomeString", s.stringField))
    val repr = renderQuery[TestClass](s => Pattern.matches("SomeString", s.stringField))

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"))
      )
    )
  }

  test("inlined pattern in regex") {

    inline def regex   = "SomeString"
    inline def pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE | Pattern.COMMENTS)

    val q    = query[TestClass](s => pattern.matcher(s.stringField).matches())
    val repr = renderQuery[TestClass](s => pattern.matcher(s.stringField).matches())

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"), "$options" -> BsonString("ix"))
      ),
      ignoreRender = true // dynamic regex
    )
  }

  test("regex dynamic pattern #1") {
    def pattern = "(?ix)SomeString"
    val q       = query[TestClass](_.stringField.matches(pattern))
    val repr    = renderQuery[TestClass](_.stringField.matches(pattern))

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"), "$options" -> BsonString("ix"))
      ),
      ignoreRender = true // dynamic regex
    )

  }

  test("regex dynamic pattern #2") {
    def regex   = "(?ix)SomeString"
    def pattern = Pattern.compile(regex)
    val q       = query[TestClass](s => pattern.matcher(s.stringField).matches())
    val repr    = renderQuery[TestClass](s => pattern.matcher(s.stringField).matches())

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"), "$options" -> BsonString("ix"))
      ),
      ignoreRender = true // dynamic regex
    )
  }

  test("regex dynamic pattern #3") {
    def regex   = "SomeString"
    def flags   = Pattern.CASE_INSENSITIVE | Pattern.COMMENTS
    def pattern = Pattern.compile(regex, flags)
    val q       = query[TestClass](s => pattern.matcher(s.stringField).matches())
    val repr    = renderQuery[TestClass](s => pattern.matcher(s.stringField).matches())

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"), "$options" -> BsonString("ix"))
      ),
      ignoreRender = true // dynamic regex
    )
  }

  test("regex dynamic pattern #4") {
    def pattern = "(?ix)SomeString"
    val q       = query[TestClass](s => Pattern.matches(pattern, s.stringField))
    val repr    = renderQuery[TestClass](s => Pattern.matches(pattern, s.stringField))

    test(
      q,
      repr,
      BsonDocument(
        "stringField" -> BsonDocument("$regex" -> BsonString("SomeString"), "$options" -> BsonString("ix"))
      ),
      ignoreRender = true // dynamic regex
    )

  }

  test("test $type for int") {
    val q    = query[TestClass](_.intField.isInstanceOf[MongoType.INT32])
    val repr = renderQuery[TestClass](_.intField.isInstanceOf[MongoType.INT32])

    test(
      q,
      repr,
      BsonDocument(
        "intField" -> BsonDocument("$type" -> BsonInt32(16))
      )
    )
  }

  test("test $type for document") {
    val q    = query[TestClass](_.innerClassField.isInstanceOf[MongoType.DOCUMENT])
    val repr = renderQuery[TestClass](_.innerClassField.isInstanceOf[MongoType.DOCUMENT])

    test(
      q,
      repr,
      BsonDocument(
        "innerClassField" -> BsonDocument("$type" -> BsonInt32(3))
      )
    )

  }
  test("test $mod for int with lift(...)") {
    val q    = query[TestClass](_.intField.mod(lift(4), 5.2))
    val repr = renderQuery[TestClass](_.intField.mod(lift(4), 5.2))

    test(
      q,
      repr,
      BsonDocument(
        "intField" -> BsonDocument(
          "$mod" -> BsonArray.fromIterable(
            List(
              BsonInt32(4),
              BsonDouble(5.2)
            )
          )
        )
      ),
      ignoreRender = true // lift
    )

  }

  test("test $mod for double with lift(...)") {
    val q    = query[TestClass](_.doubleField.mod(5.2, lift(123L)))
    val repr = renderQuery[TestClass](_.doubleField.mod(5.2, lift(123L)))

    test(
      q,
      repr,
      BsonDocument(
        "doubleField" -> BsonDocument(
          "$mod" -> BsonArray.fromIterable(
            List(
              BsonDouble(5.2),
              BsonInt64(123)
            )
          )
        )
      ),
      ignoreRender = true // lift
    )

  }

  test("$elemMatch for array containing objects") {
    case class Inner(a: Int, b: String)
    case class Test(array: Vector[Inner])

    val q    = query[Test](_.array.exists(s => s.a > 2 && s.b == "123"))
    val repr = renderQuery[Test](_.array.exists(s => s.a > 2 && s.b == "123"))

    test(
      q,
      repr,
      BsonDocument(
        "array" -> BsonDocument(
          "$elemMatch" -> BsonDocument(
            "a" -> BsonDocument("$gt" -> BsonInt32(2)),
            "b" -> BsonString("123")
          )
        )
      )
    )
  }

  test("$elemMatch for array primitives") {
    case class Test(array: Vector[Int])

    val q    = query[Test](_.array.exists(s => s > 2 && s <= 100))
    val repr = renderQuery[Test](_.array.exists(s => s > 2 && s <= 100))

    test(
      q,
      repr,
      BsonDocument(
        "array" -> BsonDocument(
          "$elemMatch" -> BsonDocument(
            "$gt"  -> BsonInt32(2),
            "$lte" -> BsonInt32(100)
          )
        )
      )
    )
  }

  test("$elemMatch for array with $and with same field twice") {
    case class Inner(a: Int, b: String)
    case class Test(array: Vector[Inner])

    val q    = query[Test](_.array.exists(s => s.a > 2 && s.a < 100 && s.b == "123"))
    val repr = renderQuery[Test](_.array.exists(s => s.a > 2 && s.a < 100 && s.b == "123"))

    test(
      q,
      repr,
      BsonDocument(
        "array" -> BsonDocument(
          "$elemMatch" -> BsonDocument(
            "$and" -> BsonArray.fromIterable(
              List(
                BsonDocument("a" -> BsonDocument("$gt" -> BsonInt32(2))),
                BsonDocument("a" -> BsonDocument("$lt" -> BsonInt32(100))),
                BsonDocument("b" -> BsonString("123")),
              )
            )
          )
        )
      )
    )
  }

  test("$elemMatch querying a single field") {
    case class Test(array: Vector[Int])

    val q    = query[Test](_.array.exists(s => s < 100))
    val repr = renderQuery[Test](_.array.exists(s => s < 100))

    test(
      q,
      repr,
      BsonDocument("array" -> BsonDocument("$lt" -> BsonInt32(100)))
    )
  }

  test("nested $elemMatch") {
    case class Inner(array1: Vector[Int])
    case class Base(array0: Vector[Inner])

    val q    = query[Base](_.array0.exists(_.array1.exists(_ > 100)))
    val repr = renderQuery[Base](_.array0.exists(_.array1.exists(_ > 100)))

    test(
      q,
      repr,
      BsonDocument(
        "array0.array1" -> BsonDocument("$gt" -> BsonInt32(100))
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
