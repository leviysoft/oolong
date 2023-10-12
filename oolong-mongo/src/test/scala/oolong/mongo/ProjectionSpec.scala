package oolong.mongo

import java.time.Instant
import java.time.LocalDate
import scala.annotation.nowarn

import oolong.bson.meta.QueryMeta
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.bson.BsonInt32
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

@nowarn("msg=unused local definition")
class ProjectionSpec extends AnyFunSuite {

  case class Inner(fieldToKeep: Int, b: BigDecimal)

  case class BaseClass(
      intField: Int,
      stringField1: String,
      timeField: Instant,
      innerClassField: Inner
  )

  case class InnerProjection(fieldToKeep: Int)

  case class ProjectionClass(
      stringField1: String,
      timeField: Instant,
      innerClassField: InnerProjection
  )

  test("projection") {
    val proj = projection[BaseClass, ProjectionClass]
    val repr = renderProjection[BaseClass, ProjectionClass]

    test(
      proj,
      repr,
      BsonDocument(
        "stringField1"                -> BsonInt32(1),
        "timeField"                   -> BsonInt32(1),
        "innerClassField.fieldToKeep" -> BsonInt32(1)
      )
    )
  }

  test("projection with BaseClass QueryMeta") {
    val proj = projection[BaseClass, ProjectionClass]
    val repr = renderProjection[BaseClass, ProjectionClass]

    inline given QueryMeta[Inner] = QueryMeta.snakeCase
    inline given QueryMeta[BaseClass] = QueryMeta.snakeCase

    test(
      proj,
      repr,
      BsonDocument(
        "string_field1"                   -> BsonInt32(1),
        "time_field"                      -> BsonInt32(1),
        "inner_class_field.field_to_keep" -> BsonInt32(1)
      )
    )
  }

  test("projection fails") {
    case class BaseInner(d: Int)
    case class Base(a: Int, b: String, c: BaseInner)

    case class Inner(d: Double)
    case class NotProjection(a: Int, c: Inner)

    assertDoesNotCompile("projection[Base, NotProjection]")

  }

  test("short path for classes with identical structure") {

    case class Inner1(field1: String, field2: Double)
    case class Inner2(field3: Int, field4: Byte)
    case class Inner(inner1: Inner1, inner2: Inner2)

    case class Base(fieldTheSame: Inner, fieldFullProjection: Inner, fieldProjection: Inner)

    case class Projection(
        fieldTheSame: Inner,
        fieldFullProjection: InnerProjection,
        fieldProjection: InnerNotFullProjection
    )

    case class Inner1Projection(field1: String, field2: Double)
    case class Inner2Projection(field3: Int, field4: Byte)
    case class InnerProjection(inner1: Inner1Projection, inner2: Inner2Projection)

    case class Inner1NotFullProjection(field1: String)
    case class InnerNotFullProjection(inner1: Inner1NotFullProjection, inner2: Inner2Projection)

    val proj = projection[Base, Projection]
    val repr = renderProjection[Base, Projection]

    test(
      proj,
      repr,
      BsonDocument(
        "fieldTheSame"                  -> BsonInt32(1),
        "fieldFullProjection"           -> BsonInt32(1),
        "fieldProjection.inner1.field1" -> BsonInt32(1),
        "fieldProjection.inner2"        -> BsonInt32(1),
      )
    )
  }

  test("Full projection returns empty document") {
    case class Passport(number: String, issueDate: LocalDate)

    case class BirthInfo(country: String, date: LocalDate)

    case class Student(name: String, lastName: String, passport: Passport, birthInfo: BirthInfo)

    case class StudentDTO(name: String, lastName: String, passport: PassportDTO, birthInfo: BirthInfoDTO)

    case class PassportDTO(number: String, issueDate: LocalDate)

    case class BirthInfoDTO(country: String, date: LocalDate)

    val proj = projection[Student, StudentDTO]
    val repr = renderProjection[Student, StudentDTO]

    test(
      proj,
      repr,
      BsonDocument()
    )

  }

  private inline def test(
      query: BsonDocument,
      repr: String,
      toCompare: BsonDocument,
  ): Assertion =
    assert(query.toJson.replaceAll("\\s+", "") == repr.replaceAll("\\s+", ""))
    assert(query == toCompare)
}
