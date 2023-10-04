package oolong.mongo

import java.time.Instant

import oolong.bson.meta.QueryMeta
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.bson.BsonInt32
import org.scalatest.funsuite.AnyFunSuite

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

    assert(
      proj == BsonDocument(
        "stringField1"                -> BsonInt32(1),
        "timeField"                   -> BsonInt32(1),
        "innerClassField.fieldToKeep" -> BsonInt32(1)
      )
    )
  }

  test("projection with BaseClass QueryMeta") {
    val proj = projection[BaseClass, ProjectionClass]

    inline given QueryMeta[Inner] = QueryMeta.snakeCase
    inline given QueryMeta[BaseClass] = QueryMeta.snakeCase

    assert(
      proj == BsonDocument(
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
}
