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

    assert(
      proj == BsonDocument(
        "fieldTheSame"                  -> BsonInt32(1),
        "fieldFullProjection"           -> BsonInt32(1),
        "fieldProjection.inner1.field1" -> BsonInt32(1),
        "fieldProjection.inner2"        -> BsonInt32(1),
      )
    )
  }
}
