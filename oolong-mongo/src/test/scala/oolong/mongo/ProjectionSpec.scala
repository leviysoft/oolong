package oolong.mongo

import java.time.Instant

import oolong.bson.meta.QueryMeta
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.bson.BsonInt32
import org.scalatest.funsuite.AnyFunSuite

class ProjectionSpec extends AnyFunSuite {

  case class Inner(a: Int)

  case class BaseClass(
      intField: Int,
      stringField1: String,
      timeField: Instant,
      innerClassField: Inner
  )

  case class ProjectionClass(
      stringField1: String,
      timeField: Instant,
      innerClassField: Inner
  )

  test("projection") {
    val proj = projection[BaseClass, ProjectionClass]

    assert(
      proj == BsonDocument(
        "stringField1"    -> BsonInt32(1),
        "timeField"       -> BsonInt32(1),
        "innerClassField" -> BsonInt32(1)
      )
    )
  }

  test("projection with BaseClass QueryMeta") {
    val proj = projection[BaseClass, ProjectionClass]

    inline given QueryMeta[BaseClass] = QueryMeta.snakeCase

    assert(
      proj == BsonDocument(
        "string_field1"     -> BsonInt32(1),
        "time_field"        -> BsonInt32(1),
        "inner_class_field" -> BsonInt32(1)
      )
    )
  }
}
