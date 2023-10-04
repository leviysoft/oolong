package oolong.bson.meta

import java.time.Instant
import scala.annotation.nowarn
import scala.util.Random

import oolong.bson.meta.*
import oolong.bson.meta.queryMeta
import org.scalatest.funsuite.AnyFunSuite

@nowarn("msg=unused local definition") // checks macro before expansion
class QueryMetaSpec extends AnyFunSuite {

  case class LowLevel(fieldOne: String)
  case class MiddleLevel(fieldOne: LowLevel)
  case class UpperLevel(fieldOne: Int, fieldTwo: String, fieldThree: MiddleLevel)
  case class MiddleLevelOption(fieldOne: Option[LowLevel])

  inline given QueryMeta[LowLevel] = queryMeta(_.fieldOne -> "fieldOneRenamed")
  inline given QueryMeta[MiddleLevel] = queryMeta(_.fieldOne -> "fieldTwo")
  inline given QueryMeta[UpperLevel] = queryMeta(_.fieldTwo -> "fieldTwoRenamed", _.fieldThree -> "fieldThreeRenamed")

  test("QueryMeta[LowLevel] is correct") {

    assert(
      summon[QueryMeta[LowLevel]] == QueryMeta[LowLevel](
        Map(
          "fieldOne" -> "fieldOneRenamed"
        )
      )
    )
  }

  test("QueryMeta[MiddleLevel] is correct") {

    assert(
      summon[QueryMeta[MiddleLevel]] == QueryMeta[MiddleLevel](
        Map(
          "fieldOne"          -> "fieldTwo",
          "fieldOne.fieldOne" -> "fieldTwo.fieldOneRenamed",
        )
      )
    )
  }

  test("QueryMeta[UpperLevel] is correct") {

    assert(
      summon[QueryMeta[UpperLevel]] == QueryMeta[UpperLevel](
        Map(
          "fieldTwo"                     -> "fieldTwoRenamed",
          "fieldThree"                   -> "fieldThreeRenamed",
          "fieldThree.fieldOne"          -> "fieldThreeRenamed.fieldTwo",
          "fieldThree.fieldOne.fieldOne" -> "fieldThreeRenamed.fieldTwo.fieldOneRenamed",
        )
      )
    )
  }

  inline given QueryMeta[MiddleLevelOption] = queryMeta(_.fieldOne -> "fieldOneRenamed")

  test("QueryMeta[MiddleLevelOption] is correct for Option[_] fields") {

    assert(
      summon[QueryMeta[MiddleLevelOption]] == QueryMeta[MiddleLevelOption](
        Map(
          "fieldOne"          -> "fieldOneRenamed",
          "fieldOne.fieldOne" -> "fieldOneRenamed.fieldOneRenamed",
        )
      )
    )
  }

  test("QueryMeta is correct if intermediate class meta is not specified") {

    case class LastClass(fieldOne: String)
    case class IntermediateClass(fieldOne: LastClass)
    case class FirstClass(fieldOne: String, fieldTwo: IntermediateClass)

    inline given QueryMeta[LastClass] = queryMeta(_.fieldOne -> "FieldOne")
    inline given QueryMeta[FirstClass] = queryMeta(_.fieldTwo -> "FieldTwo")

    assert(
      summon[QueryMeta[FirstClass]] == QueryMeta[FirstClass](
        Map(
          "fieldTwo"                   -> "FieldTwo",
          "fieldTwo.fieldOne"          -> "FieldTwo.fieldOne",
          "fieldTwo.fieldOne.fieldOne" -> "FieldTwo.fieldOne.FieldOne",
        )
      )
    )

  }

  test("QueryMeta is correct for snakeCase") {

    case class CaseClass2(someFieldName: String)
    case class CaseClass(fieldOne: String, fieldTwo: Int, fieldThree: CaseClass2)

    inline given QueryMeta[CaseClass2] = QueryMeta.snakeCase
    inline given QueryMeta[CaseClass] = QueryMeta.snakeCase

    assert(
      summon[QueryMeta[CaseClass]] == QueryMeta[CaseClass](
        Map(
          "fieldOne"                 -> "field_one",
          "fieldTwo"                 -> "field_two",
          "fieldThree"               -> "field_three",
          "fieldThree.someFieldName" -> "field_three.some_field_name"
        )
      )
    )
  }

  test("QueryMeta is correct for camelCase") {

    case class CaseClass(field_one: String, field_two: Int)

    inline given QueryMeta[CaseClass] = QueryMeta.camelCase

    assert(
      summon[QueryMeta[CaseClass]] == QueryMeta[CaseClass](
        Map(
          "field_one" -> "fieldOne",
          "field_two" -> "fieldTwo"
        )
      )
    )
  }

  test("QueryMeta is correct for upperCamelCase") {

    case class CaseClass(field_one: String, fieldTwo: Int)

    inline given QueryMeta[CaseClass] = QueryMeta.upperCamelCase

    assert(
      summon[QueryMeta[CaseClass]] == QueryMeta[CaseClass](
        Map(
          "field_one" -> "FieldOne",
          "fieldTwo"  -> "FieldTwo"
        )
      )
    )
  }

  test("QueryMeta.identity doesn't change names") {

    case class CaseClass(field_one: String, fieldTwo: Int)

    inline given QueryMeta[CaseClass] = QueryMeta.identity

    assert(
      summon[QueryMeta[CaseClass]] == QueryMeta[CaseClass](Map.empty[String, String])
    )
  }

  test("QueryMeta with renaming") {
    case class CaseClass(field: String)
    case class CaseClassAnother(fieldOne: String, fieldTwo: Int, fieldThree: CaseClass)

    inline given QueryMeta[CaseClass] = queryMeta[CaseClass](_.field -> "field_new")
    inline given QueryMeta[CaseClassAnother] = QueryMeta.snakeCase.withRenaming(_.fieldThree -> "field_three_new")

    assert(
      summon[QueryMeta[CaseClassAnother]] == QueryMeta[CaseClassAnother](
        Map(
          "fieldOne"         -> "field_one",
          "fieldTwo"         -> "field_two",
          "fieldThree"       -> "field_three_new",
          "fieldThree.field" -> "field_three_new.field_new",
        )
      )
    )
  }

  test("Query meta derived for projection") {
    case class Inner(a: Int, b: BigDecimal)

    case class BaseClass(
        intField: Int,
        stringField: String,
        timeField: Instant,
        innerClassField: Inner
    )

    case class InnerProjection(a: Int)

    case class ProjectionClass(
        stringField: String,
        timeField: Instant,
        innerClassField: InnerProjection
    )

    inline given QueryMeta[Inner] = queryMeta(_.a -> "field_one", _.b -> "missing")
    inline given QueryMeta[BaseClass] =
      queryMeta(_.innerClassField -> "inner_classField", _.timeField -> "time", _.intField -> "not_present")

    inline given QueryMeta[ProjectionClass] = QueryMeta.deriveForProjection[BaseClass, ProjectionClass]

    assert(
      summon[QueryMeta[ProjectionClass]] == QueryMeta[ProjectionClass](
        Map(
          "innerClassField"   -> "inner_classField",
          "innerClassField.a" -> "inner_classField.field_one",
          "timeField"         -> "time",
        )
      )
    )

  }

}
