package ru.tinkoff.oolong

import org.scalatest.funsuite.AnyFunSuite

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

}
