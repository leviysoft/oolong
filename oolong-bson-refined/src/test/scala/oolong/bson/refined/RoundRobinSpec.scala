package oolong.bson.refined

import eu.timepit.refined.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.*
import oolong.bson.{*, given}
import org.scalatest.TryValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import cats.scalatest.EitherValues

class RoundRobinSpec extends AnyFunSuite with Matchers with TryValues with EitherValues  {
  test("Refined serialization") {
    val refinedVal: Int Refined Positive = refineV[Positive](5).value

    val sut = BsonDecoder[Int Refined Positive].fromBson(refinedVal.bson)

    sut.success.value shouldEqual refinedVal
  }
}
