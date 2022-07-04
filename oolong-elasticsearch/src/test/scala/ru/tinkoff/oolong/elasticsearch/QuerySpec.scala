package ru.tinkoff.oolong.elasticsearch

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.shouldBe

import ru.tinkoff.oolong.dsl.*

class QuerySpec extends AnyFunSuite {
  test("Term query") {
    val q = query[TestClass](_.field2 == 2)

    q.render shouldBe """{"query":{"term":{"field2":2}}}"""
  }
}
