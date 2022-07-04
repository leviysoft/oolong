package ru.tinkoff.oolong.elasticsearch

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.shouldBe

import ru.tinkoff.oolong.dsl.*

class QuerySpec extends AnyFunSuite {
  test("Term query") {
    val q = query[TestClass](_.field2 == 2)

    q.render shouldBe """{"query":{"term":{"field2":2}}}"""
  }

  test("$$ query") {
    val q = query[TestClass](c => c.field1 == "check" && c.field2 == 42)

    q.render shouldBe """{"query":{"bool":{"must":[{"term":{"field1":"check"}},{"term":{"field2":42}}],"should":[],"must_not":[]}}}"""
  }

  test("|| query") {
    val q = query[TestClass](c => c.field1 == "check" || c.field2 == 42)

    q.render shouldBe """{"query":{"bool":{"must":[],"should":[{"term":{"field1":"check"}},{"term":{"field2":42}}],"must_not":[]}}}"""
  }
}
