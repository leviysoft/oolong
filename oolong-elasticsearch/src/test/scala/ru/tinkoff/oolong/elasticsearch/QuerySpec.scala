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

  test("!= query") {
    val q = query[TestClass](_.field2 != 2)

    q.render shouldBe """{"query":{"bool":{"must":[],"should":[],"must_not":[{"term":{"field2":2}}]}}}"""
  }

  test("Composite boolean query") {
    val q = query[TestClass](c => c.field1 == "check" && (c.field2 == 42 || c.field3.innerField == "inner"))

    q.render shouldBe """{"query":{"bool":{"must":[{"term":{"field1":"check"}}],"should":[{"term":{"field2":42}},{"term":{"field3.innerField":"inner"}}],"must_not":[]}}}"""
  }

  test(".isDefined query") {
    val q = query[TestClass](_.field3.optionalInnerField.isDefined)

    q.render shouldBe """{"query":{"exists":{"field":"field3.optionalInnerField"}}}"""
  }

  test(".isEmpty query") {
    val q = query[TestClass](_.field3.optionalInnerField.isEmpty)

    q.render shouldBe """{"query":{"bool":{"must":[],"should":[],"must_not":[{"exists":{"field":"field3.optionalInnerField"}}]}}}"""
  }

  test("> query") {
    val q = query[TestClass](_.field2 > 4)

    q.render shouldBe """{"query":{"range":{"field2":{"gt":4,"gte":null,"lt":null,"lte":null}}}}"""
  }

  test(">= query") {
    val q = query[TestClass](_.field2 >= 4)

    q.render shouldBe """{"query":{"range":{"field2":{"gt":null,"gte":4,"lt":null,"lte":null}}}}"""
  }

  test("< query") {
    val q = query[TestClass](_.field2 < 4)

    q.render shouldBe """{"query":{"range":{"field2":{"gt":null,"gte":null,"lt":4,"lte":null}}}}"""
  }

  test("<= query") {
    val q = query[TestClass](_.field2 <= 4)

    q.render shouldBe """{"query":{"range":{"field2":{"gt":null,"gte":null,"lt":null,"lte":4}}}}"""
  }
}
