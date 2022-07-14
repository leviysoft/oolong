package ru.tinkoff.oolong

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ShrinkSpec extends AnyFunSuite with Matchers {
  test("Shrink >") {
    val lhs = QExpr.Gt(QExpr.Prop("a"), QExpr.Constant(3))
    val rhs = QExpr.Gt(QExpr.Prop("a"), QExpr.Constant(4))

    LogicalOptimizer.shrink(lhs, rhs) shouldBe rhs
  }

  test("Shrink >=") {
    val lhs = QExpr.Gte(QExpr.Prop("a"), QExpr.Constant(3))
    val rhs = QExpr.Gte(QExpr.Prop("a"), QExpr.Constant(4))

    LogicalOptimizer.shrink(lhs, rhs) shouldBe rhs
  }

  test("Shrink <") {
    val lhs = QExpr.Lt(QExpr.Prop("a"), QExpr.Constant(3))
    val rhs = QExpr.Lt(QExpr.Prop("a"), QExpr.Constant(4))

    LogicalOptimizer.shrink(lhs, rhs) shouldBe lhs
  }

  test("Shrink <=") {
    val lhs = QExpr.Lte(QExpr.Prop("a"), QExpr.Constant(3))
    val rhs = QExpr.Lte(QExpr.Prop("a"), QExpr.Constant(4))

    LogicalOptimizer.shrink(lhs, rhs) shouldBe lhs
  }
}
