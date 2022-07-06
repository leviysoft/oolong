package ru.tinkoff.oolong.elasticsearch

case class TestClass(
    field1: String,
    field2: Int,
    field3: InnerClass,
    field4: List[Int]
)

case class InnerClass(
    innerField: String,
    optionalInnerField: Option[Int]
)
