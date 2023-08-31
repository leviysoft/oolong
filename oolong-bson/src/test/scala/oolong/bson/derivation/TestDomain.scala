package oolong.bson.derivation

import java.time.Instant
import java.time.Year

import oolong.bson.BsonDecoder
import oolong.bson.BsonEncoder
import oolong.bson.given
import oolong.bson.meta.QueryMeta
import oolong.bson.meta.queryMeta

case class TestMeta(time: Instant, seq: Long, flag: Boolean) derives BsonEncoder, BsonDecoder

case class TestCheck(year: Year, comment: String) derives BsonEncoder, BsonDecoder

case class TestEntity(
    id: Int,
    name: String,
    meta: TestMeta,
    comment: Option[String],
    linkId: Option[Int],
    checks: Seq[TestCheck]
) derives BsonEncoder,
      BsonDecoder

object TestEntity {
  inline given QueryMeta[TestEntity] = queryMeta(_.id -> "_id")
}

case class TestContainer[T](value: Option[T]) derives BsonEncoder, BsonDecoder

case class TestEntityWithDefaults(
    id: Int,
    name: String = "test",
    meta: TestMeta,
    comment: Option[String],
    linkId: Option[Int],
    checks: Seq[TestCheck] = Seq()
) derives BsonEncoder,
      BsonDecoder

object TestEntityWithDefaults {
  inline given QueryMeta[TestEntityWithDefaults] = queryMeta(_.id -> "_id")
}

case class XXXCaseClass(
    a: Int,
    b: Int,
    c: Int,
    d: Int,
    e: Int,
    f: Int,
    g: Int,
    h: Int,
    i: Int,
    j: Int,
    k: Int,
    l: Int,
    m: Int,
    n: Int,
    o: Int,
    p: Int,
    q: Int,
    r: Int,
    s: Int,
    t: Int,
    u: Int,
    v: Int,
    w: Int,
    x: Int,
    y: Int,
    z: Int
) derives BsonEncoder,
      BsonDecoder
