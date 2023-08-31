package oolong.mongo

import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.util.Failure
import scala.util.Random
import scala.util.Success

import com.dimafeng.testcontainers.ForAllTestContainer
import com.dimafeng.testcontainers.MongoDBContainer
import oolong.bson.*
import oolong.bson.given
import oolong.dsl.*
import oolong.mongo.MongoType
import org.mongodb.scala.MongoClient
import org.mongodb.scala.ObservableFuture
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.bson.BsonInt32
import org.mongodb.scala.bson.BsonString
import org.scalatest.BeforeAndAfterAll
import org.scalatest.compatible.Assertion
import org.scalatest.flatspec.AsyncFlatSpec

class OolongMongoSpec extends AsyncFlatSpec with ForAllTestContainer with BeforeAndAfterAll {

  override val container: MongoDBContainer = MongoDBContainer()
  container.start()

  val client     = MongoClient(container.replicaSetUrl)
  val collection = client.getDatabase("test").getCollection[BsonDocument]("testColection")

  override def beforeAll(): Unit = {
    val documents = List(
      TestClass("0", 0, InnerClass("sdf"), Nil),
      TestClass("1", 1, InnerClass("qwe"), Nil),
      TestClass("2", 2, InnerClass("asd"), Nil),
      TestClass("3", 12, InnerClass("sdf"), Nil)
    )

    implicit val ec = ExecutionContext.global

    val f = for {
      _ <- client.getDatabase("test").createCollection("testCollection").head()
      _ <- collection.insertMany(documents.map(_.bson.asDocument())).head()
    } yield ()

    Await.result(f, 30.seconds)
  }

  it should "find document in a collection with query with compile-time constant" in {
    for {
      res <- collection.find(query[TestClass](_.field1 == "1")).head()
      v <- BsonDecoder[TestClass].fromBson(res) match {
        case Failure(exception) => Future.failed(exception)
        case Success(value)     => Future.successful(value)
      }
    } yield assert(v == TestClass("1", 1, InnerClass("qwe"), Nil))
  }

  it should "find documents in a collection with query with runtime constant" in {
    val q = query[TestClass](_.field2 <= lift(Random.between(13, 100)))
    for {
      res <- collection.find(q).toFuture()
    } yield assert(res.size == 4)
  }

  it should "find both documents with OR operator" in {
    val q = query[TestClass](x => x.field2 == 1 || x.field2 == 2)
    for {
      res <- collection.find(q).toFuture()
    } yield assert(res.size == 2)
  }

  it should "compile queries with >= and <=" in {
    val q = query[TestClass](x => x.field2 >= 2 && x.field2 <= 10)
    for {
      res <- collection.find(q).toFuture()
    } yield assert(res.size == 1)
  }

  it should "compile when called inside a function" in {

    def makeQuery(id: Int) = query[TestClass](_.field2 == lift(id))

    for {
      res <- collection.find(makeQuery(1)).toFuture()
    } yield assert(res.size == 1)
  }

  it should "compile queries with `!`" in {
    val q = query[TestClass](x => !(x.field2 == 100500))

    for {
      res <- collection.find(q).toFuture()
    } yield assert(res.size == 4)
  }

  it should "compile queries with `unchecked`" in {
    // format: off
    val q = query[TestClass](_.field1 == "100500" || unchecked(
      BsonDocument(Seq(
        ("field2", BsonDocument(Seq(
          ("$eq", BsonInt32(1))
        )))
      ))
    ))
    // format: on

    for {
      res <- collection.find(q).toFuture()
    } yield assert(res.size == 1)
  }

  it should "compile queries with `.contains` #1" in {
    val q = query[TestClass](x => lift(Set(2, 3)).contains(x.field2))

    for {
      res <- collection.find(q).toFuture()
    } yield assert(res.size == 1)
  }

  it should "compile queries with `.contains` #2" in {
    val q = query[TestClass](x => lift(Set(1, 2, 3).filter(_ >= 2)).contains(x.field2))

    for {
      res <- collection.find(q).toFuture()
    } yield assert(res.size == 1)
  }

  it should "compile queries with `.contains` #3" in {
    val q = query[TestClass](x => lift(Set(InnerClass("qwe"), InnerClass("asd"))).contains(x.field3))

    for {
      res <- collection.find(q).toFuture()
    } yield assert(res.size == 2)
  }

  it should "compile queries with nested objects" in {
    val q = query[TestClass](_.field3 == lift(InnerClass("qwe")))

    for {
      res <- collection.find(q).toFuture()
    } yield assert(res.size == 1)
  }

  it should "compile queries with `.isInstance` #1" in {
    val q = query[TestClass](_.field2.isInstanceOf[MongoType.INT32])

    for {
      res <- collection.find(q).toFuture()
    } yield assert(res.size == 4)
  }

  it should "compile queries with `.isInstance` #2" in {
    val q = query[TestClass](_.field3.isInstanceOf[MongoType.DOCUMENT])

    for {
      res <- collection.find(q).toFuture()
    } yield assert(res.size == 4)
  }

  it should "compile queries with `.mod` #1" in {
    val q = query[TestClass](_.field2.mod(4, 0))

    for {
      res <- collection.find(q).toFuture()
    } yield assert(res.size == 2)
  }

  it should "compile queries with `.mod` #2" in {
    val q = query[TestClass](_.field2.mod(4.99, 0))

    for {
      res <- collection.find(q).toFuture()
    } yield assert(res.size == 2)
  }

  // TODO: test updates
//  it should "compile updates" in {
//    val upd = compileUpdate {
//      update[CompanySuccess]
//        .set(_.from, 2)
//        .set(_.field4, liftU(Field("qweasd")))
//    }
//  }

}
