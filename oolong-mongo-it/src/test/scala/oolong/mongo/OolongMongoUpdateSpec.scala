package oolong.mongo

import scala.concurrent.Await
import scala.concurrent.ExecutionContext

import com.dimafeng.testcontainers.ForAllTestContainer
import com.dimafeng.testcontainers.MongoDBContainer
import concurrent.duration.DurationInt
import oolong.dsl.*
import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.model.UpdateOptions
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AsyncFlatSpec

class OolongMongoUpdateSpec extends AsyncFlatSpec with ForAllTestContainer with BeforeAndAfterAll {

  override val container: MongoDBContainer = MongoDBContainer()
  container.start()

  val client     = MongoClient(container.replicaSetUrl)
  val collection = client.getDatabase("test").getCollection[BsonDocument]("testCollection")

  override def beforeAll(): Unit = {
    val documents = List(
      TestClass("0", 0, InnerClass("sdf"), List(1, 2), None),
      TestClass("1", 1, InnerClass("qwe"), Nil, Some(2L)),
      TestClass("2", 2, InnerClass("asd"), Nil, None),
      TestClass("3", 12, InnerClass("sdf"), Nil, None),
      TestClass("12345", 12, InnerClass("sdf"), Nil, None),
    )

    implicit val ec = ExecutionContext.global

    val f = for {
      _ <- client.getDatabase("test").createCollection("testCollection").head()
      _ <- collection.insertMany(documents.map(_.bson.asDocument())).head()
    } yield ()

    Await.result(f, 30.seconds)
  }

  it should "update with $inc" in {
    for {
      res <- collection
        .updateOne(
          query[TestClass](_.field1 == "0"),
          update[TestClass](_.inc(_.field2, 1))
        )
        .head()
    } yield assert(res.wasAcknowledged() && res.getMatchedCount == 1 && res.getModifiedCount == 1)
  }

  it should "update with $min" in {
    for {
      res <- collection
        .updateOne(
          query[TestClass](_.field1 == "3"),
          update[TestClass](_.min(_.field2, 1))
        )
        .head()
    } yield assert(res.wasAcknowledged() && res.getMatchedCount == 1 && res.getModifiedCount == 1)
  }

  it should "update with $max" in {
    for {
      res <- collection
        .updateOne(
          query[TestClass](_.field1 == "0"),
          update[TestClass](_.max(_.field2, 10))
        )
        .head()
    } yield assert(res.wasAcknowledged() && res.getMatchedCount == 1 && res.getModifiedCount == 1)
  }

  it should "update with $mul" in {
    for {
      res <- collection
        .updateOne(
          query[TestClass](_.field1 == "0"),
          update[TestClass](_.mul(_.field2, 10))
        )
        .head()
    } yield assert(res.wasAcknowledged() && res.getMatchedCount == 1 && res.getModifiedCount == 1)
  }

  it should "update with $rename" in {
    for {
      res <- collection
        .updateOne(
          query[TestClass](_.field1 == "12345"),
          update[TestClass](_.rename(_.field2, "NewField2"))
        )
        .head()
    } yield assert(res.wasAcknowledged() && res.getMatchedCount == 1 && res.getModifiedCount == 1)
  }

  it should "update with $set" in {
    for {
      res <- collection
        .updateOne(
          query[TestClass](_.field1 == "0"),
          update[TestClass](_.set(_.field5.!!, 2L))
        )
        .head()
    } yield assert(res.wasAcknowledged() && res.getMatchedCount == 1 && res.getModifiedCount == 1)
  }

  it should "not update existing document with $senOnInsert" in {
    for {
      res <- collection
        .updateOne(
          query[TestClass](_.field1 == "0"),
          update[TestClass](_.setOnInsert(_.field2, 2)),
          (new UpdateOptions).upsert(true)
        )
        .head()
    } yield assert(res.wasAcknowledged() && res.getMatchedCount == 1 && res.getModifiedCount == 0)
  }

  it should "update with $unset" in {
    for {
      res <- collection
        .updateOne(
          query[TestClass](_.field1 == "1"),
          update[TestClass](_.unset(_.field5))
        )
        .head()
    } yield assert(res.wasAcknowledged() && res.getMatchedCount == 1 && res.getModifiedCount == 1)
  }

}
