package oolong.mongo

import scala.concurrent.Await
import scala.concurrent.ExecutionContext

import com.dimafeng.testcontainers.ForAllTestContainer
import com.dimafeng.testcontainers.MongoDBContainer
import com.mongodb.client.model.FindOneAndUpdateOptions
import com.mongodb.client.model.ReturnDocument
import concurrent.duration.DurationInt
import oolong.bson.*
import oolong.bson.given
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
      TestClass("0", 0, InnerClass("sdf", 2.0), List(1, 2), None, List.empty),
      TestClass("1", 1, InnerClass("qwe", 3), Nil, Some(2L), List.empty),
      TestClass("2", 2, InnerClass("asd", 0), Nil, None, List.empty),
      TestClass("3", 12, InnerClass("sdf", 1), Nil, None, List.empty),
      TestClass("4", 13, InnerClass("sdf", 1), List(1), None, List.empty),
      TestClass("12345", 12, InnerClass("sdf", 11), Nil, None, List.empty),
      TestClass("popHead", 1, InnerClass("popHead", 1), List(1, 2, 3), None, List.empty),
      TestClass("popTail", 1, InnerClass("popTail", 1), List(1, 2, 3), None, List.empty),
      TestClass("pullAll", 1, InnerClass("pullAll", 1), List(1, 2, 3), None, List(InnerClass("1", 1), InnerClass("2", 2))),
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

  it should "update with $addToSet" in {
    for {
      upd <- collection
        .findOneAndUpdate(
          query[TestClass](_.field1 == "0"),
          update[TestClass](
            _.addToSet(_.field4, 3)
          ),
          new FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER)
        )
        .head()
        .map(BsonDecoder[TestClass].fromBson(_).get)

    } yield assert(
      upd.field4 == List(1, 2, 3)
    )
  }

  it should "update with $addToSet using $each" in {
    for {
      upd <- collection
        .findOneAndUpdate(
          query[TestClass](_.field1 == "4"),
          update[TestClass](
            _.addToSetAll(_.field4, List(2, 3))
          ),
          new FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER)
        )
        .head()
        .map(BsonDecoder[TestClass].fromBson(_).get)
    } yield assert(
      upd.field4 == List(1, 2, 3)
    )
  }

  it should "$pop the fist element" in {
    for {
      upd <- collection
        .findOneAndUpdate(
          query[TestClass](_.field1 == "popHead"),
          update[TestClass](
            _.popHead(_.field4)
          ),
          new FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER)
        )
        .head()
        .map(BsonDecoder[TestClass].fromBson(_).get)
    } yield assert(
      upd.field4 == List(2, 3)
    )
  }

  it should "$pop the last element" in {
    for {
      upd <- collection
        .findOneAndUpdate(
          query[TestClass](_.field1 == "popTail"),
          update[TestClass](
            _.popLast(_.field4)
          ),
          new FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER)
        )
        .head()
        .map(BsonDecoder[TestClass].fromBson(_).get)
    } yield assert(
      upd.field4 == List(1, 2)
    )
  }

  it should "$pulAll primitive elements" in {
    for {
      upd <- collection
        .findOneAndUpdate(
          query[TestClass](_.field1 == "pullAll"),
          update[TestClass](
            _.pullAll(_.field4, List(1, 100))
          ),
          new FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER)
        )
        .head()
        .map(BsonDecoder[TestClass].fromBson(_).get)

    } yield assert(
      upd.field4 == List(2, 3)
    )
  }

  it should "$pulAll documents" in {
    for {
      upd <- collection
        .findOneAndUpdate(
          query[TestClass](_.field1 == "pullAll"),
          update[TestClass](
            _.pullAll(_.field6, lift(List(InnerClass("1", 1)))) // TODO: fix lift for update
          ),
          new FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER)
        )
        .head()
        .map(BsonDecoder[TestClass].fromBson(_).get)
    } yield assert(
      upd.field6 == List(InnerClass("2", 2))
    )
  }

}
