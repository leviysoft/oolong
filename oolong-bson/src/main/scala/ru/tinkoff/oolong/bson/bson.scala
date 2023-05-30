package ru.tinkoff.oolong.bson

import java.time.Instant
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.chaining.*

import org.mongodb.scala.bson.*

private type PartialEndo[A]     = PartialFunction[A, A]
private type PartialEndo2[A, B] = PartialFunction[(A, B), (A, B)]

object BUndef:
  def unapply(bu: BsonUndefined): true = true

object BNull:
  def unapply(bn: BsonNull): true = true

object BBoolean:
  def unapply(bb: BsonBoolean): Some[Boolean] = Some(bb.getValue)

object BInt:
  def unapply(bi: BsonInt32): Some[Int] = Some(bi.getValue)

object BLong:
  def unapply(bl: BsonInt64): Some[Long] = Some(bl.getValue)

object BDouble:
  def unapply(bd: BsonDouble): Some[Double] = Some(bd.getValue)

object BDecimal:
  def unapply(bd: BsonDecimal128): Some[BigDecimal] = Some(bd.getValue.bigDecimalValue())

object BString:
  def unapply(bs: BsonString): Some[String] = Some(bs.getValue)

object BDateTime:
  def unapply(bdt: BsonDateTime): Option[Instant] =
    Try(Instant.ofEpochMilli(bdt.asDateTime().getValue)).toOption

object BElement:
  def unapply(be: BsonElement): Some[(String, BsonValue)] = Some((be.key, be.value))

object BArray:
  def unapply(ba: BsonArray): Some[Vector[BsonValue]] = Some(ba.asScala.toVector)

object BDocument:
  def unapply(bd: BsonDocument): Some[Map[String, BsonValue]] = Some(bd.asScala.toMap)

object BObjectId:
  def unapply(boi: BsonObjectId): Some[ObjectId] = Some(boi.getValue)

object BSymbol:
  def unapply(bs: BsonSymbol): Some[String] = Some(bs.getSymbol)

object BJavaScript:
  def unapply(bjs: BsonJavaScript): Some[String] = Some(bjs.getCode)

object BScopedJavaScript:
  def unapply(bjs: BsonJavaScriptWithScope): Some[(String, BsonDocument)] =
    Some(bjs.getCode -> bjs.getScope)

object BRegex:
  def unapply(brx: BsonRegularExpression): Some[(String, String)] =
    Some(brx.getPattern -> brx.getOptions)

extension (doc: BsonDocument.type)
  @inline def apply(element: BsonElement): BsonDocument =
    BsonDocument(element.key -> element.value)

extension (bv: BsonValue)
  /**
   * Merges two bson values
   *
   * bson1 :+ bson2
   *
   * In a case of key collision bson1 values takes priority
   */
  @inline def :+(other: BsonValue): BsonValue = merge(other, bv, false)

  /**
   * Merges two bson values
   *
   * bson1 :+ bson2
   *
   * In a case of key collision bson2 values takes priority
   */
  @inline def +:(other: BsonValue): BsonValue = merge(other, bv, false)

extension (ba: BsonArray)
  def modify(f: PartialEndo[BsonValue]): BsonArray = BsonArray.fromIterable(
    ba.asScala.map(f.applyOrElse(_, identity[BsonValue]))
  )

  def modifyAt(idx: Int, f: PartialEndo[BsonValue]): BsonArray = BsonArray.fromIterable(
    ba.asScala.patch(idx, Seq(f.applyOrElse(ba.get(idx), identity[BsonValue])), 1)
  )

extension (bd: BsonDocument)
  @inline def getFieldOpt(name: String): Option[BsonValue] =
    if (bd.containsKey(name)) Try(bd.get(name)).toOption else None

  def modify(f: PartialEndo2[String, BsonValue]): BsonDocument =
    BsonDocument(
      bd.asScala.map(f.applyOrElse(_, identity[(String, BsonValue)]))
    )

  def modifyValues(f: PartialEndo[BsonValue]): BsonDocument =
    BsonDocument(
      bd.asScala.view.mapValues(f.applyOrElse(_, identity[BsonValue]))
    )

  /**
   * Add or update
   */
  def +!(el: (String, BsonValue)): BsonDocument =
    if (bd.containsKey(el._1))
      bd.clone().tap { doc =>
        val existing = doc.get(el._1)
        doc.put(el._1, merge(existing, el._2, false))
      }
    else
      bd.clone().append(el._1, el._2)
