package oolong.bson

import scala.jdk.CollectionConverters.*
import scala.util.chaining.*

import org.mongodb.scala.bson.*

protected def merge(
    base: BsonValue,
    patch: BsonValue,
    arraySubvalues: Boolean = false
): BsonValue =
  (base, patch) match {
    case (ld: BsonDocument, rd: BsonDocument) =>
      ld.clone().tap { left =>
        rd.forEach { (key, value) =>
          if (left.containsKey(key))
            left.put(key, merge(value, left.get(key)))
          else
            left.put(key, value)
        }
      }
    case (baseArr: BsonArray, patchArr: BsonArray) =>
      val mrgPair = (l: BsonValue, r: BsonValue) => merge(l, r, arraySubvalues = true)

      if (baseArr.size >= patchArr.size)
        BsonArray.fromIterable((baseArr.asScala zip patchArr.asScala).map(mrgPair.tupled))
      else
        BsonArray.fromIterable(
          baseArr.asScala
            .zipAll(patchArr.asScala, BsonNull(), patchArr.asScala.last)
            .map(mrgPair.tupled)
        )
    case (p, BNull()) if arraySubvalues => p
    case (_, p)                         => p
  }
