# Oolong

Oolong - compile-time query generation for document stores.

This library is insipred by [Quill](https://github.com/zio/zio-protoquill).
Everything is implemented with Scala 3 macros. Scala 2 is not supported.
At the moment MongoDB is the only supported document store.

## Community

[Join us on Discord!](https://discord.gg/wjzXb4tEG2)

If you want to contribute please see our [guide for contributors](CONTRIBUTING.md).

## Overview

All query generation is happening at compile-time. This means:
1. Zero runtime overhead. You can enjoy the abstraction without worrying about performance.
2. Debugging is straightforward because generated queries are displayed as compilation messages.

Write your queries as plain Scala lambdas and oolong will translate them into the target representation for your document store:

```scala
import org.mongodb.scala.bson.BsonDocument

import ru.tinkoff.oolong.dsl.*
import ru.tinkoff.oolong.mongo.*

case class Person(name: String, address: Address)

case class Address(city: String)

val q: BsonDocument = query[Person](p => p.name == "Joe" && p.address.city == "Amsterdam")

// The generated query will be displayed during compilation:
// {"$and": [{"name": {"$eq": "Joe"}}, {"address.city": {"$eq": "Amsterdam"}}]}
  
// ... Then you run the query by passing the generated BSON to mongo-scala-driver
```

Updates are also supported:
```scala
val q: BsonDocument = update[Person](_
	.set(_.name, "Alice")
	.inc(_.age, 5)
)
// q is {
// 	$set: { "name": "Alice" },
//	$inc: { "age": 5 }
// }
```

## DSL of oolong

### QueryMeta

In order to rename fields in codecs and queries for type T the instance of QueryMeta[T] should be provided in the scope:

```scala

import org.mongodb.scala.BsonDocument

import ru.tinkoff.oolong.bson.BsonDecoder
import ru.tinkoff.oolong.bson.BsonEncoder
import ru.tinkoff.oolong.bson.given
import ru.tinkoff.oolong.bson.meta.*
import ru.tinkoff.oolong.bson.meta.QueryMeta
import ru.tinkoff.oolong.dsl.*
import ru.tinkoff.oolong.mongo.*

case class Person(name: String, address: Option[Address]) derives BsonEncoder, BsonDecoder

object Person:
  inline given QueryMeta[Person] = queryMeta(_.name -> "lastName")
end Person

case class Address(city: String) derives BsonEncoder, BsonDecoder

val person = Person("Adams", Some(Address("New York")))
val bson: BsonDocument = person.bson.asDocument()
val json = bson.toJson
// json is {"lastName": "Adams", "address": {"city": "New York"}}

//also having QueryMeta[Person] affects filter and update queries:
val q0: BsonDocument = query[Person](_.name == "Johnson")

// The generated query will be:
// {"lastName": "Johnson"}

val q1: BsonDocument = update[Person](_
  .set(_.name, "Brook")
)
// q1 is {
// 	$set: { "lastName": "Brook" },
// }
```

All QueryMeta instances should be inline given instances to be used in macro. 
If they are not given their presence will not have any effect on codecs and queries.
And if they are not inline the error will be thrown during compilation:
```Please, add `inline` to given QueryMeta[T]```

In addition to manual creation of QueryMeta instances, there are several existing instances of QueryMeta:
QueryMeta.snakeCase
QueryMeta.camelCase
QueryMeta.upperCamelCase

Also they can be combined with manual fields renaming:
```scala

import ru.tinkoff.oolong.bson.BsonDecoder
import ru.tinkoff.oolong.bson.BsonEncoder
import ru.tinkoff.oolong.bson.given
import ru.tinkoff.oolong.bson.meta.*
import ru.tinkoff.oolong.bson.meta.QueryMeta

case class Student(firstName: String, lastName: String, previousUniversity: String) derives BsonEncoder, BsonDecoder

object Student:
  inline given QueryMeta[Student] = QueryMeta.snakeCase.withRenaming(_.firstName -> "name")
end Student

val s = Student("Alexander", "Bloom", "MSU")
val bson = s.bson
// bson printed form is: {"name": "Alexander", "last_name": "Bloom", "previous_university": "MSU"}
```

If fields of a class `T` are not renamed, you don't need to provide any instance, even if some other class `U` has  a field of type `T`. 
Macro automatically searches for instances of QueryMeta for all fields, types of which are case classes, and if not found, assumes that fields are not renamed, and then continues doing it recursively 

### Working with Option[_]

When we need to unwrap an `A` from `Option[A]`, we don't use `map` / `flatMap` / etc.
We use `!!` to reduce verbosity: 
```scala
case class Person(name: String, address: Option[Address])

case class Address(city: String)

val q = query[Person](_.address.!!.city == "Amsterdam")
```

Similar to Quill, Oolong provides a quoted DSL, which means that the code you write inside `query(...)` and `update` blocks never gets to execute.
Since we don't have to worry about runtime exceptions, we can tell the compiler to relax and give us the type that we want.

### Raw subquries

If you need to use a feature that's not supported by oolong, you can write the target subquery manually and combine it with the high level query DSL:
```scala
val q = query[Person](_.name == "Joe" && unchecked(
  BsonDocument(Seq(
    ("address.city", BsonDocument(Seq(
      ("$eq", BsonString("Amsterdam"))
    )))
  ))
))
```

### Reusing queries

It's possible to reuse a query by defining an 'inline def':
```scala
inline def cityFilter(doc: Person) = doc.address.!!.city == "Amsterdam"

val q = query[Person](p => p.name == "Joe" && cityFilter(p))
```

## Coming soon

- elasticsearch support
- field renaming 
- aggregation pipelines for Mongo

