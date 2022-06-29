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

### Working with Option[_]

When we need to unwrap an `A` from `Option[A]`, we don't use `map` / `flatMap` / etc.
We use `!!` to reduce verbosity: 
```scala
case class Person(name: String, address: Option[Address])

case class Address(city: String)

val q = query[Person](_.address.!!.city == "Amsterdam")
```

Similar to Quill, Oolong provides a quoted DSL, which means that the code you write inside `query(...)` and `compileUpdate` blocks never gets to execute.
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

### Composing queries

At the moment query composition is only supported via `unchecked`:
```scala
val cityFilter: BsonDocument = query[Person](_.address.!!.city == "Amsterdam")

val q = query[Person](_.name == "Joe" && unchecked(cityFilter))
```

## Coming soon

- better query composition
- elasticsearch support
- field renaming 
- aggregation pipelines for Mongo

