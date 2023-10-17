# Oolong

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.github.leviysoft/oolong-core_3/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.leviysoft/oolong-core_3)

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

import oolong.dsl.*
import oolong.mongo.*

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

### Supported MongoDB operators 

#### Query operators

I Comparison query operators
1. $eq

```scala
import oolong.dsl.*
import oolong.mongo.*

case class Person(name: String, age: Int, email: Option[String])

val q = query[Person](_.name == "John")
// q is {"name": "John"}
```
In oolong $eq query is transformed into its implicit form: `{ field: <value> }`, except when a field is queried more than once.

2. $gt

```scala
val q = query[Person](_.age > 18)
// q is {"age": {"$gt": 18}}
```

3. $gte

```scala
val q = query[Person](_.age >= 18)
// q is {"age": {"$gte": 18}}
```

4. $in

```scala
val q = query[Person](p => List(18, 19, 20).contains(p.age))
// q is {"age": {"$in": [18, 19, 20]}}
```

5. $lt

```scala
val q = query[Person](_.age < 18)
// q is {"age": {"$lt": 18}}
```

6. $lte

```scala
val q = query[Person](_.age <= 18)
// q is {"age": {"$lte": 18}}
```

7. $ne

```scala
val q = query[Person](_.name != "John")
// q is {"name" : {"$ne": "John"}}
```

8. $nin

```scala
val q = query[Person](p => !List(18, 19, 20).contains(p.age))
// q is {"age": {"$nin": [18, 19, 20]}}
```

9. $type

```scala
val q = query[Person](_.age.isInstance[MongoType.INT32])
// q is {"age": { "$type": 16 }}
```

10. $mod

```scala
val q = query[Person](_.age % 4.5 == 2)
// q is {"age": {"$mod": [4.5, 2]}}
```

Also `$mod` is supported if `%` is defined in extension:

```scala 3
trait NewType[T](using ev: Numeric[T]):
  opaque type Type = T
  given Numeric[Type] = ev
  extension (nt: Type) def value: T = nt

object Number extends NewType[Int]:
  extension (self: Number) def %(a: Int): Int = self.value % a
type Number = Number.Type

case class Human(age: Number)
val q    = query[Human](_.age % 2 == 2)
// q is {"age": {"$mod": [2, 2]}}
```

II Logical query operators

1. $and

```scala
val q = query[Person](p => p.name == "John" && p.age >= 18)
// q is {"name" : "John", "age": {"$gte": 18}}
```
If we query different fields the query is simplified as above. 

```scala
//However, should we query the same field twice, we would observe the form with $and
val q = query[Person](p => p.age != 33 && p.age >= 18)
// q is {"$and": [{"age": {"$ne": 33}}, {"age": {"$gte": 18}]}
```
2. $or

```scala
val q = query[Person](p => p.age != 33 || p.age >= 18)
// q is {"or": [{"age": {"$ne": 33}}, {"age": {"$gte": 18}]}
```

3. $not

```scala
val q = query[Person](p => !(p.age < 18))
// q is { "age": { "$not": { "$lt": 18 } } }
```

III Element Query Operators

1. $exists
```scala
val q = query[Person](_.email.isDefined)
// q is { "email": { "$exists": true } }
val q1 = query[Person](_.email.nonEmpty)
// q1 is { "email": { "$exists": true } }
val q2 = query[Person](_.email.isEmpty)
// q2 is { "email": { "$exists": false } }
```

IV Evaluation Query Operators

1. $regex

There are 4 ways to make a $regex query, that are supported in oolong, which are:
```scala
import java.util.regex.Pattern

val q = query[Person](_.email.!!.matches("(?ix)^[\\w-\\.]+@([\\w-]+\\.)+[\\w-]{2,4}$"))
//q is {"email": {"$regex": "^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$", "$options": "ix"} 
val q1 = query[Person](p => Pattern.compile("(?ix)^[\\w-\\.]+@([\\w-]+\\.)+[\\w-]{2,4}$").matcher(p.email.!!).matches())
//q1 is {"email": {"$regex": "^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$", "$options": "ix"}
val q2 = query[Person](p => Pattern.compile("^[\\w-\\.]+@([\\w-]+\\.)+[\\w-]{2,4}$", Pattern.CASE_INSENSITIVE | Pattern.COMMENTS).matcher(p.email.!!).matches())
//q2 is {"email": {"$regex": "^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$", "$options": "ix"}
val q3 = query[Person](p => Pattern.matches("^[\\w-\\.]+@([\\w-]+\\.)+[\\w-]{2,4}$", p.email.!!))
//q3 is {"email": {"$regex": "^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$"}
```

V Array Query Operators

1. $size

```scala
import oolong.dsl.*

case class Course(studentNames: List[String])

val q = query[Course](_.studentNames.size == 20)
val q = query[Course](_.studentNames.length == 20)
// q is {"studentNames": {"$size": 20}}
```

2. $elemMatch

```scala
import oolong.dsl.*

case class Course(studentNames: List[String], tutor: String)

val q = query[Course](_.studentNames.exists(_ == 20)) // $elemMatch ommited when querying single field
// q is {"studentNames": 20}

val q = query[Course](course => course.studentNames.exists(_ > 20) && course.tutor == "Pavlov")
// q is {"studentNames": {"$elemMatch": {"studentNames": {"$gt": 20}, "tutor": "Pavlov"}}}

```

3. $all

```scala
case class LotteryTicket(numbers: List[Int])

inline def winningNumbers = List(4, 8, 15, 16, 23, 42)

val q    = query[LotteryTicket](lt => winningNumbers.forall(lt.numbers.contains))
// q is { "numbers": { "$all": [4, 8, 15, 16, 23, 42] } }
```

$all with $elemMatch
```scala
case class LotteryTicket(numbers: List[Int], series: Long)

case class LotteryTickets(tickets: Vector[LotteryTicket])

val q = query[LotteryTickets](lts =>
  lts.tickets.exists(_.numbers.size == 20) && lts.tickets.exists(ticket =>
    ticket.numbers.size == 10 && ticket.series == 99L
  )
)
// q is { "tickets": { "$all": [{ "$elemMatch": { "numbers": { "$size": 20 } } }, { "$elemMatch": { "numbers": { "$size": 10 }, "series": 99 } }] } }
```

#### Update operators

I Field Update Operators

1. $inc
```scala
import oolong.dsl.*
import oolong.mongo.*

case class Observation(count: Int, result: Long, name: String, threshold: Option[Int])

val q = update[Observation](_.inc(_.count, 1))
// q is {"$set": {"count": 1}}
```

2. $min
```scala
val q = update[Observation](_.min(_.result, 1))
// q is {"$min": {"result": 1}}
```

3. $max
```scala
val q = update[Observation](_.max(_.result, 10))
// q is {"$min": {"result": 1}}
```

4. $mul
```scala
val q = update[Observation](_.mul(_.result, 2))
// q is {"$mul": {"result": 2}}
```

5. $rename
```scala
val q = update[Observation](_.rename(_.name, "tag"))
// q is {"$rename": {"name": "tag"}}
```

6. $set
```scala
val q = update[Observation](_.set(_.count, 0))
// q is {"$set": {"count": 0}}
```

7. $set
```scala
val q = update[Observation](_.set(_.count, 0))
// q is {"$set": {"count": 0}}
```

7. $set
```scala
val q = update[Observation](_.setOnInsert(_.threshold, 100))
// q is {"$setOnInsert": {"threshold": 100}}
```

8. $unset

$unset can be used only to set None on Option fields
```scala
val q = update[Observation](_.unset(_.threshold))
// q is {"$unset": {"threshold": ""}}
```
#### Projection

```scala 3
case class Passport(number: String, issueDate: LocalDate)
case class BirthInfo(country: String, date: LocalDate)
case class Student(name: String, lastName: String, passport: Passport, birthInfo: BirthInfo)

case class StudentDTO(name: String, lastName: String)
case class PassportDTO(number: String, issueDate: LocalDate)
case class BirthDateDTO(country: String, date: LocalDate)

val proj = projection[Student, StudentDTO]
// proj is {"name": 1, "birthInfo.date": 1, "passport": 1, "lastName": 1}
```


### QueryMeta

In order to rename fields in codecs and queries for type T the instance of QueryMeta[T] should be provided in the scope:

```scala 3

import org.mongodb.scala.BsonDocument

import oolong.bson.BsonDecoder
import oolong.bson.BsonEncoder
import oolong.bson.given
import oolong.bson.meta.*
import oolong.bson.meta.QueryMeta
import oolong.dsl.*
import oolong.mongo.*

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
```scala 3

import oolong.bson.BsonDecoder
import oolong.bson.BsonEncoder
import oolong.bson.given
import oolong.bson.meta.*
import oolong.bson.meta.QueryMeta

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
- aggregation pipelines for Mongo

