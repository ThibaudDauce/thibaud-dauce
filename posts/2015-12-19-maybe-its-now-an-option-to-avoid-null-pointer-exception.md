---
title: Maybe it's now an option to avoid NullPointerException?
image: /images/schroedinger.jpg
thumbnail: /images/thumbnail-schroedinger.jpg
---

`NullPointerException` is an obvious runtime problem in most Java applications. But it's not limited to Java, in PHP for example we could get a "Trying to get property of non-object". But today, there's a compilation alternative to this runtime problem.

<!--more-->

![Maybe Cat | Option[Cat] | Optional\<Cat\>](/images/schroedinger.jpg)

Scala is a modern functional (and Object Oriented) language based on the JVM. I did a lot of Scala during the past year, mostly with the Spark framework for distributed computing. In Java, if you try to access a element in a `Map<String, Cat>` for example, [the signature](http://docs.oracle.com/javase/7/docs/api/java/util/Map.html#get%28java.lang.Object%29) will look like this:
```java
/**
 * Returns the value to which the specified key is mapped, or null if this map contains no mapping for the key.
 *
 * @return Cat or null
*/
public Cat get(String key);
```

And it's correct (for the compiler) to write:
```java
this.get("Schroedinger").miaou();
```

If the `Cat` is here, it's fine. If he's not, the program crash with a `NullPointerException`. The compiler will not help you to catch this kind of bugs.

In Scala [the signature](http://www.scala-lang.org/api/current/index.html#scala.collection.Map) for a type `Map[String, Cat]` is:
```scala
def get(key: String): Option[Cat]
```

So you cannot write something like before. The compiler will not allow it because the object `Option[Cat]` doesn't have a `miaou` method. So, how did we manage this? We use pattern matching to check the `None` option. Indeed, the `Option[Map]` type is divided into two types: `Some[Cat]` and `None`.
```scala
this.get("Schroedinger") match {
  Some(cat) => cat.miaou()
  None      => // do something
}
```

Scala is not a strict language so it's possible to get the content of an option or a `null` with the `get` method on `Option[Cat]`:
```scala
// throw a NullPointerException
this.get("Schroedinger").get.miaou()
```

This concept of optional type come from functional programming. The last version of Java introduces a lot of functional programming stuff, it's now possible to use [the new type](http://docs.oracle.com/javase/8/docs/api/java/util/Optional.html): `Optional<Cat>`.

And, last but not least, it's not a new hype feature from 2015, some great (and old) languages like Haskell has [the `Maybe` type](http://haddock.stackage.org/lts-3.18/base-4.8.1.0/Prelude.html#t:Maybe) for a long time. `Maybe Cat` could be `Just Cat` or `Nothing`.

----
In conclusion, if you want to know more about this amazing functionality:

* Scala explanations of `Option` type: [http://www.scala-lang.org/api/2.8.1/scala/Option.html](http://www.scala-lang.org/api/2.8.1/scala/Option.html)
* Haskell explanations of `Maybe` type: [http://learnyouahaskell.com/a-fistful-of-monads#getting-our-feet-wet-with-maybe](http://learnyouahaskell.com/a-fistful-of-monads#getting-our-feet-wet-with-maybe)
* Rust explanations of `Option` type: [https://doc.rust-lang.org/std/option/index.html](https://doc.rust-lang.org/std/option/index.html)
