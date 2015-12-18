package com.czeczotka.scala.fpis.package03

import com.czeczotka.scala.fpis.package03.List.{patternMatched, tail, setHead, drop, dropWhile}
import org.specs2.mutable.Specification

class FunctionalDataStructuresSpec extends Specification {

  "List implementation" should {
    "exercise 3.1: pattern match to the value of 3 " in {
      patternMatched should equalTo(3)
    }

    "exercise 3.2: `tail` function should remove the first element of a List" in {
      tail(Nil) should equalTo(Nil)
      tail(List("hello")) should equalTo(Nil)
      tail(List("hello", "world")) should equalTo(List("world"))
      tail(List("hello", "world", "!")) should equalTo(List("world", "!"))
    }

    "exercise 3.3: `setHead` function should replace the first element of a List with a different value" in {
      setHead("value", Nil) should equalTo(Nil)
      setHead("value", List("hello")) should equalTo(List("value"))
      setHead("value", List("hello", "world")) should equalTo(List("value", "world"))
      setHead("value", List("hello", "world", "!")) should equalTo(List("value", "world", "!"))
    }

    "exercise 3.4: `drop` function should remove n first elements of a List" in {
      drop(1, Nil) should equalTo(Nil)
      drop(1, List("hello")) should equalTo(Nil)
      drop(2, List("hello", "world")) should equalTo(Nil)
      drop(2, List("hello", "world", "!")) should equalTo(List("!"))
      drop(3, List("hello", "world", "!")) should equalTo(Nil)
      drop(1, Nil) should equalTo(Nil)
    }

    "exercise 3.5: `dropWhile` function should remove elements from the List prefix as long as they match a predicate" in {
      dropWhile(Nil, (a: Any) => true) should equalTo(Nil)
      dropWhile(Nil, (a: Any) => false) should equalTo(Nil)
      dropWhile(List("hello"), (s: String) => s.isEmpty) should equalTo(List("hello"))
      dropWhile(List("hello"), (s: String) => !s.isEmpty) should equalTo(Nil)
      dropWhile(List("hello", "world"), (s: String) => s == "hello") should equalTo(List("world"))
      dropWhile(List("hello", "world"), (s: String) => s == "world") should equalTo(List("hello", "world"))
      dropWhile(List("hello", "world", "!"), (s: String) => s.length > 1) should equalTo(List("!"))
      dropWhile(List("hello", "world", "!"), (s: String) => s.length > 0) should equalTo(Nil)
      dropWhile(List(1, 2, 3, 4, 5), (i: Int) => i < 4) should equalTo(List(4, 5))
    }
  }
}