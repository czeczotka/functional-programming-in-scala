package com.czeczotka.scala.fpis.package03

import com.czeczotka.scala.fpis.package03.List.{patternMatched, tail, setHead}
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
  }
}