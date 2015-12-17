package com.czeczotka.scala.fpis.package03

import com.czeczotka.scala.fpis.package03.List._
import org.specs2.mutable.Specification

class FunctionalDataStructuresSpec extends Specification {

  "List implementation" should {
    "exercise 3.1: pattern match to the value of 3 " in {
      patternMatched should equalTo(3)
    }

    "exercise 3.2: `tail` function should remove the first element of a List" in {
      tail(Nil) should equalTo(Nil)
      tail(Cons("hello", Nil)) should equalTo(Nil)
      tail(Cons("hello", Cons("world", Nil))) should equalTo(Cons("world", Nil))
      tail(Cons("hello", Cons("world", Cons("!", Nil)))) should equalTo(Cons("world", Cons("!", Nil)))
    }
  }
}