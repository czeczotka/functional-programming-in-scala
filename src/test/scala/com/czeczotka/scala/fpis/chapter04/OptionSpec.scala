package com.czeczotka.scala.fpis.chapter04

import org.specs2.mutable.Specification

class OptionSpec extends Specification {

  "Option" should {
    "exercise 4.1: `map` method should apply `f` if the Option is not None" in {
      (None:Option[Int]).map((i: Int) => 0) shouldEqual None
      Some(2).map((i: Int) => i * i) shouldEqual Some(4)
      Some(-2).map((i: Int) => (i * i).toString) shouldEqual Some("4")
      Some("abc").map(_.length) shouldEqual Some(3)
    }
  }
}