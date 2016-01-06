package com.czeczotka.scala.fpis.chapter04

import org.specs2.mutable.Specification

class OptionSpec extends Specification {

  "Option" should {

    "exercise 4.1: `map` function should apply `f` if the Option is not None" in {
      (None:Option[Int]).map((i: Int) => 0) shouldEqual None
      Some(2).map((i: Int) => i * i) shouldEqual Some(4)
      Some(-2).map((i: Int) => (i * i).toString) shouldEqual Some("4")
      Some("abc").map(_.length) shouldEqual Some(3)
    }

    "exercise 4.1: `flatMap` function should apply `f` if the Option is not None" in {
      (None:Option[Int]).flatMap((i: Int) => Some(0)) shouldEqual None
      Some(2).flatMap((i: Int) => Some(i * i)) shouldEqual Some(4)
      Some(-2).flatMap((i: Int) => Some((i * i).toString)) shouldEqual Some("4")
      Some("abc").flatMap((s: String) => Some(s.length)) shouldEqual Some(3)
      Some(-2).flatMap((i: Int) => if (i >=  0) Some(Math.sqrt(i)) else None) shouldEqual None
      Some(16).flatMap((i: Int) => if (i >=  0) Some(Math.sqrt(i)) else None) shouldEqual Some(4)
    }
  }
}