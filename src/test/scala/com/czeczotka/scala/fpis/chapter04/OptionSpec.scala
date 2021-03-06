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

    "exercise 4.1: `getOrElse` function returns the result inside the Option if defined, or the given default value otherwise" in {
      (None:Option[Int]).getOrElse(0) shouldEqual 0
      (None:Option[String]).getOrElse("") shouldEqual ""
      Some(2).getOrElse(1) shouldEqual 2
      Some("abc").getOrElse("") shouldEqual "abc"
    }

    "exercise 4.1: `orElse` function returns the Option if defined, or the given default value otherwise" in {
      (None:Option[Int]).orElse(Some(0)) shouldEqual Some(0)
      (None:Option[String]).getOrElse(Some("")) shouldEqual Some("")
      Some(2).orElse(Some(1)) shouldEqual Some(2)
      Some("abc").orElse(Some("")) shouldEqual Some("abc")
    }

    "exercise 4.1: `filter` function should convert Some to None if the value doesn't satisfy the predicate" in {
      (None:Option[Int]).filter(_ > 0) shouldEqual None
      Some(0).filter(_ > 0) shouldEqual None
      Some(2).filter(_ > 0) shouldEqual Some(2)
      Some("abc").filter(!_.isEmpty) shouldEqual Some("abc")
    }
  }
}