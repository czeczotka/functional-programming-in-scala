package com.czeczotka.scala.fpis.package03

import com.czeczotka.scala.fpis.package03.List.patternMatched
import org.specs2.mutable.Specification

class FunctionalDataStructuresSpec extends Specification {

  "List implementation" should {
    "exercise 3.1: pattern match to the value of 3 " in {
      patternMatched should equalTo(3)
    }
  }
}