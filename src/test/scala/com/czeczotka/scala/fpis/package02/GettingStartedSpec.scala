package com.czeczotka.scala.fpis.package02

import org.specs2.Specification

class GettingStartedSpec extends Specification {def is = s2"""

 Function 'factorial' should
   return 1 for -1               $oneForMinusOne
   return 1 for 0                $oneForZero
   return 1 for 1                $oneForOne
   return 6 for 3                $factorialThree
   return 120 for 5              $factorialFive
                                 """
  val gs = new GettingStarted
  def oneForMinusOne = gs.factorial(-1) must_== 1
  def oneForZero = gs.factorial(0) must_== 1
  def oneForOne =  gs.factorial(1) must_== 1
  def factorialThree = gs.factorial(3) must_== 6
  def factorialFive = gs.factorial(5) must_== 120

}