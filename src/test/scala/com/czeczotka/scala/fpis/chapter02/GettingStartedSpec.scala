package com.czeczotka.scala.fpis.chapter02

import org.specs2.Specification

class GettingStartedSpec extends Specification {def is = s2"""

 Function 'factorial' should return
   factorial(-1) =  1             ${gs.factorial(-1) must_== 1}
   factorial(0)  =  1             ${gs.factorial(0) must_== 1}
   factorial(1)  =  1             ${gs.factorial(1) must_== 1}
   factorial(3)  =  6             ${gs.factorial(3) must_== 6}
   factorial(5)  =  120           ${gs.factorial(5) must_== 120}

 Function 'fibonacci' should return
   fibonacci(-1) =  0             ${gs.fibonacci(-1) must be equalTo 0}
   fibonacci(0)  =  0             ${gs.fibonacci(0)  must be equalTo 0}
   fibonacci(1)  =  1             ${gs.fibonacci(1)  must be equalTo 1}
   fibonacci(2)  =  1             ${gs.fibonacci(2)  must be equalTo 1}
   fibonacci(3)  =  2             ${gs.fibonacci(3)  must be equalTo 2}
   fibonacci(4)  =  3             ${gs.fibonacci(4)  must be equalTo 3}
   fibonacci(5)  =  5             ${gs.fibonacci(5)  must be equalTo 5}
   fibonacci(6)  =  8             ${gs.fibonacci(6)  must be equalTo 8}
   fibonacci(7)  =  13            ${gs.fibonacci(7)  must be equalTo 13}

 Function 'isSorted' should return
   true for an empty array              ${gs.isSorted(emptyArray,    greaterThan) must beTrue}
   true for a array with one element    ${gs.isSorted(oneElemArray,  greaterThan) must beTrue}
   true for a sorted array              ${gs.isSorted(sortedArray,   greaterThan) must beTrue}
   false for an unsorted array          ${gs.isSorted(unsortedArray, greaterThan) must beFalse}
                                 """

  val gs = new GettingStarted

  val emptyArray = Array[Int]()
  val oneElemArray = Array[Int](1)
  val sortedArray = Array(8, 6, 4, 2, 1)
  val unsortedArray = Array(4, 2, 6, 8, 1)
  val greaterThan: (Int, Int) => Boolean = _ > _

}