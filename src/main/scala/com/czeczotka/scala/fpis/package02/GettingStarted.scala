package com.czeczotka.scala.fpis.package02

object GettingStarted {

  val gs = new GettingStarted

  def main(args: Array[String]) {
    example_241_factorial()
    exercise21_fibonacci()
    exercise22_isSorted()
  }

  def example_241_factorial() {
    val num = 5
    println(s"factorial($num) = ${gs.factorial(num)}")
  }

  def exercise21_fibonacci() {
    val num = 10
    println(s"fibonacci($num) = ${gs.fibonacci(num)}")
  }

  def exercise22_isSorted() {
    val emptyArray = Array[Int]()
    val sortedArray = Array(8, 6, 4, 2, 1)
    val unsortedArray = Array(4, 2, 6, 8, 1)
    println(s"isSorted(${emptyArray.toBuffer}) = ${gs.isSorted(emptyArray, (x: Int, y: Int) => x > y)}")
    println(s"isSorted(${sortedArray.toBuffer}) = ${gs.isSorted(sortedArray, (x: Int, y: Int) => x > y)}")
    println(s"isSorted(${unsortedArray.toBuffer}) = ${gs.isSorted(unsortedArray, (x: Int, y: Int) => x > y)}")
  }
}

class GettingStarted {

  def isSorted[A](array: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(index: Int): Boolean = {
      if (index + 1 >= array.length) true
      else if (ordered(array(index), array(index + 1))) loop(index + 1)
      else false
    }
    loop(0)
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, n * acc)
    }
    go(n, 1)
  }

  def fibonacci(num: Int): Int = {
    @annotation.tailrec
    def go(counter: Int, minusOne: Int, minusTwo: Int): Int = {
      if (counter >= num) minusOne + minusTwo
      else go(counter + 1, minusOne + minusTwo, minusOne)
    }
    num match {
      case _ if num > 0 => go(2, 1, 0)
      case _ => 0
    }
  }
}