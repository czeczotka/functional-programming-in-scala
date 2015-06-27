package com.czeczotka.scala.fpis.package02

object GettingStarted {

  def main(args: Array[String]) {
    example_241_factorial()
    exercise21_fibonacci()
  }

  def example_241_factorial() {
    val gs = new GettingStarted
    val num = 5
    println(s"factorial($num) = ${gs.factorial(num)}")
  }

  def exercise21_fibonacci() {
    val gs = new GettingStarted
    val num = 10
    println(s"fibonacci($num) = ${gs.fibonacci(num)}")
  }
}

class GettingStarted {

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