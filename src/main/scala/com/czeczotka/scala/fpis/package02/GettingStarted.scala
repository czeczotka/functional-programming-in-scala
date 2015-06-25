package com.czeczotka.scala.fpis.package02

object GettingStarted {

  def main(args: Array[String]) {
    example_241_factorial()
  }

  def example_241_factorial() {
    val gs = new GettingStarted
    val num = 5
    println(s"factorial($num) = ${gs.factorial(num)}")
  }

  def exercise21_fibonacci() = ???
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

  def fib(n: Int): Int = ???

}