package com.czeczotka.scala.fpis.package02

object GettingStarted {

  val gs = new GettingStarted

  def main(args: Array[String]) {
    example_241_factorial()
    exercise21_fibonacci()
    exercise22_isSorted()
    example26_partial()
    exercise23_currying()
    exercise24_uncurry()
    exercise25_composeTwoFunctions()
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

  def example26_partial() = {
    val myInt = 3
    val myDouble = 2.5
    val f = gs.partial1[Int, Double, Double](myInt, (a: Int, b: Double) => a * b)
    println(s"partial1: partial1($myInt, (a: Int, b: Double) => a * b) = f($myDouble) = ${f(myDouble)}")
  }

  def exercise23_currying() {
    val myDouble = 2.5
    val multiplyBy5 = gs.curry[Int, Double, Double]((a: Int, b: Double) => a * b)(5)
    println(s"curry: multiplyBy5($myDouble) = ${multiplyBy5(myDouble)}")
  }

  def exercise24_uncurry() {
    val myInt = 5
    val myDouble = 2.5
    val multiply = gs.uncurry[Int, Double, Double]((a: Int) => (b: Double) => a * b)
    println(s"uncurry: multiply($myInt, $myDouble) = ${multiply(myInt, myDouble)}")
  }

  def exercise25_composeTwoFunctions() {
    val myInt = 2
    val add5Multiply2 = gs.compose[Int, Double, Double]((c: Double) => c * 2, (b: Int) => b + 5)
    println(s"compose: add5Multiply2($myInt) = ${add5Multiply2(myInt)}")
  }
}

class GettingStarted {

  def compose[A,B,C](f: B => C, g: A=> B): A => C =
    (a: A) => f(g(a))

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

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