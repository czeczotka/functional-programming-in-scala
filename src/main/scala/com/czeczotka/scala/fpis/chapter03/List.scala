package com.czeczotka.scala.fpis.chapter03

import scala.annotation.tailrec

/**
  *
  *   List definition copied from
  *   https://github.com/fpinscala/fpinscala/blob/master/exercises/src/main/scala/fpinscala/datastructures/List.scala
  *
  */

// `List` data type, parameterized on a type, `A`
sealed trait List[+A]

// A `List` data constructor representing the empty list
case object Nil extends List[Nothing]

// Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(head, _tail) => _tail
  }

  def setHead[A](newHead: A, list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(head, tail) => Cons(newHead, tail)
  }

  @tailrec
  def drop[A](num: Int, list: List[A]): List[A] = num match {
    case 0 => list
    case n => drop(n - 1, tail(list))
  }

  @tailrec
  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(head, tail) => f(head) match {
      case true => dropWhile(tail, f)
      case false => Cons(head, tail)
    }
  }

  @tailrec
  def dropWhileWithCurry[A](list: List[A])(f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(head, tail) => f(head) match {
      case true => dropWhileWithCurry(tail)(f)
      case false => Cons(head, tail)
    }
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(head, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def sum2(list: List[Int]): Int = {
    foldRight(list, 0)((a, b) => a + b)
  }

  def product2(list: List[Double]): Double = {
    foldRight(list, 1.0)((a, b) => a * b)
  }

  def product3(list: List[Double]): Double = {
    val func = (first: Double, second: Double) => (first, second) match {
      case (a, b) if a == 0 || b == 0 => 0
      case (a, b) => a * b
    }
    foldRight(list, 1.0)(func)
  }

  def length[A](list: List[A]): Int = foldRight(list, 0)((a, b) => 1 + b)

  def foldRight[A,B](list: List[A], z: B)(f: (A, B) => B): B = {
    list match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  @tailrec
  def foldLeft[A,B](list: List[A], z: B)(f: (B, A) => B): B = {
    list match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sumFoldLeft(list: List[Double]): Double = {
    foldLeft(list, 0d)(_ + _)
  }

  def productFoldLeft(list: List[Double]): Double = {
    foldLeft(list, 1d)(_ * _)
  }

  def lengthFoldLeft(list: List[Double]): Int = {
    foldLeft(list, 0)((a, b) => 1 + a)
  }

  def reverse[A](list: List[A]): List[A] = foldLeft(list, List[A]())((a, b) => Cons(b, a))

  def append[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)((a, b) => Cons(a, b))

  def addOne(list: List[Int]): List[Int] = foldRight(list, List[Int]())((a, b) => Cons(a + 1, b))

//  def addOne(list: List[Int]): List[Int] = list match {
//    case Nil => Nil
//    case Cons(head, tail) => Cons(head + 1, addOne(tail))
//  }

  def double2String(list: List[Double]): List[String] = foldRight(list, List[String]())((a, b) => Cons(a.toString, b))

//  def double2String(list: List[Double]): List[String] = list match {
//    case Nil => Nil
//    case Cons(head, tail) => Cons(head.toString, double2String(tail))
//  }

  def map[A, B](list: List[A])(f: (A) => B): List[B] = foldRight(list, List[B]())((a, b) => Cons(f(a), b))

  def filter[A](list: List[A])(predicate: (A) => Boolean): List[A] = {
    val f: (A, List[A]) => List[A] = (a: A, l: List[A]) => if (predicate(a)) Cons(a, l) else l
    foldRight(list, List[A]())(f)
  }

  val patternMatched = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
}