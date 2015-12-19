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

  val patternMatched = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
}