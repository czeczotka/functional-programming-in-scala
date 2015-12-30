package com.czeczotka.scala.fpis.chapter03

import com.czeczotka.scala.fpis.chapter03.List._
import com.czeczotka.scala.fpis.chapter03.Tree._
import org.specs2.mutable.Specification

class FunctionalDataStructuresSpec extends Specification {

  "List implementation" should {
    "exercise 3.1: pattern match to the value of 3 " in {
      patternMatched should equalTo(3)
    }

    "exercise 3.2: `tail` function should remove the first element of a List" in {
      tail(Nil) should equalTo(Nil)
      tail(List("hello")) should equalTo(Nil)
      tail(List("hello", "world")) should equalTo(List("world"))
      tail(List("hello", "world", "!")) should equalTo(List("world", "!"))
    }

    "exercise 3.3: `setHead` function should replace the first element of a List with a different value" in {
      setHead("value", Nil) should equalTo(Nil)
      setHead("value", List("hello")) should equalTo(List("value"))
      setHead("value", List("hello", "world")) should equalTo(List("value", "world"))
      setHead("value", List("hello", "world", "!")) should equalTo(List("value", "world", "!"))
    }

    "exercise 3.4: `drop` function should remove n first elements of a List" in {
      drop(1, Nil) should equalTo(Nil)
      drop(1, List("hello")) should equalTo(Nil)
      drop(2, List("hello", "world")) should equalTo(Nil)
      drop(2, List("hello", "world", "!")) should equalTo(List("!"))
      drop(3, List("hello", "world", "!")) should equalTo(Nil)
      drop(1, Nil) should equalTo(Nil)
    }

    "exercise 3.5: `dropWhile` function should remove elements from the List prefix as long as they match a predicate" in {
      dropWhile(Nil, (a: Any) => true) should equalTo(Nil)
      dropWhile(Nil, (a: Any) => false) should equalTo(Nil)
      dropWhile(List("hello"), (s: String) => s.isEmpty) should equalTo(List("hello"))
      dropWhile(List("hello"), (s: String) => !s.isEmpty) should equalTo(Nil)
      dropWhile(List("hello", "world"), (s: String) => s == "hello") should equalTo(List("world"))
      dropWhile(List("hello", "world"), (s: String) => s == "world") should equalTo(List("hello", "world"))
      dropWhile(List("hello", "world", "!"), (s: String) => s.length > 1) should equalTo(List("!"))
      dropWhile(List("hello", "world", "!"), (s: String) => s.length > 0) should equalTo(Nil)
      dropWhile(List(1, 2, 3, 4, 5), (i: Int) => i < 4) should equalTo(List(4, 5))
    }

    "example 3.3.2: `dropWhileWithCurry` function should remove elements from the List prefix as long as they match a predicate" in {
      val f1 = dropWhileWithCurry(List("hello")) _
      f1(s => !s.isEmpty) should equalTo(Nil)

      val f2 = dropWhileWithCurry(List(1, 2, 3, 4, 5)) _
      f2(i => i < 4) should equalTo(List(4, 5))
    }

    "exercise 3.6: `init` function should return all but the last element of a List" in {
      init(Nil) should equalTo(Nil)
      init(List("hello")) should equalTo(Nil)
      init(List("hello", "world")) should equalTo(List("hello"))
      init(List("hello", "world", "!")) should equalTo(List("hello", "world"))
    }

    // looking at the stack I don't think it's possible as the curried function
    // is not evaluated until all recursive foldRight calls are executed
    "exercise 3.7: `product2` should use `foldRight` to calculate product" in {
      product3(List(2, 1.5)) should equalTo(3)
      product3(List(2, 0)) should equalTo(0)
      product3(List(2, 0, 3, 4, 5)) should equalTo(0)
    }

    "exercise 3.8: pass Nil and Cons to foldRight" in {
      println("exercise 3.8: foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_,_)) = " + foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_,_)))

      //   see the trace of subsequent calls - we ended up with list we had in the beginning
      //      foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_,_))
      //      Cons(1, foldRight(List(2, 3), Nil:List[Int])(Cons(_,_)))
      //      Cons(1, Cons(2, foldRight(List(3), Nil:List[Int])(Cons(_,_))))
      //      Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
      //      Cons(1, Cons(2, Cons(3, Nil)))

      true should beTrue
    }

    "exercise 3.9: `length` should return the number of elements in a List" in {
      List.length(Nil) should equalTo(0)
      List.length(List(5)) should equalTo(1)
      List.length(List(5, 6)) should equalTo(2)
      List.length(List(1, 2, 3, 4, 5)) should equalTo(5)
      List.length(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)) should equalTo(10)
    }

    "exercise 3.10: `foldLeft` should be tail recursive" in {
      def sum(list: List[Int]): Int = {
        foldLeft(list, 0)((a, b) => a + b)
      }
      sum(List(1, 2, 3, 4, 5)) should equalTo(15)
    }

    "exercise 3.11: `sumFoldLeft` should return the sum of elements in a List" in {
      sumFoldLeft(Nil) should equalTo(0)
      sumFoldLeft(List(2, 1.5)) should equalTo(3.5)
      sumFoldLeft(List(2, 0, -2)) should equalTo(0)
      sumFoldLeft(List(2, 0, 3, 4, 5)) should equalTo(14)
    }

    "exercise 3.11: `productFoldLeft` should return the product of elements in a List" in {
      productFoldLeft(List(2, 1.5)) should equalTo(3)
      productFoldLeft(List(2, 0)) should equalTo(0)
      productFoldLeft(List(2, 0, 3, 4, 5)) should equalTo(0)
    }

    "exercise 3.11: `lengthFoldLeft` should return the length of a List" in {
      lengthFoldLeft(Nil) should equalTo(0)
      lengthFoldLeft(List(5)) should equalTo(1)
      lengthFoldLeft(List(5, 6)) should equalTo(2)
      lengthFoldLeft(List(1, 2, 3, 4, 5)) should equalTo(5)
      lengthFoldLeft(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)) should equalTo(10)
    }

    "exercise 3.12: `reverse` should reverse a List" in {
      reverse(Nil) should equalTo(Nil)
      reverse(List(5)) should equalTo(List(5))
      reverse(List(5, 6)) should equalTo(List(6, 5))
      reverse(List(1, 2, 3, 4, 5)) should equalTo(List(5, 4, 3, 2, 1))
    }

    "exercise 3.14: `append` should append another list to a List" in {
      append(Nil, List(1)) should equalTo(List(1))
      append(List(5), Nil) should equalTo(List(5))
      append(List(5), List(6)) should equalTo(List(5, 6))
      append(List(1, 2, 3), List(4, 5)) should equalTo(List(1, 2, 3, 4, 5))
    }

    "exercise 3.16: `addOne` should add 1 to each element of a List[Int]" in {
      addOne(Nil) should equalTo(Nil)
      addOne(List(1)) should equalTo(List(2))
      addOne(List(5, 6)) should equalTo(List(6, 7))
      addOne(List(1, 2, 3, 4, 5)) should equalTo(List(2, 3, 4, 5, 6))
    }

    "exercise 3.17: `double2String` should turn each value of a List[Double] into a String" in {
      double2String(Nil) should equalTo(Nil)
      double2String(List(1.0)) should equalTo(List("1.0"))
      double2String(List(5.1, 6.2)) should equalTo(List("5.1", "6.2"))
    }

    "exercise 3.18: `map` should modify each element of a List in a generic way" in {
      List.map(Nil)(_ => true) should equalTo(Nil)
      List.map(List(1))(_ => true) should equalTo(List(true))
      List.map(List(1))((i: Int) => i > 0) should equalTo(List(true))
      List.map(List(1))((i: Int) => i <= 0) should equalTo(List(false))
      List.map(List(1, 2, 4, 8))((i: Int) => (i * i).toString) should equalTo(List("1", "4", "16", "64"))
    }

    "exercise 3.19: `filter` should remove an element from a List if it does not satisfy a predicate" in {
      filter(Nil)(_ => true) should equalTo(Nil)
      filter(List(1))(_ => true) should equalTo(List(1))
      filter(List(1, 2, 4, 5, 7, 8))((i: Int) => i % 2 == 0) should equalTo(List(2, 4, 8))
    }

    "exercise 3.20: `flatMap` should flatten map operation which returns a list instead of a single element" in {
      flatMap(Nil)(_ => List(true)) should equalTo(Nil)
      flatMap(List(1, 2, 3))(_ => List(true)) should equalTo(List(true, true, true))
      flatMap(List(1, 2, 3))(i => List(i, i)) should equalTo(List(1, 1, 2, 2, 3, 3))
      flatMap(List(1, 2, 3))(i => List(i, i * i)) should equalTo(List(1, 1, 2, 4, 3, 9))
    }

    "exercise 3.21: `flatMapFilter` should remove an element from a List if it does not satisfy a predicate" in {
      flatMapFilter(Nil)(_ => true) should equalTo(Nil)
      flatMapFilter(List(1))(_ => true) should equalTo(List(1))
      flatMapFilter(List(1, 2, 4, 5, 7, 8))((i: Int) => i % 2 == 0) should equalTo(List(2, 4, 8))
    }

    "exercise 3.22: `addList` should take two lists and construct a new list by adding corresponding elements" in {
      addLists(Nil, Nil) should equalTo(Nil)
      addLists(List(1, 2, 3), Nil) should equalTo(List(1, 2, 3))
      addLists(Nil, List(1, 2, 3)) should equalTo(List(1, 2, 3))
      addLists(List(1), List(5, 6, 7)) should equalTo(List(6, 6 ,7))
      addLists(List(-1, -4, -8), List(5, 6, 7)) should equalTo(List(4, 2, -1))
    }

    "exercise 3.23: `zipWith` should take two lists and construct a new list using a function to combine elements" in {
      zipWith(Nil, Nil)((a: Any, b: Any) => 1) should equalTo(Nil)
      zipWith(List(1, 2, 3), Nil)((_, _) => 1) should equalTo(List(1, 2, 3))
      zipWith(Nil, List(1, 2, 3))((_, _) => 1) should equalTo(List(1, 2, 3))
      zipWith(List(1), List(5, 6, 7))((a, b) => a + b) should equalTo(List(6, 6 ,7))
      zipWith(List(-1.1, -4.0, -9.0), List(5.0, 6.0, 7.0))((a, b) => a * b) should equalTo(List(-5.5, -24, -63))
    }

    "exercise 3.24: `hasSubsequence` checks whether a list constains another list as a subsequence" in {
      hasSubsequence(Nil, List(1, 2, 3)) should equalTo(false)
      hasSubsequence(List(1, 2, 3, 4), Nil) should equalTo(true)
      hasSubsequence(List(1, 2, 3, 4), List(4, 5)) should equalTo(false)
      hasSubsequence(List(1, 2, 3, 4), List(2, 3)) should equalTo(true)
      hasSubsequence(List(1, 2, 3, 4), List(1, 3)) should equalTo(false)
      hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4)) should equalTo(true)
      hasSubsequence(List(1, 2, 3, 4), List(1, 2)) should equalTo(true)
    }

    "exercise 3.25: `size` counts the number of nodes in a Tree" in {
      Tree.size(Leaf(123)) should equalTo(1)
      Tree.size(Branch(Leaf(123), Leaf(321))) should equalTo(3)
      Tree.size(Branch(Branch(Leaf("a"), Leaf("b")), Leaf("c"))) should equalTo(5)
      Tree.size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) should equalTo(7)
    }
  }
}