package com.czeczotka.scala.fpis.chapter03

import com.czeczotka.scala.fpis.chapter03.List.{patternMatched, tail, setHead, drop, dropWhile, dropWhileWithCurry}
import com.czeczotka.scala.fpis.chapter03.List.{init, foldRight, product3, length}
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
  }
}