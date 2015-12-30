package com.czeczotka.scala.fpis.chapter03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => max(left) max max(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 1
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }
}