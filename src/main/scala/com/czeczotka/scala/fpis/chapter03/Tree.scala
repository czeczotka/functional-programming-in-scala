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

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeFold[A](tree: Tree[A]): Int = {
    fold(tree)((a: A) => 1)((a, b) => a + b + 1)
  }

  def maxFold(tree: Tree[Int]): Int = {
    fold(tree)(identity)((a, b) => a max b)
  }

  def depthFold[A](tree: Tree[A]): Int = {
    fold(tree)((a: A) => 1)((a, b) => (a max b) + 1)
  }

  def mapFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
  }
}