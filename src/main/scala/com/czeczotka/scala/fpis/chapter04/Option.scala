package com.czeczotka.scala.fpis.chapter04

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(get) => f(get)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None => default
  }

//  def orElse[B >: A](ob: => Option[B]): Option[B]
//  def filter(f: A => Boolean): Option[A]
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]