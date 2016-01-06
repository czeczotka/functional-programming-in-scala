package com.czeczotka.scala.fpis.chapter04

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this flatMap((a: A) => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]