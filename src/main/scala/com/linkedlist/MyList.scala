package com.linkedlist

import com.linkedlist.MyList.empty

import scala.annotation.{tailrec, targetName}

sealed abstract class MyList[+A] {
  @targetName("Cons")//prepend
  def ::[B >: A](elem: B): MyList[B]

  @targetName("append")
  def +:[B >: A](elem: B): MyList[B] =
    @tailrec
    def loop(list: MyList[A], acc: MyList[B] = empty[B]): MyList[B] = list match
      case Cons(head, tail) => loop(tail, head :: acc)
      case Empty => (elem :: acc).reverse
    loop(this)

  def toList: List[A] =
    @tailrec
    def loop(list: MyList[A], acc: List[A] = List.empty[A]): List[A] = list match
      case Cons(head, tail) => loop(tail, head :: acc)
      case Empty => acc.reverse
    loop(this)

  def isEmpty: Boolean

  @throws[NoSuchElementException]("if list is empty")
  def head: A

  @throws[NoSuchElementException]("if list is empty")
  def tail: MyList[A]

  @throws[NoSuchElementException]("if list is empty or index is greater than or equal to size")
  @throws[IllegalArgumentException]("if index is less than 0")
  def apply(i: Int): A =
    if i >= length then throw new NoSuchElementException("i larger than length")
    @tailrec
    def loop(i: Int, list: MyList[A]): A = list match
      case Cons(head, tail) =>
        if i == 0 then head
        else loop(i - 1, tail)
      case Empty => throw new NoSuchElementException("i larger than length")
    loop(i, this)

  @throws[IllegalArgumentException]("if n < 0")
  def take(n: Int): MyList[A] =
    if n < 0 then throw new IllegalArgumentException("n is < 0")
    @tailrec
    def loop(i: Int, list: MyList[A], acc: MyList[A] = empty[A]): MyList[A] = list match
      case Cons(head, tail) =>
        if i == 0 then acc.reverse
        else loop(i - 1, tail, head :: acc)
      case Empty => acc.reverse
    loop(n,this)


  @throws[IllegalArgumentException]("if n < 0")
  def drop(n: Int): MyList[A] =
    if n < 0 then throw new IllegalArgumentException("n is < 0")
    @tailrec
    def loop(i: Int, list: MyList[A]): MyList[A] = list match
      case Empty => empty[A]
      case Cons(_, tail) =>
        if i == 0 then list
        else loop(i - 1, tail)
    loop(n, this)

  def length: Int =
    @tailrec
    def loop(list: MyList[A], acc: Int = 0): Int = list match
      case Cons(_, tail) => loop(tail, acc + 1)
      case Empty => acc
    loop(this)

  def reverse: MyList[A] =
    @tailrec
    def loop(list: MyList[A], acc: MyList[A] = empty[A]): MyList[A] = list match
      case Cons(head, tail) => loop(tail, head :: acc)
      case Empty => acc
    loop(this)

  def append[B >: A](m: MyList[B]): MyList[B] = ???

  def contains[B >: A](target: B): Boolean =
    @tailrec
    def loop(list: MyList[A]): Boolean = list match
      case Cons(head, tail) =>
        if target == head then true
        else loop(tail)
      case Empty => false
    loop(this)

  def mkString(init: String, sep: String, end: String): String =
    val builder = StringBuilder(init)
    @tailrec
    def loop(list: MyList[A]): String = list match
      case Empty =>
        builder ++= end
        builder.toString
      case Cons(head, Empty) =>
        builder ++= head.toString
        loop(Empty)
      case Cons(head, tail) =>
        builder ++=  s"$head,"
        loop(tail)
    loop(this)

  override def toString: String = mkString("MyList(", ",", ")")
}

case class Cons[A](head: A, tail: MyList[A]) extends MyList[A] {
  @targetName("Cons")
  def ::[B >: A](elem: B): MyList[B] = Cons(elem, this)
  //@targetName("append")
  //def +:[B >: A](elem: B): MyList[B] = ???
  //def toList: List[A] = ???
  def isEmpty: Boolean = false
  //def apply(i: Int): A = ???
  //def take(n: Int): MyList[A] = ???
  //def drop(n: Int): MyList[A] = ???
  //def length: Int = ???//1 + tail.length
  //def reverse: MyList[A] = ???
  //def append[B >: A](m: MyList[B]): MyList[B] = ???
  //def contains[B >: A](target: B): Boolean = head == ???//target || tail.contains(target)
  //def mkString(init: String, sep: String, end: String): String = ???
}
case object Empty extends MyList[Nothing] {
  @targetName("Cons")
  def ::[B](elem: B): MyList[B] = Cons(elem, this)
  @targetName("append")
  override def +:[B](elem: B): MyList[B] = elem :: this
  override def toList: List[Nothing] = Nil
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("head of empty list")
  def tail: Nothing = throw new NoSuchElementException("tail of empty list")
  override def apply(i: Int): Nothing = throw new NoSuchElementException("apply on empty list")
  override def take(n: Int): MyList[Nothing] = this
  override def drop(n: Int): MyList[Nothing] = this
  override def length: Int = 0
  override def reverse: MyList[Nothing] = this
  override def append[B](m: MyList[B]): MyList[B] = m
  override def contains[B](target: B): Boolean = false
  override def mkString(init: String, sep: String, end: String): String = init + end
}

object MyList {
  def apply[A](elems: A*): MyList[A] = elems.foldLeft(empty[A])((l, e) => e :: l)

  def empty[A]: MyList[A] = Empty

  def flatten[A](l: MyList[MyList[A]]): MyList[A] = ???
}

