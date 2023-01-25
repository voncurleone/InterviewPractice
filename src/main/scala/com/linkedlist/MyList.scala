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
    require(i > -1)
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

  def append[B >: A](m: MyList[B]): MyList[B] =
    @tailrec
    def loop(list: MyList[B], acc: MyList[B] = this.reverse): MyList[B] = list match
      case Cons(head, tail) => loop(tail, head :: acc)
      case Empty => acc.reverse
    loop(m)

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
  def isEmpty: Boolean = false
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
  override def take(n: Int): MyList[Nothing] =
    require(n > -1)
    this
  override def drop(n: Int): MyList[Nothing] =
    require(n > -1)
    this
  override def length: Int = 0
  override def reverse: MyList[Nothing] = this
  override def append[B](m: MyList[B]): MyList[B] = m
  override def contains[B](target: B): Boolean = false
  override def mkString(init: String, sep: String, end: String): String = init + end
}

object MyList {
  def apply[A](elems: A*): MyList[A] = elems.foldLeft(empty[A])((l, e) => e :: l).reverse

  def empty[A]: MyList[A] = Empty

  //non tail recursive flatten
  /*def flatten[A](l: MyList[MyList[A]]): MyList[A] = l match
    case Empty => Empty
    case Cons(head, tail) => head.append(flatten(tail))*/

  def flatten[A](l: MyList[MyList[A]]): MyList[A] =
    @tailrec
    def loop(list: MyList[MyList[A]], acc: MyList[A] = Empty): MyList[A] = list match
      case Empty => acc.reverse
      case Cons(head, tail) =>
        @tailrec
        def loop2(inList: MyList[A], inAcc: MyList[A] = acc): MyList[A] = inList match
          case Empty => inAcc
          case Cons(head, tail) => loop2(tail, head :: inAcc)
        loop(tail, loop2(head))
    loop(l)

  //exercises
  def removeDupes[A](l: MyList[A]): MyList[A] =
    @tailrec
    def loop(list: MyList[A], acc: MyList[A] = Empty, set: Set[A] = Set()): MyList[A] = list match
      case Cons(head, tail) =>
        if set.contains(head) then loop(tail, acc, set)
        else loop(tail, head :: acc, set + head)
      case Empty => acc.reverse
    loop(l)

  def deleteMiddle[A](l: MyList[A]): MyList[A] =
    if l.length % 2 == 0 then ???
    else ???

  def partition[A: Ordering](n: Int, l: MyList[A]): MyList[A] = ???
  def sum[A: Numeric](l: MyList[A], l2: MyList[A]): MyList[A] = ???
  def palindrome[A](l: MyList[A]): MyList[A] = ???
}

