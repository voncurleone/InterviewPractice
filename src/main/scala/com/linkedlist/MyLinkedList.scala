package com.linkedlist

import scala.annotation.{tailrec, targetName}

sealed trait MyNode
case class Node[A](var elem: A, var next: MyNode = End) extends MyNode
case object End extends MyNode

class MyLinkedList[A]() {
  private var head: MyNode = End
  private var size: Int = 0
  
  def length: Int = size
  def append(elem: A): Unit =
    @tailrec
    def loop(node: MyNode): Unit = node match
      case End =>
        head = Node(elem)
        size += 1
      case n @ Node(_, End) =>
        n.next = Node(elem, End)
        size += 1
      case Node(_, next) =>
        loop(next)

    loop(head)

  def prepend(elem: A): Unit =
    val n = Node(elem, head)
    head = n
    size += 1

  def insert(elem: A, index: Int): Unit =
    require(index > -1 && index <= size)
    @tailrec
    def loop(count: Int, current: MyNode, previous: MyNode = End): Unit =
      if count == 0 then
        if head == End then
          val n = Node(elem, End)
          head = n
          size += 1
        else previous match
          case p @ Node(_, _) =>
            val n = Node(elem, current)
            p.next = n
            size += 1
          case _ =>
            if index == 0 then
              val n = Node(elem, current)
              head = n
              size += 1
            else throw new IllegalStateException("adding to front")
      else current match
        case c @ Node(_, _) =>
          loop(count - 1, c.next, current)
        case _ => throw new IllegalStateException("adding else where")
    loop(index, head)

  def removeHead(): Unit = head match
    case Node(_, next) =>
      head = next
      size -= 1
    case _ => throw new IllegalStateException

  def removeTail(): Unit =
    if size == 0 then throw new IllegalStateException
    @tailrec
    def loop(current: MyNode): Unit = current match
      case n @ Node(_, Node(_, End)) =>
        n.next = End
        size -= 1
      case Node(_, End) =>
        head = End
        size -= 1
      case _ => current match
        case Node(_, next) => loop(next)
        case _ => throw new IllegalStateException
    loop(head)

  def remove(index: Int): Unit = ???
  def isEmpty: Boolean = head == End

  override def equals(obj: Any): Boolean =
    @tailrec
    def loop(n1: MyNode, n2: MyNode): Boolean = n1 match
      case Node(elem, next) => n2 match
        case Node(e, n) =>
          if elem != e then false
          else loop(next, n)
      case End => if n2 == End then true else false

    obj match
      case list: MyLinkedList[A] =>
        if list.size != size then false
        else loop(list.head, head)
      case _ => false

  //exercises
  def removeDupes(): Unit = ???
  def apply(index: Int): A = ???
  def deleteMiddle(): Unit = ???
  def partition(elem: A): Unit = ???
  def sum(list: MyLinkedList[A]): MyLinkedList[A] = ???
  def palindrome: Boolean = ???
}

object MyLinkedList {
  def apply[A](elems: A*): MyLinkedList[A] =
    val list = new MyLinkedList[A]()

    for(elem <- elems) {
      list.append(elem)
    }

    list
}



