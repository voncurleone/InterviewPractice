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

  def prepend(elem: A): Unit = ???
  def insert(elem: A, index: Int): Unit = ???
  def removeHead(): Unit = ???
  def removeTail(): Unit = ???
  def remove(index: Int): Unit = ???
  def isEmpty: Boolean = head == End

  override def equals(obj: Any): Boolean =
    @tailrec
    def loop(n1: MyNode, n2: MyNode): Boolean = n1 match
      case Node(elem, next) => n2 match
        case Node(e, n) =>
          if elem != e then false
          else loop(next, n)
      case End => true

    obj match
      case list: MyLinkedList[A] =>
        if list.size != size then false
        else loop(list.head, head)
      case _ => false

  //exercises
  def removeDupes(): Unit = ???
  def apply(index: Int): A = ???
  def deleteMiddle(): Unit = ???
  def partition(): Unit = ???
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



