package com.linkedlist

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

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

  def remove(index: Int): Unit =
    require(index > -1 && index < size)
    @tailrec
    def loop(count: Int, current: MyNode, previous: MyNode = End): Unit = current match
      case Node(_, next) =>
        if count == 0 then previous match
          case p @ Node(_, _) =>
            p.next = next
            size -= 1
          case End =>
            head = next
            size -= 1
          case _ => throw new IllegalStateException
        else loop(count - 1, next, current)
      case _ => throw new IllegalStateException
    loop(index, head)

  def isEmpty: Boolean = head == End

  override def toString: String =
    val builder = StringBuilder("")
    builder ++= "MyLinkedList("
    @tailrec
    def loop(n: MyNode): Unit = n match
      case Node(e, End) => builder ++= s"$e"
      case Node(e, next) =>
        builder ++= s"$e, "
        loop(next)
      case _ => throw new IllegalStateException

    loop(head)
    builder ++= s")"
    builder.toString()

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
  def removeDupes(): Unit =
    val list = MyLinkedList[A]()
    var seen = Set[A]()
    @tailrec
    def loop(current: MyNode): Unit = current match
      case End =>
        head = list.head
        size = list.size
      case Node(elem: A, next) =>
        if seen.contains(elem) then loop(next)
        else
          list.append(elem)
          seen = seen + elem
          loop(next)
    loop(head)

  def apply(index: Int): A =
    require(index > -1 && index < size)
    @tailrec
    def loop(i: Int, current: MyNode): A = current match
      case Node(e: A, n) =>
        if i == 0 then e
        else loop(i - 1, n)
      case End => throw new IllegalStateException
    loop(index, head)

  def deleteMiddle(): Unit =
    if size == 0 then ()
    else
      val index = size / 2
      remove(index)

  //scalac: Error: assertion failed: TS[2546377, 2546376, 2546375] attempted to take ownership of A which is already owned by committable TS[2546720, 2546377, 2546376, 2546375]
  /*def partition(element: A): Unit =
    val list = MyLinkedList[A]()
    var seen = Set[A]()
    @tailrec
    def loop(current: MyNode): Unit = current match
      case End =>
        head = list.head
        size = list.size
      case Node(elem: A, next) =>
        if elem > element then
          list.append(elem)
          loop(next)
        else
          list.prepend(elem)
          loop(next)
    loop(head)*/
  def partition(element: A): Unit = ???

  def sum(list: MyLinkedList[A]): MyLinkedList[A] = ??? //need to think about this.. A doesn't have + method. most likely cant implement with the way I have implemented the rest of MyLinkedList

  def palindrome: Boolean =
    var stack = List[A]()
    //@tailrec
    def loop(node: MyNode): Boolean = node match
      case End => true
      case Node(elem: A, next) =>
        if stack.contains(elem) then ???
        else ???
    loop(head)
}

object MyLinkedList {
  def apply[A](elems: A*): MyLinkedList[A] =
    val list = new MyLinkedList[A]()

    for(elem <- elems) {
      list.append(elem)
    }

    list
}



