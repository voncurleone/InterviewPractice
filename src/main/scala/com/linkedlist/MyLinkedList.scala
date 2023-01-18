package com.linkedlist

sealed trait MyNode[+A]
case class Node[A](var elem: A, next: MyNode[A] = End) extends MyNode[A]
case object End extends MyNode[Nothing]

class MyLinkedList[A]() {
  private var head: MyNode[A] = End
  private var size: Int = 0
  
  def length: Int = size
  def append(elem: A): Unit = ???
  def insert(elem: A): Unit = ???
  def removeHead(): Unit = ???
  def removeTail(): Unit = ???
  def remove(index: Int): Unit = ???
  def isEmpty: Boolean = head == End
  def appendNode(node: MyNode[A]): Unit = ???

  //exercises
  def removeDupes(): Unit = ???
  def apply(index: Int): A = ???
  def deleteMiddle(): Unit = ???
  def partition(): Unit = ???
  def sum(list: MyLinkedList[A]): MyLinkedList[A] = ???
  def palindrome: Boolean = ???
  //def intersection(list: MyLinkedList[A]): MyNode[A] = ???
  //def loopDetection: MyNode[A] = ???


}

object MyLinkedList {
  def apply[A](elems: A*): MyLinkedList[A] = ???
}



