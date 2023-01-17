package com.linkedlist

sealed trait MyNode[+A]
case class Node[A](elem: A, next: MyNode[A] = End) extends MyNode[A]
case object End extends MyNode[Nothing]

class MyLinkedList[A]() {
  var head: MyNode[A] = End
  var tail: MyNode[A] = End
  
  def length: Int = ???
  def append(elem: A): Unit = ???
  def insert(elem: A): Unit = ???
  def removeHead(): Unit = ???
  def removeTail(): Unit = ???
  def remove(index: Int): Unit = ???
  def isEmpty: Boolean = ???
  def apply(index: Int): A = ???
}

object MyLinkedList {
  def apply[A](elems: A*): MyLinkedList[A] = ???
}



