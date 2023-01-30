package com.stacksandqueues

import com.arraysandstrings.MyArrayList

class ListStack[A] {
  private val array: MyArrayList[MyStack[A]] = MyArrayList()
  private val maxSize = 10
  private var stackSize = 0

  @throws[IllegalStateException]("stack is empty")
  def pop: A = ???

  @throws[IllegalArgumentException]("stack is empty")
  def peek: A = ???

  def push(elem: A): Unit = ???

  @throws[IllegalArgumentException]("index out of bounds")
  def popAt(index: Int): A = ???

  def length: Int = ???

  def isEmpty: Boolean = ???
}

object ListStack {
  def apply[A](): ListStack[A] = new ListStack
}
