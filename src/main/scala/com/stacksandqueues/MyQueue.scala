package com.stacksandqueues

class MyQueue[A] {
  var front: List[A] = Nil
  var end: List[A] = Nil

  @throws[IllegalStateException]("stack is empty")
  def pop: A = ???

  def push(elem: A): Unit = ???

  @throws[IllegalStateException]("stack is empty")
  def peek: A = ???

  def isEmpty: Boolean = ???
}

object MyQueue {
  def apply[A](): MyQueue[A] = new MyQueue[A]
}
