package com.stacksandqueues

import com.stacksandqueues.MyQueue.fillFront

class MyQueue[A] {
  private var front: List[A] = Nil
  private var end: List[A] = Nil

  @throws[IllegalStateException]("stack is empty")
  def pop: A =
    if isEmpty then throw new IllegalStateException("queue is empty")
    if front.isEmpty then
      fillFront(this)
      val ret = front.head
      front = front.tail
      ret
    else
      val ret = front.head
      front = front.tail
      ret

  def push(elem: A): Unit = end = elem :: end

  @throws[IllegalStateException]("stack is empty")
  def peek: A =
    if isEmpty then throw new IllegalStateException("queue is empty")
    if front.isEmpty then
      fillFront(this)
      front.head
    else front.head

  def isEmpty: Boolean = front.isEmpty && end.isEmpty
}

object MyQueue {
  def apply[A](): MyQueue[A] = new MyQueue[A]

  private def fillFront[A](q: MyQueue[A]): Unit =
    q.front = q.end.reverse
    q.end = Nil
}
