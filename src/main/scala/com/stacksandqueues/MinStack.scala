package com.stacksandqueues

import scala.math.Ordered.orderingToOrdered

class MinStack[A: Ordering] {
  private val stack = MyStack[(A, A)]()

  @throws[IllegalStateException]("stack is empty")
  def pop: A = stack.pop._1

  @throws[IllegalArgumentException]("stack is empty")
  def peek: A = stack.peek._1

  @throws[IllegalStateException]("stack is empty")
  def min: A = stack.peek._2

  def push(elem: A): Unit = //stack.push((elem, elem))
    if isEmpty || elem < min then stack.push((elem, elem))
    else stack.push((elem, min))

  def isEmpty: Boolean = stack.isEmpty
}

object MinStack {
  def apply[A: Ordering](): MinStack[A] = new MinStack
}
