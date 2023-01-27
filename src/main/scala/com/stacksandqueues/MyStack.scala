package com.stacksandqueues

class MyStack[A]{
  private var stack: List[A] = Nil
  
  @throws[IllegalStateException]("stack is empty")
  def pop: A =
    if isEmpty then throw new IllegalStateException("stack is empty")
    else 
      val ret = stack.head
      stack = stack.tail
      ret
  
  def push(elem: A): Unit =
    stack = elem :: stack
  
  @throws[IllegalStateException]("stack is empty")
  def peek: A =
    if isEmpty then throw new IllegalStateException("stack is empty")
    else stack.head
  
  def isEmpty: Boolean = stack.isEmpty
}

object MyStack {
  def apply[A](): MyStack[A] = new MyStack[A]
}
