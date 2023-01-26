package com.stacksandqueues

class MyStack[A]{
  var stack: List[A] = Nil
  
  @throws[IllegalStateException]("stack is empty")
  def pop: A = ???
  
  def push(elem: A): Unit = ???
  
  @throws[IllegalStateException]("stack is empty")
  def peek: A = ???
  
  def isEmpty: Boolean = ???
}

object MyStack {
  def apply[A](): MyStack[A] = new MyStack[A]
}
