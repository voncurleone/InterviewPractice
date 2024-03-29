package com.stacksandqueues

import scala.math.Ordered.orderingToOrdered

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

  def size: Int = stack.size
}

object MyStack {
  def apply[A](): MyStack[A] = new MyStack[A]

  def sort[A: Ordering](unsorted: MyStack[A]): Unit =
    val sorted = MyStack[A]()

    while(!unsorted.isEmpty) {
      val temp = unsorted.pop
      while(!sorted.isEmpty && temp > sorted.peek) {
        unsorted.push(sorted.pop)
      }
      sorted.push(temp)
    }

    unsorted.stack = sorted.stack
}

/*
1) Multiple stacks could be implemented using one array by tracking the index of each stack base 
  along with the index of the top of each stack. Be sure that one stack doesn't grow to the point where 
  it reaches a point in the array containing another stack
*/
