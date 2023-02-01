package com.stacksandqueues

import com.arraysandstrings.MyArrayList

class ListStack[A] {
  private val array: MyArrayList[MyStack[A]] = MyArrayList()

  @throws[IllegalStateException]("stack is empty")
  def pop: A =
    if isEmpty then throw new IllegalStateException()
    else
      val ret = array(array.length - 1).pop
      if array(array.length - 1).size == 0 then
        array.remove(array.length - 1)
        ret
      else ret

  @throws[IllegalArgumentException]("stack is empty")
  def peek: A =
    if isEmpty then throw new IllegalStateException()
    else array(array.length - 1).peek

  def push(elem: A): Unit =
    if isEmpty || array(array.length - 1).size == 10 then
      array.append(MyStack())
      array(array.length - 1).push(elem)
    else
      array(array.length - 1).push(elem)

  @throws[IllegalArgumentException]("index out of bounds")
  def popAt(index: Int): A =
    if index < 0 || index > array.length - 1 then throw new IllegalArgumentException()
    else
      val ret = array(index).pop
      if array(index).size == 0 then
        array.remove(index)
        ret
      else ret

  def length: Int = array.length

  def isEmpty: Boolean = if array.length == 0 then true else false
}

object ListStack {
  def apply[A](): ListStack[A] = new ListStack
}
