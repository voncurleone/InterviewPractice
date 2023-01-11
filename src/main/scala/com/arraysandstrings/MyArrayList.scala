package com.arraysandstrings

import scala.reflect.ClassTag

object MyArrayList {
  def apply[A:ClassTag](elems: A*): MyArrayList[A] = {
    val arrayList = new MyArrayList[A]()
    elems.foreach(arrayList.append)
    arrayList
  }
}

class MyArrayList[A:ClassTag]() {
  private var arraySize = 0 //also the index of next append
  private var array = new Array[A](10)

  def append(elem: A): Unit =
    if array.length == arraySize then
      val temp = new Array[A](array.length * 2)
      for(i <- 0 until arraySize) {
        temp(i) = array(i)
      }

      temp(arraySize) = elem
      arraySize += 1
      array = temp
    else
      array(arraySize) = elem
      arraySize += 1

  def unAppend(): Unit =
    arraySize -= 1

  def insert(elem: A, index: Int): Unit =
    if index > arraySize then throw new IndexOutOfBoundsException
    if array.length == arraySize then
      val temp = new Array[A](array.length * 2)
      for(i <- 0 until index) {
        temp(i) = array(i)
      }

      temp(index) = elem
      for(i <- index until arraySize) {
        temp(i + 1) = array(i)
      }
      array = temp
      arraySize += 1
    else
      val temp = new Array[A](array.length)
      for(i <- 0 until index) {
        temp(i) = array(i)
      }

      temp(index) = elem
      for(i <- index until arraySize) {
        temp(i + 1) = array(i)
      }
      array = temp
      arraySize += 1

  def remove(index: Int): Unit =
    val temp = new Array[A](array.length)
    for(i <- 0 until index) {
      temp(i) = array(i)
    }
    for(i <- index until arraySize) {
      temp(i) = array(i + 1)
    }
    array = temp
    arraySize -= 1

  def forEach(f: A => Unit): Unit = for (index <- 0 until arraySize) {
    f(array(index))
  }

  def length: Int = arraySize

  def apply(index: Int): A =
    if index > -1 && index < arraySize then array(index)
    else throw new IndexOutOfBoundsException

  override def toString: String =
    val builder = StringBuilder("")
    builder ++= "MyArrayList("

    for (i <- 0 until arraySize) {
      if i == arraySize - 1 then builder ++= array(i).toString
      else builder ++= array(i).toString + ", "
    }

    builder ++= ")"
    builder.toString

  override def equals(obj: Any): Boolean = obj match
    case list: MyArrayList[A] =>
      if list.arraySize != arraySize then return false
      else for(i <- 0 until arraySize) {
        if list(i) != array(i) then return false
      }
      true

    case _ => false
}
