package com.arraysandstrings

object MyStringBuilder {
  def apply(): MyStringBuilder = new MyStringBuilder()
}
class MyStringBuilder() {
  val arrayList: MyArrayList[String] = MyArrayList()

  def add(s: String): Unit =
    arrayList.append(s)

  override def toString: String =
    var ret = ""

    for(i <- 0 until arrayList.length) {
      ret = ret + arrayList(i)
    }
    ret
}
