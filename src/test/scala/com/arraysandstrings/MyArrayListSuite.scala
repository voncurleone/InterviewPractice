package com.arraysandstrings

import org.scalatest.wordspec.AnyWordSpec

class MyArrayListSuite extends AnyWordSpec {
  "MyArrayList" should {
    "be instantiable with no arguments" in {
      val list = MyArrayList[Int](1, 2, 3, 4, 5, 6, 7, 8, 9)
      val list2 = MyArrayList[Int](1, 2, 3, 4, 5, 6, 7, 8, 9)
      val list3 = MyArrayList[Int](1, 2, 9, 4, 5, 6, 7, 8, 9)

      println(list == list2)
      println(list == list3)
      println(list != list3)
    }
  }
}
