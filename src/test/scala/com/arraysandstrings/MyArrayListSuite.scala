package com.arraysandstrings

import org.scalatest.matchers.should.Matchers.should
import org.scalatest.wordspec.AnyWordSpec

class MyArrayListSuite extends AnyWordSpec {
  "MyArrayList" should {
    "be instantiable with no arguments" in {
      val list = MyArrayList[Int]()
      list.length should === (0)
    }

    "be instantiable with 1 or more elems" in {
      val list = MyArrayList(1)
      val list2 = MyArrayList(1, 2)

      list.length should === (1)
      list2.length should === (2)
    }

    "be able to append when a resize isn't needed" in {
      val list = MyArrayList[Int]()
      list.length should === (0)

      list.append(5)
      list.length should === (1)
      list should === (MyArrayList(5))
    }

    "be able to append when a resize is needed" in {
      val list = MyArrayList(1,2,3,4,5,6,7,8,9,10)
      list.length should === (10)

      list.append(11)
      list.length should === (11)
      list should === (MyArrayList(1,2,3,4,5,6,7,8,9,10,11))
    }

    "be able to un append before a resize" in {
      val list = MyArrayList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      list.length should ===(10)

      list.unAppend()
      list.length should === (9)
      list should === (MyArrayList(1,2,3,4,5,6,7,8,9))
    }

    "be able to un append after a resize" in {
      val list = MyArrayList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      list.length should ===(10)
      list.append(0)

      list.unAppend()
      list.unAppend()
      list.length should === (9)
      list should === (MyArrayList(1,2,3,4,5,6,7,8,9))
    }

    "be able to insert without resizing" in {
      val list = MyArrayList(1, 2, 3, 4, 5, 6, 7, 8, 9)
      list.length should ===(9)

      list.insert(99, 4)
      list.length should === (10)
      list should === (MyArrayList(1,2,3,4,99,5,6,7,8,9))
    }

    "be able to insert causing a resize" in {
      val list = MyArrayList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      list.length should ===(10)

      list.insert(99, 4)
      list.length should === (11)
      list should === (MyArrayList(1,2,3,4,99,5,6,7,8,9,10))
    }

    "be able to remove an element at a given index" in {
      val list = MyArrayList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      list.append(11)
      list.length should ===(11)

      list.remove(4)
      list.length should === (10)
      list should === (MyArrayList(1,2,3,4,6,7,8,9,10,11))
    }
  }
}
