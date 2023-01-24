package com.linkedlist

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.should

val empty = MyList.empty[Int]

class MyListSuite extends AnyWordSpec {
  "MyList" should {
    "Empty" should {
      "prepend an element with ::" in {
        val list = 1 :: empty
        list.length should === (1)
        list should === (Cons(1, Empty))
        list.contains(1) should === (true)
        list.isEmpty should === (false)
        list.head should === (1)
        list.tail should === (empty)
        list.toString should === ("MyList(1)")
        list(0) should === (1)
      }
    }
  }
}
