package com.linkedlist

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.should

class MyLinkedListSuite extends AnyWordSpec {
  "linkedList" must {
    "length should provide the correct length of the list" in {
      MyLinkedList().length should === (0)
      MyLinkedList(1).length should === (1)
      MyLinkedList(1,2).length should === (2)
    }

    "implement isEmpty" must {
      "return true when empty" in {
        MyLinkedList().isEmpty should === (true)
      }

      "return false when not empty" in {
        MyLinkedList(1).isEmpty should === (false)
      }
    }

    "implement equals" in {
      val l = MyLinkedList()
      l should === (MyLinkedList())
      MyLinkedList(1,2) should === (MyLinkedList(1,2))
      val list = MyLinkedList(1,2)
      list should !== (MyLinkedList(2,1))
      MyLinkedList(1) should !== (MyLinkedList(1,2))

      MyLinkedList() should !== (MyLinkedList(1))
      MyLinkedList(1) should !== (MyLinkedList())
    }

    "implement append" must {
      "add element to an empty list" in {
        val list: MyLinkedList[Int] = MyLinkedList()
        list.append(1)
        list should === (MyLinkedList(1))
        list.length should === (1)
      }

      "add element to a list that already has an element" in {
        val list = MyLinkedList(1,2)
        list.append(3)
        list.length should === (3)
        list should === (MyLinkedList(1,2,3))
      }
    }

    "implement prepend" must {
      "prepend an empty list" in {
        val list: MyLinkedList[Int] = MyLinkedList()
        list.prepend(1)
        list.length should === (1)
        list should === (MyLinkedList(1))
      }

      "prepend a list that already contains elements" in {
        val list = MyLinkedList(1)
        list.prepend(2)
        list.length should === (2)
        list should === (MyLinkedList(2,1))

        list.prepend(3)
        list.length should === (3)
        list should === (MyLinkedList(3,2,1))
      }
    }

    "implement insert" must {
      "insert into an empty list" in {
        val list: MyLinkedList[Int] = MyLinkedList()
        list.insert(1, 0)
        list.length should === (1)
        list should === (MyLinkedList(1))
      }

      "throw an exception if index given is < 0 or > length" in {
        val list = MyLinkedList(1,2,3)
        intercept[IllegalArgumentException] {
          list.insert(4, 4)
        }

        intercept[IllegalArgumentException] {
          list.insert(0, -1)
        }
      }

      "add to the front of the list" in {
        val list = MyLinkedList(1,2,3)
        list.insert(0, 0)
        list.length should === (4)
        list should === (MyLinkedList(0,1,2,3))
      }

      "add the the end of the list" in {
        val list = MyLinkedList(1,2,3)
        list.insert(4, 3)
        list.length should === (4)
        list should === (MyLinkedList(1,2,3,4))
      }

      "add in the middle" in {
        val list = MyLinkedList(1,2,3)
        list.insert(99, 1)
        list.length should === (4)
        list should === (MyLinkedList(1,99,2,3))
      }
    }

    "implement removeHead" must {
      "throw exception if called on an empty list" in {
        intercept[IllegalStateException] {
          MyLinkedList().removeHead()
        }
      }

      "remove the head of a list that contains elements" in {
        val list = MyLinkedList(1,2)
        list.removeHead()
        list.length should === (1)
        list should === (MyLinkedList(2))

        list.removeHead()
        list.length should === (0)
        list should === (MyLinkedList())
      }
    }

    "implement removeTail" must {
      "throw exception when called on an empty list" in {
        intercept[IllegalStateException] {
          MyLinkedList().removeTail()
        }
      }

      "remove the tail of a list that contains elements" in {
        val list = MyLinkedList(1,2,3)
        list.removeTail()
        list.length should === (2)
        list should === (MyLinkedList(1,2))

        val l = MyLinkedList(1)
        l.removeTail()
        list.length should === (0)
        list should === (MyLinkedList())
      }
    }

    "implement remove" must {
      "throw exception if index is < 0 or >= length" in {
        val list = MyLinkedList(1,2,3)

        intercept[IllegalArgumentException] {
          list.remove(-1)
        }

        intercept[IllegalArgumentException] {
          list.remove(3)
        }
      }

      "remove from any index" in {
        val list = MyLinkedList(1,2,3,4,5,6)

        list.remove(0)
        list should === (MyLinkedList(2,3,4,5,6))

        list.remove(2)
        list should === (MyLinkedList(2,3,5,6))

        list.remove(3)
        list should === (MyLinkedList(2,3,5))
        list.length should === (3)
      }
    }

    "implement removeDupes" must {
      "not affect the list if it has no duplicates" in {
        val list = MyLinkedList(1,2,3,4)
        list.removeDupes()
        list should === (MyLinkedList(1,2,3,4))

        val list2 =  MyLinkedList()
        list2.removeDupes()
        list2 should === (MyLinkedList())
      }

      "remove duplicates" in {
        val list = MyLinkedList(1,1,2,3,4,5,6,6)
        list.removeDupes()
        list should === (MyLinkedList(1,2,3,4,5,6))
        list.length should === (6)

        val list2 = MyLinkedList(1,2,3,4,2,5,6,7,6)
        list2.removeDupes()
        list2.length should === (6)
        list2 should === (MyLinkedList(1,2,3,4,5,6))
      }
    }

    "implement apply" must {
      "throw exception if the indexes are out of bounds" in {
        val list = MyLinkedList(1,2,3)

        intercept[IllegalArgumentException] {
          list(-1)
        }

        intercept[IllegalArgumentException] {
          list(3)
        }

        intercept[IllegalArgumentException] {
          MyLinkedList()(0)
        }
      }

      "return correct value" in {
        val list = MyLinkedList(1,2,3)

        list(0) should === (1)
        list(1) should === (2)
        list(2) should === (3)
      }
    }

    "implement deleteMiddle" must {
      "return empty when called by an empty list" in {
        val list = MyLinkedList()

        list.deleteMiddle()
        list should === (MyLinkedList)
        list.length should === (0)
      }

      "delete middle in a list with an odd length" in {
        val list = MyLinkedList(1,2,3)
        val list2 = MyLinkedList(1,2,3,4,5)

        list.deleteMiddle()
        list should === (MyLinkedList(1,3))
        list.length should === (2)

        list2.deleteMiddle()
        list2 should === (MyLinkedList(1,2,4,5))
        list2.length should === (4)
      }

      "delete middle in a list with an even length" in {
        val list = MyLinkedList(1,2)
        val list2 = MyLinkedList(1,2,3,4)

        list.deleteMiddle()
        list should === (MyLinkedList(2))
        list.length should === (1)

        list2.deleteMiddle()
        list2 should === (MyLinkedList(1,3,4))
        list2.length should === (3)
      }
    }

    "implement partition" must {

    }
  }
}
