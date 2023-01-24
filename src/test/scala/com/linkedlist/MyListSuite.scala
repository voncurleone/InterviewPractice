package com.linkedlist

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.should

val empty = MyList.empty[Int]
val two = MyList(1,2)
val three = MyList(1,2,3)

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

      "append an element with +:" in {
        val list = 1 +: empty
        list.length should ===(1)
        list should ===(Cons(1, Empty))
        list.contains(1) should ===(true)
        list.isEmpty should ===(false)
        list.head should ===(1)
        list.tail should ===(empty)
        list.toString should ===("MyList(1)")
        list(0) should ===(1)
      }

      "toList should return Nil" in {
        val list = empty.toList
        list should === (Nil)
      }

      "head, tail, and apply should throw exceptions" in {
        intercept[NoSuchElementException] {
          empty.head
        }

        intercept[NoSuchElementException] {
          empty.tail
        }

        intercept[NoSuchElementException] {
          empty(0)
        }
        intercept[NoSuchElementException] {
          empty(-1)
        }
      }

      "take and drop should return empty unless n < 0" in {
        empty.take(1) should === (Empty)
        empty.drop(2) should === (Empty)

        intercept[IllegalArgumentException] {
          empty.take(-2)
        }

        intercept[IllegalArgumentException] {
          empty.drop(-1)
        }
      }

      "length must return 0" in {
        empty.length should === (0)
      }

      "reverse should return empty" in {
        empty.reverse should === (Empty)
      }

      "return the MyList that is being appended" in {
        val list = empty.append(two)
        list should === (two)
      }
    }

    "Non empty" should {
      "append with ::" in {
        val list = 0 :: two
        list.length should ===(3)
        list should ===(Cons(0, two))
        list.contains(0) should ===(true)
        list.contains(3) should === (false)
        list.isEmpty should ===(false)
        list.head should ===(0)
        list.tail should ===(two)
        list.toString should ===("MyList(0,1,2)")
        list(0) should ===(0)
      }

      "append an element with +:" in {
        val list = 0 +: two
        list.length should ===(3)
        list should ===(MyList(1,2,0))
        list.contains(0) should ===(true)
        list.contains(3) should ===(false)
        list.isEmpty should ===(false)
        list.head should ===(1)
        list.tail should ===(MyList(2,0))
        list.toString should ===("MyList(1,2,0)")
        list(0) should ===(1)
      }

      "return the correct list with toList" in {
        val list = two.toList
        list should ===(List(1,2))
      }

      "return false with isEmpty" in {
        two.isEmpty should === (false)
      }

      "return the first elem with head" in {
        two.head should === (1)
        three.head should === (1)
      }

      "return the correct tail" in {
        two.tail should === (MyList(2))
        three.tail should === (MyList(2,3))
      }

      "apply should return correct elem" in {
        three(0) should === (1)
        three(1) should === (2)
        three(2) should === (3)
      }

      "throw exception if i is out of bounds for apply" in {
        intercept[IllegalArgumentException] {
          three(-1)
        }

        intercept[NoSuchElementException] {
          three(4)
        }
      }

      "take throws exception if n is < 0" in {
        intercept[IllegalArgumentException] {
          three.take(-1)
        }
      }

      "take returns the while list if n is larger than size" in {
        three.take(42) should === (MyList(1,2,3))
      }

      "take returns a list with the correct amount of elements" in {
        three.take(0) should ===(Empty)
        three.take(1) should === (MyList(1))
        three.take(2) should === (MyList(1,2))
        three.take(3) should ===(three)
      }


    }
  }
}
