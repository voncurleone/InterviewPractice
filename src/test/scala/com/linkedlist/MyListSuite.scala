package com.linkedlist

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.should

val empty = MyList.empty[Int]
val one = MyList(1)
val two = MyList(1,2)
val three = MyList(1,2,3)
val four = MyList(1,2,3,4)

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

      "toString should return the correct string" in {
        empty.toString should === ("MyList()")
      }

      "flatten on empty should return empty" in {
        val emptyL = MyList.empty[MyList[MyList[Int]]]
        MyList.flatten(emptyL) should === (Empty)
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
        three.take(2).length should === (2)
        three.take(3) should ===(three)
      }

      "drop throws exception if n < 0" in {
        intercept[IllegalArgumentException] {
          three.drop(-1)
        }
      }

      "drop returns Empty if n > than size" in {
        three.drop(42) should === (Empty)
      }

      "drop returns returns the list - the first n elements" in {
        three.drop(0) should === (three)
        three.drop(1) should === (MyList(2,3))
        three.drop(1).length should === (2)
        three.drop(2) should === (MyList(3))
        three.drop(3) should === (Empty)
      }

      "length should provide the number of elems in the list" in {
        two.length should === (2)
        three.length should === (3)
      }

      "reverse should return the list with the opposite order of elems" in {
        two.reverse should === (MyList(2,1))
        three.reverse should === (MyList(3,2,1))
      }

      "contains should return false if the list does not contain the elem" in {
        two.contains(3) should === (false)
        three.contains(4) should === (false)
      }

      "contains should return true if the list contains the elem" in {
        two.contains(1) should === (true)
        three.contains(2) should === (true)
      }

      "toString should return the correct string to represent the list" in {
        three.toString should === ("MyList(1,2,3)")
        two.toString should === ("MyList(1,2)")
      }

      "flatten should flatten a non empty List" in {
        val list = MyList(MyList(1,2), Empty, MyList(3,4), Empty, MyList(5))
        MyList.flatten(list) should === (MyList(1,2,3,4,5))

        val list2 = MyList(MyList(1,2), MyList(3,4), MyList(5))
        MyList.flatten(list2) should === (MyList(1,2,3,4,5))
      }
    }

    "exercises" should {
      val noDupes = MyList(1,2,3,4,5,6,7,8,9)
      val dupes = MyList(1,1,2,3,4,5,5,6,7,7,8,9,9)
      "remove dupes returns the same list if there are no dupes" in {
        MyList.removeDupes(noDupes) should === (noDupes)
      }

      "remove dupes should remove all duplicate elems" in {
        MyList.removeDupes(dupes) should === (noDupes)
      }

      "delete middle returns Empty if the list is empty" in {
        MyList.deleteMiddle(empty) should === (Empty)
      }

      "delete middle removes the one elem in a list with an odd length" in {
        MyList.deleteMiddle(three) should === (MyList(1,2))
      }

      "delete middle removes the middle 2 elements" in {
        MyList.deleteMiddle(two) should === (Empty)
        MyList.deleteMiddle(four) should === (MyList(1,4))
      }

      val pList = MyList(5,8,1,3,2,9)
      "partition on empty will return empty" in {
        MyList.partition(4, empty) should === (Empty)
        MyList.partition(-4, empty) should === (Empty)
      }

      "partition with n greater than list should reverse list" in {
        MyList.partition(10, pList) should === (MyList(9,2,3,1,8,5))
      }

      "partition with n less than list elems should return list " in {
        MyList.partition(0, pList) should === (pList)
      }

      "partition should work on non edge cases" in {
        MyList.partition(3, pList) should === (MyList(2,1,5,8,3,9))
        MyList.partition(5, pList) should === (MyList(2,3,1,5,8,9))
        MyList.partition(9, pList) should === (MyList(2,3,1,8,5,9))
      }

      "sum should return empty if both lists are empty" in {
        MyList.sum(empty, empty) should === (Empty)
      }

      "sum should return list that is not empty if one is empty" in {
        MyList.sum(empty, three) should === (three)
        MyList.sum(two, empty) should === (two)
      }

      "should calculate the sum properly" in {
        val l1 = MyList(7,1,6)
        val l2 = MyList(5,9,5)

        MyList.sum(l1, l2) should === (MyList(2,1,2,1))

        val l3 = MyList(1,2)
        val l4 = MyList(1)

        MyList.sum(l3, l4) should === (MyList(2,2))
      }

      "palindrome should return true if it is empty or contains only one elem" in {
        MyList.palindrome(empty) should === (true)
        MyList.palindrome(one) should === (true)
      }

      "palindrome should return true for even and odd length palindrome" in {
        MyList.palindrome(MyList(0,1,2,1,0)) should === (true)
        MyList.palindrome(MyList(0,1,2,2,1,0)) should === (true)
      }

      "palindrome should return true for long middles" in {
        MyList.palindrome(MyList(0,1,2,2,2,1,0)) should ===(true)
        MyList.palindrome(MyList(0,1,2,2,2,2,1,0)) should ===(true)
      }
    }
  }
}
