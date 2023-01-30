package com.stacksandqueues

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.should

class ListSetSuite extends AnyWordSpec {
  "ListStack" should {
    "implement push" should {
      "add elements to the list stack" in {
        val stack = ListStack[Int]()

        stack.push(1)
        stack.isEmpty should === (false)
      }

      "add enough elements to require a new stack to be created in the list" in {
        val stack = ListStack[Int]()

        for( i <- 0 until 15) {
          stack.push(i)
        }

        stack.length should ===(2)
      }
    }

    "implement pop" should {
      "throw exception if all stacks are empty" in {
        val stack = ListStack[Int]()

        intercept[IllegalStateException] {
          stack.pop
        }
      }

      "pop when there is only one stack in the list" in {
        val stack = ListStack[Int]()

        stack.push(1)
        stack.pop should ===(1)
        stack.isEmpty should ===(true)
      }

      "pop from the last stack when there are multiple stacks in the list" in {
        val stack = ListStack[Int]()

        for (i <- 0 until 15) {
          stack.push(i)
        }

        stack.pop should ===(14)
      }

      "resize to the appropriate length when a stack is emptied" in {
        val stack = ListStack[Int]()

        for(i <- 0 until 11) {
          stack.push(i)
        }

        stack.length should === (2)
        stack.pop should === (10)
        stack.length should === (1)
      }
    }

    "implement peek" should {
      "throw exception if the ListStack is empty" in {
        val stack = ListStack[Int]()

        intercept[IllegalStateException] {
          stack.peek
        }
      }

      "peek at the top element when there is only one stack" in {
        val stack = ListStack[Int]()

        stack.push(1)
        stack.peek should === (1)
        stack.isEmpty should === (false)
      }

      "peek at the top of the last stack in the list when it contains multiple stacks" in {
        val stack = ListStack[Int]()

        for(i <- 0 until 15) {
          stack.push(i)
        }

        stack.peek should === (14)
      }
    }

    "implement popAt" should {
      "throw an exception if the index given is out of bounds" in {
        val stack = ListStack[Int]()

        intercept[IllegalArgumentException] {
          stack.popAt(0)
        }

        stack.push(1)
        stack.length should === (1)
        stack.popAt(1) should === (1)
        stack.isEmpty should === (true)

        stack.push(2)
        intercept[IllegalArgumentException] {
          stack.popAt(-1)
        }

        intercept[IllegalArgumentException] {
          stack.popAt(2)
        }
      }
      
      
    }
  }
}
