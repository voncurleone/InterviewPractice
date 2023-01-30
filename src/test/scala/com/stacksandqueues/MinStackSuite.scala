package com.stacksandqueues

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.should

class MinStackSuite extends AnyWordSpec {
  "MinStack" should {
    "implement pop" should {
      "throw an exception if the stack is empty" in {
        val stack = MinStack[Int]()

        intercept[IllegalStateException] {
          stack.pop
        }
      }

      "pop the correct value from the stack and remove it" in {
        val stack = MinStack[Int]()

        stack.push(1)
        stack.push(2)

        stack.pop should === (2)
        stack.pop should === (1)

        intercept[IllegalStateException] {
          stack.pop
        }
      }
    }

    "implement push" should {
      "push a value onto the stack" in {
        val stack = MinStack[Int]()

        stack.push(1)
        stack.push(2)

        stack.isEmpty should === (false)
      }
    }

    "implement peek" should {
      "throw exception if the stack is empty" in {
        val stack = MinStack[Int]()

        intercept[IllegalStateException] {
          stack.peek
        }
      }

      "return the element of the stack without removing it" in {
        val stack = MinStack[Int]()

        stack.push(1)
        stack.push(2)

        stack.peek should === (2)
        stack.peek should === (2)
      }
    }

    "implement min" should {
      "throw exception if the stack is empty" in {
        val stack = MinStack[Int]()

        intercept[IllegalStateException] {
          stack.min
        }
      }

      "return the correct minimum value in the stack" in {
        val stack = MinStack[Int]()

        stack.push(3)
        stack.min should === (3)

        stack.push(2)
        stack.min should === (2)

        stack.push(1)
        stack.min should === (1)

        stack.push(4)
        stack.min should === (1)

        stack.pop
        stack.min should === (1)
      }
    }

    "implement isEmpty" should {
      "return empty if the tack is empty" in {
        val stack = MinStack[Int]()
        stack.isEmpty should === (true)

        stack.push(1)
        stack.isEmpty should === (false)

        stack.pop
        stack.isEmpty should === (true)
      }
    }
  }
}
