package com.stacksandqueues

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.should

class MyStackSuite extends AnyWordSpec {
  "MyStack" should {
    "implement push" in {
      val s = MyStack[Int]()
      s.push(1)
      s.push(2)

      val s2: MyStack[Int] = MyStack()
      s2.push(1)
    }

    "implement peek" should {
      "throw exception when the stack is empty" in {
        val s = MyStack()
        intercept[IllegalStateException] {
          s.peek
        }
      }

      "return the correct element at the top of the stack without removing it" in {
        val s = MyStack[Int]()

        s.push(1)
        s.push(2)

        s.peek should === (2)
        s.peek should === (2)
      }
    }

    "implement pop" should {
      "throw exception when the stack is empty" in {
        val s = MyStack()

        intercept[IllegalStateException] {
          s.pop
        }
      }

      "return the correct value and remove it from the stack" in {
        val s = MyStack[Int]()

        s.push(1)
        s.push(2)

        s.pop should === (2)
        s.pop should === (1)

        intercept[IllegalStateException] {
          s.pop
        }
      }
    }

    "isEmpty should return correct value" in {
      val s = MyStack[Int]()

      s.isEmpty should === (true)
      s.push(1)

      s.isEmpty should === (false)
      s.pop

      s.isEmpty should === (true)
    }

    "implement sort" should {
      "sort the stack" in {
        val stack = MyStack[Int]()

        stack.push(5)
        stack.push(3)
        stack.push(4)
        stack.push(1)
        stack.push(2)

        MyStack.sort(stack)

        stack.pop should === (1)
        stack.pop should === (2)
        stack.pop should === (3)
        stack.pop should === (4)
        stack.pop should === (5)
      }
    }
  }
}
