package com.stacksandqueues

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.should

class MyQueueSuite extends AnyWordSpec {
  "MyQueue" should {
    "implement push" in {
      val q: MyQueue[Int] = MyQueue()

      q.push(1)
      q.push(2)
    }

    "implement peek" should {
      "throw exception if queue is empty" in {
        val q = MyQueue[Int]()

        intercept[IllegalStateException] {
          q.peek
        }
      }

      "return the correct element without removing it from the queue" in {
        val q = MyQueue[Int]()

        q.push(1)
        q.push(2)

        q.peek should === (1)
        q.peek should === (1)
      }
    }

    "implement pop" should {
      "throw exception if the queue is empty" in {
        val q = MyQueue[Int]()

        intercept[IllegalStateException] {
          q.pop
        }
      }

      "return the correct element and remove it from the queue" in {
        val q = MyQueue[Int]()

        q.push(1)
        q.push(2)

        q.pop should === (1)
        q.pop should === (2)

        intercept[IllegalStateException] {
          q.pop
        }
      }
    }

    "implement isEmpty" in {
      val q = MyQueue[Int]()
      q.isEmpty should === (true)

      q.push(1)
      q.push(2)

      q.isEmpty should === (false)
      q.pop
      q.isEmpty should === (false)
      q.pop
      q.isEmpty should === (true)
    }
  }
}
