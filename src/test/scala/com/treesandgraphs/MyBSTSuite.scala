package com.treesandgraphs

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.should

class MyBSTSuite extends AnyWordSpec {
  "MyBST" should {
    "implement insert" should {
    }

    "implement remove" should {
    }

    "implement depth" should {
      "return 0 for a leaf" in {
        Leaf.depth should === (0)
      }

      "return 1 for a branch with no children" in {
        Branch(1).depth should === (1)
      }

      "return correct depth for branch with multiple children" in {
        val left = Branch(5)
        val right = Branch(10, Branch(7), Branch(11, Leaf, Branch(12)))
        val root = Branch(6, left, right)
        val root2 = Branch(10, Branch(5, Branch(3)))

        root.depth should === (4)
        left.depth should === (1)
        right.depth should === (3)
        right.right.depth should === (2)
        root2.depth should === (3)
      }
    }

    "implement balance" should {
    }

    "implement rotateRight" should {
    }

    "implement rotateLeft" should {
    }

    "implement rotateRightLeft" should {
    }

    "implement rotateLeftRight" should {
    }

    "implement inOrder" should {
    }

    "implement preOrder" should {
    }

    "implement postOrder" should {
    }

    "implement equals" should {
    }

    "implement toString" should {
    }
  }
}
