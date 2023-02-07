package com.treesandgraphs

sealed trait MyBST[+A] {
  def insert[B >: A : Ordering](elem: B): MyBST[B] = ???
  def remove[B >: A](elem: B): MyBST[B] = ???

  def depth: Int
  def balanceFactor: Int = ???
  def balance: MyBST[A] = ???
  def isBalanced: Boolean = ???
  def rotateRight: MyBST[A] = ???
  def rotateLeft: MyBST[A] = ???
  def rotateRightLeft: MyBST[A] = ???
  def rotateLeftRight: MyBST[A] = ???

  def inOrder: String = ???
  def preOrder: String = ???
  def postOrder: String = ???
  
  override def toString: String = inOrder
}
case class Branch[A](elem: A, var left: MyBST[A] = Leaf, var right: MyBST[A] = Leaf) extends MyBST[A] {
  def depth =
    if left == Leaf && right == left then 0
    else if left.depth > right.depth then left.depth + 1
    else right.depth + 1
    /*if left.depth > right.depth then left.depth + 1
    else right.depth + 1*/
}
case object Leaf extends MyBST[Nothing] {
  def depth: Int = 0
}

object MyBST {
  def apply[A: Ordering](): MyBST[A] = Leaf
}