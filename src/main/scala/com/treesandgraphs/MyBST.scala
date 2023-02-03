package com.treesandgraphs

sealed trait MyBST[+A] {
  def insert[B >: A : Ordering](elem: B): MyBST[B] = ???
  def remove[B >: A](elem: B): MyBST[B] = ???

  def depth: Int = ???
  def balance: MyBST[A] = ???
  def rotateRight: MyBST[A] = ???
  def rotateLeft: MyBST[A] = ???
  def rotateRightLeft: MyBST[A] = ???
  def rotateLeftRight: MyBST[A] = ???

  def inOrder: String = ???
  def preOrder: String = ???
  def postOrder: String = ???

  override def equals(obj: Any): Boolean = ???
  override def toString: String = inOrder
}
case class Branch[A](elem: A, left: MyBST[A] = Leaf, right: MyBST[A] = Leaf) extends MyBST[A]
case object Leaf extends MyBST[Nothing]

object MyBST {
  def apply[A: Ordering](): MyBST[A] = ???
}