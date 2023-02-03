package com.treesandgraphs

import scala.annotation.targetName

//this will be a ternary search tree
sealed trait MyTrie {
  def isEmpty: Boolean = ???
  def nonEmpty: Boolean = ???
  def size: Int = ???
  def depth: Int = ???
  def skips: Int = ???

  def first: String = ???
  def last: String = ???
  def longest: String = ???
  def words: List[String] = ???

  @targetName("add")
  def +(word: String): MyTrie = ???
  @targetName("remove")
  def -(word: String): MyTrie = ???
}
case class Prefix(char: Char, lt: MyTrie, eq: MyTrie, gt: MyTrie) extends MyTrie
case class Word(trie: MyTrie)
case object Empty extends MyTrie
