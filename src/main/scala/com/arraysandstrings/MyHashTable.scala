package com.arraysandstrings

object MyHashTable {
  def apply[K,V](pairs: (K, V)*): MyHashTable[K,V] =
    val table = new MyHashTable[K,V]()
    for(pair <- pairs) {
      table.add(pair)
    }
    table
}

class MyHashTable[K,V]() {
  private var array = new Array[List[(K, V)]](10)
  private val growthRatio = .75
  private var elems = 0

  private def rehash(): Unit = ???
  def add(elem: (K, V)): Unit = ???
  def remove(key: K): Unit = ???
  def get(key: K): Option[V] = ???
  def keySet: Set[K] = ???
  def contains(key: K): Boolean = ???

  override def equals(obj: Any): Boolean = super.equals(obj)

  override def toString: String = super.toString
}
