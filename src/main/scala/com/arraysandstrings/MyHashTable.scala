package com.arraysandstrings

import scala.annotation.tailrec

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

  //initialize array
  for(i <- array.indices) {
    array(i) = Nil
  }

  private def rehash(): Unit =
    val temp = new Array[List[(K, V)]](array.length * 2)
    //initialize temp array
    for(i <- temp.indices) {
      temp(i) = Nil
    }

    for {
      list <- array
      elem <- list
    } {
      val (key, _) = elem
      val index = key.hashCode % temp.length
      temp(index) = elem :: temp(index)
    }
    array = temp

  def add(elem: (K, V)): Unit =
    if elems.toDouble / array.length > growthRatio then
      rehash()
      innerAdd(elem)
    else innerAdd(elem)

  private def innerAdd(elem: (K, V)): Unit =
    val (key, _) = elem
    val hash = key.hashCode % array.length
    val index = if hash < 0 then hash * -1 else hash
    array(index) = elem :: array(index)
    elems += 1

  def remove(key: K): Unit =
    val hash = key.hashCode % array.length
    val index = if hash < 0 then hash * -1 else hash
    array(index) = array(index).filter(_._1 != key)
    elems -= 1

  def get(key: K): Option[V] =
    val hash = key.hashCode % array.length
    val index = if hash < 0 then hash * -1 else hash
    array(index).filter(_._1 == key) match
      case Nil => None
      case (_, value) :: _ => Some(value)

  def keySet: Set[K] =
    var set = Set[K]()

    for {
      list <- array
      elem <-list
    } {
      val (key, _) = elem
      set += key
    }
    set

  def contains(key: K): Boolean = get(key) match
    case None => false
    case _ => true

  def size: Int = elems

  private def keyValSet: Set[(K, V)] =
    var set = Set[(K, V)]()

    for {
      list <- array
      elem <- list
    } {
      set += elem
    }
    set

  override def equals(obj: Any): Boolean = obj match
    case table: MyHashTable[K,V] =>
      if keyValSet == table.keyValSet then true
      else false
    case _ => false

  override def toString: String =
    var elemCount = elems

    @tailrec
    def createListString(list: List[(K, V)], builder: StringBuilder = StringBuilder("")): String = list match
      case Nil => builder.toString()
      case head :: Nil =>
        val (key, value) = head
        if elemCount == 1 then
          builder ++= s"($key -> $value)"
        else
          builder ++= s"($key -> $value), "
        elemCount -= 1
        builder.toString()

      case head :: tail =>
        val (key, value) = head
        builder ++= s"($key -> $value), "
        elemCount -= 1
        createListString(tail, builder)

    val builder = StringBuilder("")
    builder ++= "MyHashTable("

    for(i <- array.indices) {
      builder ++= createListString(array(i))
    }

    builder ++= ")"
    builder.toString()
}
