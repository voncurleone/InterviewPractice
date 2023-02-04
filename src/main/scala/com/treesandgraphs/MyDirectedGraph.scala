package com.treesandgraphs

case class Node(name: String, var children: List[String])

class MyDirectedGraph {
  private var list = Vector[Node]()

  def addEdge(e: (String, String)): Unit = ???
  def addVert(v: String): Unit = ???

  override def toString: String = ???
}
