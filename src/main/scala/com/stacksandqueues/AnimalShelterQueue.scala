package com.stacksandqueues

import com.linkedlist.{Cons, Empty, MyList}

import scala.annotation.tailrec

trait Animal
case class Dog(name: String) extends Animal
case class Cat(name: String) extends Animal

class AnimalShelterQueue {
  private var front = MyList[Animal]()
  private var end = MyList[Animal]()

  def enqueue(a: Animal): Unit = end = a :: end

  def dequeue: Option[Animal] =
    front = front.append(end.reverse)
    end = Empty
    front match
      case Empty => None
      case Cons(head, tail) =>
        front = tail
        Some(head)

  def dequeueDog: Option[Dog] =
    front = front.append(end.reverse)
    end = Empty

    @tailrec
    def loop(l: MyList[Animal], acc: MyList[Animal] = Empty): Option[Dog] = l match
      case Empty => None
      case Cons(head: Dog, tail) =>
        front = acc.append(tail).reverse
        Some(head)
      case Cons(head, tail) => loop(tail, head :: acc)
    loop(front)

  def dequeueCat: Option[Cat] =
    front = front.append(end.reverse)
    end = Empty

    @tailrec
    def loop(l: MyList[Animal], acc: MyList[Animal] = Empty): Option[Cat] = l match
      case Empty => None
      case Cons(head: Cat, tail) =>
        front = acc.append(tail).reverse
        Some(head)
      case Cons(head, tail) => loop(tail, head :: acc)
    loop(front)
}

object AnimalShelterQueue {
  def apply(): AnimalShelterQueue = new AnimalShelterQueue
}