package com.stacksandqueues

import com.arraysandstrings.MyArrayList

trait Animal
case class Dog(name: String) extends Animal
case class Cat(name: String) extends Animal

class AnimalShelterQueue {
  private val list = MyArrayList[Animal]()

  def enqueue(animal: Animal): Unit = ???
  def dequeueAny: Option[Animal] = ???
  def dequeueDog: Option[Dog] = ???
  def dequeue: Option[Cat] = ???
}

object AnimalShelterQueue {
  def apply(): AnimalShelterQueue = new AnimalShelterQueue
}