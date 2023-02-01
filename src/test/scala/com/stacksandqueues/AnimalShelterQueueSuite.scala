package com.stacksandqueues

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.should

val cat = Cat("pasta")
val dog = Dog("brody")

class AnimalShelterQueueSuite extends AnyWordSpec {
  "AnimalShelterQueue" should {
    "implement enqueue" should {
      "be able to add dogs" in {
        val asq = AnimalShelterQueue()

        asq.enqueue(dog)
        asq.dequeue should === (Some(dog))
      }

      "be able to add cats" in {
        val asq = AnimalShelterQueue()

        asq.enqueue(cat)
        asq.dequeue should === (Some(cat))
      }

      "be able to add cats and dogs" in {
        val asq = AnimalShelterQueue()

        asq.enqueue(dog)
        asq.enqueue(cat)

        asq.dequeue should === (Some(dog))
        asq.dequeue should === (Some(cat))
      }
    }

    "implement dequeue" should {
      "return None if the queue is empty" in {
        val asq = AnimalShelterQueue()

        asq.dequeue should === (None)
      }

      "return the correct animal in the queue" in {
        val asq = AnimalShelterQueue()

        asq.enqueue(dog)
        asq.enqueue(cat)

        asq.dequeue should ===(Some(dog))
        asq.dequeue should ===(Some(cat))
      }
    }

    "implement dequeueDog" should {
      "return None if the queue is empty" in {
        val asq = AnimalShelterQueue()

        asq.dequeueDog should ===(None)
      }

      "return none if the queue contains only cats" in {
        val asq = AnimalShelterQueue()

        asq.enqueue(cat)
        asq.enqueue(Cat("fred"))

        asq.dequeueDog should === (None)
      }

      "return the first dog in the queue" in {
        val asq = AnimalShelterQueue()

        asq.enqueue(cat)
        asq.enqueue(Cat("fred"))
        asq.enqueue(dog)
        asq.enqueue(Dog("spark"))

        asq.dequeueDog should === (Some(dog))
        asq.dequeue should === (Some(cat))
        asq.dequeueDog should === (Some(Dog("spark")))
      }
    }

    "implement dequeueCat" should {
      "return None if the queue is empty" in {
        val asq = AnimalShelterQueue()

        asq.dequeueCat should ===(None)
      }

      "return none if the queue only contains dogs" in {
        val asq = AnimalShelterQueue()

        asq.enqueue(dog)
        asq.enqueue(Dog("spark"))

        asq.dequeueCat should ===(None)
      }

      "return the first cat in the queue" in {
        val asq = AnimalShelterQueue()

        asq.enqueue(dog)
        asq.enqueue(Dog("spark"))
        asq.enqueue(cat)
        asq.enqueue(Cat("fred"))

        asq.dequeueCat should ===(Some(cat))
        asq.dequeue should ===(Some(dog))
        asq.dequeueCat should ===(Some(Cat("fred")))
      }
    }
  }
}
