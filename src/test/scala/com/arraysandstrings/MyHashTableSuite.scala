package com.arraysandstrings

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.should

trait tables {
  val table1: MyHashTable[String, Int] = MyHashTable(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
    "ten" -> 10,
    "twenty" -> 20
  )
  val table2: MyHashTable[String, Int] = MyHashTable("one" -> 1, "two" -> 2, "three" -> 3)
  val table3: MyHashTable[String, Int] = MyHashTable(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "eight" -> 8,
    "nine" -> 9,
    "ten" -> 10,
    "twenty" -> 20
  )
  val table: MyHashTable[String, Int] = MyHashTable(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
    "ten" -> 10
  )
}

class MyHashTableSuite extends AnyWordSpec{
  "MyHashTable" should {
    "be instantiable with no elements" in {
      val table = MyHashTable()
      table.size should === (0)
    }

    "be instantiable with one or more elements" in {
      val table = MyHashTable("0ne" -> 1)
      table.size should === (1)

      val table2 = MyHashTable("one" -> 1, "two" -> 2, "three" -> 3)
      table2.size should === (3)
    }

    "be able to add elements without rehashing" in {
      val table = MyHashTable("one" -> 1, "two" -> 2, "three" -> 3)
      val table2 = MyHashTable("one" -> 1, "two" -> 2, "three" -> 3, "five" -> 5)
      table.size should ===(3)

      table.add("five" -> 5)
      table.size should === (4)
      table should === (table2)
    }

    "be able to add elements causing a rehash and after a rehash" in{
      val tableOld = MyHashTable(
        "one" -> 1,
        "two" -> 2,
        "three" -> 3,
        "four" -> 4,
        "five" -> 5,
        "six" -> 6,
        "seven" -> 7,
        "eight" -> 8,
        "nine" -> 9,
        "ten" -> 10
      )
      tableOld.size should === (10)

      tableOld.add("twenty" -> 20)
      tableOld.size should === (11)
      new tables {
        tableOld should === (table1)
      }
    }

    "be able to remove before a rehash" in {
      val tableOld = MyHashTable("one" -> 1, "two" -> 2, "three" -> 3, "five" -> 5)

      new tables {
        tableOld.size should ===(4)

        tableOld.remove("five")
        tableOld.size should ===(3)
        tableOld should === (table2)
      }

    }

    "be able to remove after a rehash" in {
      new tables {
        table.size should === (10)

        table.remove("seven")
        table.add("twenty" -> 20)
        table.size should === (10)
        table should === (table3)
      }
    }

    "be able to get an element associated with a key that exists in the table" in {
      new tables {
        val elem: Option[Int] = table.get("three")
        elem should === (Some(3))

        table.add("zero" -> 0)
        table.get("zero") should === (Some(0))
      }
    }

    "be able to return None when getting an element associated with a key that has no entry in the table" in {
      new tables {
        table.get("twelve") should === (None)

        table.get("three") should === (Some(3))
        table.remove("three")
        table.get("three") should === (None)
      }
    }

    "be able to get the keyset for the table" in {
      new tables {
        Set(
          "one",
          "two",
          "three",
          "four",
          "five",
          "six",
          "eight",
          "nine",
          "ten",
          "twenty"
        ) should === (table3.keySet)
      }
    }

    "be able to return a boolean indicating whether the key is a part of the tables keyset" in {
      new tables {
        table3.contains("twenty") should === (true)
        table3.contains("twelve") should === (false)
      }
    }
  }
}
