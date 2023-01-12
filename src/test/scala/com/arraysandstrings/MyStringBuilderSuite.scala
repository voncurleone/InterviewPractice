package com.arraysandstrings

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.should


class MyStringBuilderSuite extends AnyWordSpec {
  "MyStringBuilder" should {
    "build the correct string" in {
      val b = MyStringBuilder()
      b add "Hello "
      b add "world"
      b add "!"

      b.toString should === ("Hello world!")

      b add " 1"
      b add " 2"
      b add " 3"
      b add " 4"
      b add " 5"
      b add " 6"
      b add " 7"
      b add " 8"
      b add " 9"

      b.toString should === ("Hello world! 1 2 3 4 5 6 7 8 9")
    }
  }
}
