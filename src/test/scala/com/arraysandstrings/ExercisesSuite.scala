package com.arraysandstrings

import org.scalatest.matchers.should.Matchers.should
import org.scalatest.wordspec.AnyWordSpec

class ExercisesSuite extends AnyWordSpec{
  import Exercises._

  "isUnique" must {
    "return true for an empty string" in {
      isUnique("") should === (true)
    }

    "return true when there are no duplicates" in {
      isUnique("a") should === (true)
      isUnique("senfuromp") should === (true)
      isUnique("qwertyuiplkajshdgf62739485") should === (true)
    }

    "return false when there are duplicate characters" in {
      isUnique("aa") should === (false)
      isUnique("aqwertyuiopljkhgfdszxcvbnma") should === (false)
      isUnique("qowieurythhskdlfcmzb") should === (false)
    }
  }

  "checkPermutation" must {
    "return true for strings that are permutations of another" in {
      checkPermutation("abc", "acb") should === (true)
      checkPermutation("bim shang", " mhaignsb") should === (true)
    }

    "return false for strings that are not permutations" in {
      checkPermutation("dog ", "dog") should === (false)
      checkPermutation("dog", "cat") should === (false)
    }
  }

  "urlify" must {
    "replace all ' ' with '%20" in {
      urlify("hello friend  ".toCharArray) should ===("hello%20friend".toCharArray)
      urlify("NoSpaces".toCharArray) should ===("NoSpaces".toCharArray)
      urlify("multiple spaces in this      ".toCharArray) should ===("multiple%20spaces%20in%20this".toCharArray)
      intercept[IndexOutOfBoundsException] {
        urlify("multiple spaces in this     ".toCharArray)
      }
    }
  }

  "palindromePermutation" must {
    "remove all non letter characters before analysing the string" in {
      palindromePermutation("qqwwee666 ") should === (true)
      palindromePermutation("aaabbb3 ") should === (false)
    }

    "be able to recognize palindrome permutations" in {
      palindromePermutation("aaabbcccc") should === (true)
      palindromePermutation("abbnnnnmmmmmm") should === (true)
      palindromePermutation("abcabc") should === (true)
    }

    "return false if there is no permutation that is a palindrome" in {
      palindromePermutation("aaabbbcc") should === (false)
      palindromePermutation("ab") should === (false)
    }
  }

  "oneAway" must {
    "provide correct result when strings are not the same length" in {
      oneAway("pur", "pure") should === (true)
      oneAway("pure", "pur") should === (true)

      oneAway("pure", "pu") should === (false)
      oneAway("pu", "pure") should === (false)

      oneAway("pu", "per") should === (false)
      oneAway("per", "pu") should === (false)
    }

    "provide correct result when strings are the same length" in {
      oneAway("pure", "pure") should === (true)
      oneAway("pure", "pore") should === (true)
      oneAway("pure", "lure") should === (true)
      oneAway("pure", "purs") should === (true)

      oneAway("pure", "llre") should === (false)
      oneAway("pure", "lurl") should === (false)
    }
  }

  "stringCompression" must {
    "return empty string when given empty string" in {
      stringCompression("") should === ("")
    }

    "leave a string that cant be compressed the same" in {
      stringCompression("asdfgh") should === ("asdfgh")
      stringCompression("qwertASD") should === ("qwertASD")
    }

    "compress properly" in {
      stringCompression("aaaYYYYnMtt") should === ("a3Y4nMt2")
      stringCompression("tOObbbY") should === ("tO2b3Y")
    }
  }

  "rotateMatrix" must {
    "rotate a square matrix" in {
      val in = Array(
        Array(1,2,3),
        Array(4,5,6),
        Array(7,8,9)
      )
      val exp = Array(
        Array(7,4,1),
        Array(8,5,2),
        Array(9,6,3)
      )
      rotateMatrix(in) should === (exp)

      val in2 = Array(
        Array(1,2,3,4),
        Array(5,6,7,8),
        Array(9,10,11,12),
        Array(13,14,15,16)
      )
      val exp2 = Array(
        Array(13,9,5,1),
        Array(14,10,6,2),
        Array(15,11,7,3),
        Array(16,12,8,4)
      )
      rotateMatrix(in2) should === (exp2)

      intercept[IllegalArgumentException] {
        val in3 = Array(
          Array(13, 9, 5, 1),
          Array(14, 10, 6, 2),
          Array(15, 11, 7),
          Array(16, 12, 8, 4)
        )

        rotateMatrix(in3)
      }

      intercept[IllegalArgumentException] {
        val in3 = Array(
          Array(13, 9, 5, 1, 0),
          Array(14, 10, 6, 2, 0),
          Array(15, 11, 7, 8, 0),
          Array(16, 12, 8, 4, 0)
        )

        rotateMatrix(in3)
      }
    }
  }

  "zeroMatrix" must {
    "throw exception if input matrix is not m x n" in {
      val in = Array(
        Array(13, 9, 5, 1, 0),
        Array(14, 10, 6, 2, 0),
        Array(15, 11, 7, 8, 0),
        Array(16, 12, 8, 4, 0)
      )

      zeroMatrix(in)
      val in2 = Array(
        Array(13, 9, 5, 1, 0),
        Array(14, 10, 6, 2, 0),
        Array(15, 11, 7, 8),
        Array(16, 12, 8, 4, 0)
      )

      intercept[IllegalArgumentException] {
        zeroMatrix(in2)
      }
    }

    "zero an array if it contains s zero" in {
      val in = Array(
        Array(0, 9, 5, 1, 1),
        Array(14, 10, 6, 2, 1),
        Array(15, 11, 7, 8, 1),
        Array(16, 12, 8, 4, 0)
      )
      val exp = Array(
        Array(0, 0, 0, 0, 0),
        Array(14, 10, 6, 2, 1),
        Array(15, 11, 7, 8, 1),
        Array(0, 0, 0, 0, 0)
      )

      zeroMatrix(in) should === (exp)

      val in2 = Array(
        Array(1, 9, 5, 1, 1),
        Array(14, 10, 0, 2, 1),
        Array(15, 0, 7, 8, 1),
        Array(16, 12, 8, 4, 1)
      )
      val exp2 = Array(
        Array(1, 9, 5, 1, 1),
        Array(0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0),
        Array(16, 12, 8, 4, 1)
      )

      zeroMatrix(in2) should === (exp2)
    }
  }

  "stringRotation" must {
    "return true for identical strings" in {
      stringRotation("12345", "12345") should === (true)
    }

    "find 1 rotation left or right" in {
      stringRotation("12345", "23451") should === (true)
      stringRotation("12345", "51234") should === (true)
    }

    "throw exception if strings have different length" in {
      intercept[IllegalArgumentException] {
        stringRotation("12", "123")
      }
    }

    "Return false when strings are not a rotation" in {
      stringRotation("12345", "34567") should === (false)
      stringRotation("nbvcxz", "qwerty") should === (false)
    }

    "other amounts of rotation in both directions" in {
      stringRotation("1234", "3412") should === (true)
      stringRotation("1234567", "4567123") should === (true)
      stringRotation("1234567", "6712345") should === (true)
    }
  }
}
