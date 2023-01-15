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
}
