package com.arraysandstrings

object Exercises {
  //returns true if the string has all unique characters
  def isUnique(s: String): Boolean = ???

  //returns true if s1 is a permutation of s2
  def checkPermutation(s1: String, s2: String): Boolean = ???

  //replace all spaces with "%20" in place given a character array that has enough space for the added characters
  def urlify(s: Array[Char]): Array[Char] = ???

  //return true if a permutation of s is a palindrome. ignore casing and non-letter characters
  def palindromePermutation(s: String): Boolean = ???

  //return true if s1 is one character away from s2
  def oneAway(s1: String, s2: String): Boolean = ???

  //return a compressed version of s(only containing uppercase and lowercase letters) where the number of repeats
  //follows the character
  //ex: aabcccccaaa => a2bc5a3
  def stringCompression(s: String): String = ???

  //rotate a matrix by 90 degrees in place
  def rotateMatrix(m: Array[Array[Int]]): Array[Array[Int]] = ???

  //if a row has a zero in a matrix then set the whole row to zero in place
  def zeroMatrix(m: Array[Array[Int]]): Array[Array[Int]] = ???

  //return true if s1 is a rotation of s2 only using the substring method
  def stringRotation(s1: String, s2: String): Boolean = ???
 }
