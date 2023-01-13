package com.arraysandstrings

object Exercises {
  //returns true if the string has all unique characters( ASCII only)
  def isUnique(s: String): Boolean =
    //check if the string is longer than the number of ascii charaters(128)
    if s.length > 128 then return false
    var set: Set[Char] = Set()
    for(i <- s.indices) {
      if set.contains(s.charAt(i)) then return false
      set = set + s.charAt(i)
    }
    true

  //returns true if s1 is a permutation of s2
  def checkPermutation(s1: String, s2: String): Boolean =
    if s1.length != s2.length then return false
    val sorted1 = s1.sorted
    val sorted2 = s2.sorted
    sorted1 == sorted2

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
