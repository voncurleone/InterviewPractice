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
  def urlify(s: Array[Char]): Array[Char] =
    //lns => last non space [index]
    var lns = s.length - (s.reverse.takeWhile(_ == ' ').length + 1)

    for(i <- s.indices) {
      if s(i) == ' ' then
        var temp1 = s(i + 1)
        var temp2 = s(i + 2)

        for(j <- i + 3 to lns) {
          val hold = s(j)
          s(j) = temp1
          temp1 = temp2
          temp2 = hold
        }

        s(lns + 1) = temp1
        s(lns + 2) = temp2

        s(i) = '%'
        s(i + 1) = '2'
        s(i + 2) = '0'
        lns += 2
    }
    s

  //return true if a permutation of s is a palindrome. ignore casing and non-letter characters
  def palindromePermutation(s: String): Boolean =
    val chars = s.filter(_.isLetter).map(_.toLower).groupBy(identity)
    var used = false //used our char that can have 1 or more occurrences. all others must be 2

    for(t <- chars) {
      val (_, letters) = t
      if used then
        if letters.length % 2 != 0 then return false
      else
        if letters.length % 2 != 0 then used = true
    }
    true

  //return true if s1 is one character away from s2
  def oneAway(s1: String, s2: String): Boolean =
    val (short, long) = if s1.length > s2.length then (s2, s1) else (s1, s2)
    var one = false

    for(i <- short.indices) {
      if i < s2.length then
        if s1(i) != s2(i) then
          if one then
            return false
          else
            one = true
    }
    if short.length + 1 == long.length && !one then true
    else
      if short.length == long.length then true else false


  //return a compressed version of s(only containing uppercase and lowercase letters) where the number of repeats
  //follows the character
  //ex: aabcccccaaa => a2bc5a3
  def stringCompression(s: String): String =
    if s.isEmpty then return ""
    val builder = StringBuilder("")

    var count = 0
    var current = s(0)
    for(c <- s) {
      if c == current then
        count += 1
      else
        if count > 1 then builder ++= s"$current$count"
        else builder ++= current.toString
        current = c
        count = 1

    }
    if count > 1 then builder ++= s"$current$count"
    else builder ++= current.toString
    builder.toString()

  //rotate a matrix by 90 degrees in place
  def rotateMatrix(m: Array[Array[Int]]): Array[Array[Int]] =
    for {
      r <- m.indices
      c <- 0 until m(r).length - r
    } {
      val nr = m(r).length - c -1
      val nc = r
      val temp = m(nr)(nc)
      m(nr)(nc) = m(r)(c)
      m(r)(c) = temp
    }
    m

  //if a row has a zero in a matrix then set the whole row to zero in place
  def zeroMatrix(m: Array[Array[Int]]): Array[Array[Int]] = ???

  //return true if s1 is a rotation of s2 only using the substring method
  def stringRotation(s1: String, s2: String): Boolean = ???
 }
