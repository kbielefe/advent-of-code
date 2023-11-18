package algorithms

import math.Integral.Implicits.*

extension [N](value: N)(using n: Integral[N])
  def digits = value.toString map {_.asDigit}

  def isPrime = ???

  def primeFactors: List[N] = ???

  infix def gcd(other: N): N =
    if n.equiv(other, n.zero) then
      value.abs
    else
      other gcd (value % other)

  def toRoman: String =
    val number = value.toInt

    if (number <= 0) return ""

    val conversions = List(
      (1000, "M"),
      (900,  "CM"),
      (500,  "D"),
      (400,  "CD"),
      (100,  "C"),
      (90,   "XC"),
      (50,   "L"),
      (40,   "XL"),
      (10,   "X"),
      (9,    "IX"),
      (5,    "V"),
      (4,    "IV"),
      (1,    "I"))

    val (decimal, roman) =
      (conversions dropWhile (_._1 > number)).head

    roman + (number - decimal).toRoman
  end toRoman

  // Handles up to 4 digit numbers
  def toWord: String =
    val digitWords = Array("", "one", "two", "three", "four", "five", "six",
      "seven", "eight", "nine")

    val teenWords = Array("ten", "eleven", "twelve", "thirteen", "fourteen",
      "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")

    val tensWords = Array("", "ten", "twenty", "thirty", "forty", "fifty",
      "sixty", "seventy", "eighty", "ninety")

    def thousands(n: Int) =
      val digit = n / 1000
      Option.when(digit > 0)(digitWords(digit) ++ " thousand")

    def hundreds(n: Int) =
      val digit = n % 1000 / 100
      Option.when(digit > 0)(digitWords(digit) ++ " hundred")

    def tensAndOnes(n: Int) =
      val tensDigit = n % 100 / 10
      val onesDigit = n % 10
      if n == 0 then
        Some("zero")
      else if tensDigit == 0 && onesDigit != 0 then
        Some(digitWords(onesDigit))
      else if tensDigit == 1 then
        Some(teenWords(onesDigit))
      else if tensDigit > 1 && onesDigit != 0 then
        Some(tensWords(tensDigit) ++ "-" ++ digitWords(onesDigit))
      else if tensDigit > 1 then
        Some(tensWords(tensDigit))
      else
        None

    def andWord(n: Int) =
      val tens = n % 100
      Option.when(tens != 0 && n > 99)("and")

    val n = value.toInt

    List(thousands(n), hundreds(n), andWord(n), tensAndOnes(n)).flatten.mkString(" ")
  end toWord

object Number:
  def fromRoman[N](roman: String)(using n: Integral[N]) =
    val digits = roman map Map(
      'M' -> 1000,
      'D' -> 500,
      'C' -> 100,
      'L' -> 50,
      'X' -> 10,
      'V' -> 5,
      'I' -> 1)

    def inversion(digits: Seq[Int]) =
      digits.size > 1 && digits(0) < digits(1)

    val subtractive = digits sliding 2 filter inversion map (_.head)

    n.fromInt(digits.sum - subtractive.sum * 2)

