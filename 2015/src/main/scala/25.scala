package day25

import algorithms.Matrix
import parse.{*, given}

type I = (Int, Int) ~ """.*row (\d+), column (\d+)\."""

object Puzzle extends runner.Day[I, BigInt, BigInt]:
  def part1(input: I): BigInt =
    val (row, col) = input
    val init  = Matrix.colVector[3, Int](1, 1, 1)
    val m     = Matrix[3, 3, Int](List(1, 1, 0), List(0, 1, 1), List(0, 0, 1))
    val toCol = Matrix[3, 3, Int](List(1, 0, 0), List(0, 1, 1), List(0, 0, 1))
    val codeNumber = (m.pow(col-1) * toCol * m.pow(row-1) * init).element[0, 0]
    BigInt(20151125) * BigInt(252533).modPow(BigInt(codeNumber-1), BigInt(33554393)) % BigInt(33554393)

  def part2(input: I): BigInt =
    ???
