package advent2020

import cats.implicits._
import common._
import monix.eval.Task
import monix.reactive.Observable

object Day14 extends StringsDay[Long, Long](2020, 14) {
  override def part1(input: Observable[String]): Task[Long] =
    input.foldLeftL((0L, 0L, Map.empty[Long, Long])){case ((andMask, orMask, memory), line) =>
      if (line.startsWith("mask")) {
        val s"mask = $maskString" = line
        val andMaskString = maskString.map(char => if (char == 'X') '1' else char)
        val orMaskString = maskString.map(char => if (char == 'X') '0' else char)
        val newAndMask = binaryStringToLong(andMaskString)
        val newOrMask = binaryStringToLong(orMaskString)
        (newAndMask, newOrMask, memory)
      } else {
        val s"mem[$address] = $value" = line
        val newMemory = memory.updated(address.toLong, value.toLong & andMask | orMask)
        (andMask, orMask, newMemory)
      }
    }.map(_._3.values.sum)

  override def part2(input: Observable[String]): Task[Long] =
    input.foldLeftL(("", Map.empty[Long, Long])){case ((mask, memory), line) =>
      if (line.startsWith("mask")) {
        val s"mask = $newMask" = line
        (newMask, memory)
      } else {
        val s"mem[$address] = $value" = line
        val newMemory = memory ++ addresses(maskedAddress(mask, address.toLong)).map(address => (address -> value.toLong))
        (mask, newMemory)
      }
    }.map(_._2.values.sum)

  private def addresses(address: String, acc: Long = 0): Iterator[Long] = {
    if (address.isEmpty) {
      Iterator(acc)
    } else {
      val bit = address.head
      val newAddress = address.tail
      if (bit == '0') {
        addresses(newAddress, acc * 2L)
      } else if (bit == '1') {
        addresses(newAddress, acc * 2L + 1L)
      } else {
        Iterator(addresses(newAddress, acc * 2L), addresses(newAddress, acc * 2L + 1L)).flatten
      }
    }
  }

  @scala.annotation.tailrec
  private def maskedAddress(mask: String, address: Long, acc: String = ""): String =
    if (mask.isEmpty) {
      acc
    } else {
      val addressBit = address & 1L
      val maskBit = mask.last
      val newAddress = address >> 1
      val newMask = mask.init
      val newBit = if (maskBit == '0') {
        if (addressBit == 0) '0' else '1'
      } else {
        maskBit
      }
      maskedAddress(newMask, newAddress, s"$newBit$acc")
    }

  private def binaryStringToLong(string: String): Long =
    string.foldLeft(0L){case (acc, digit) =>
      acc * 2L + (if (digit == '1') 1L else 0L)
    }
}
