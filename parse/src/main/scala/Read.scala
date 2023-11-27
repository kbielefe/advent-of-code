package parse

import io.circe.*, io.circe.parser.*

trait Read[A]:
  def read(input: String): A

given Read[Int] with
  def read(input: String): Int =
    input.toInt

given Read[Long] with
  def read(input: String): Long =
    input.toLong

given Read[BigInt] with
  def read(input: String): BigInt =
    BigInt(input)

given Read[String] with
  def read(input: String): String =
    input

given Read[Char] with
  def read(input: String): Char =
    if input.size == 1 then input.head else throw new Exception(s"Unable to parse $input into a Char")

given Read[Json] with
  def read(input: String): Json =
    parse(input).getOrElse(throw new Exception(s"Unable to parse $input as Json"))
