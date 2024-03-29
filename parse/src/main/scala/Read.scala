package parse

import io.circe.*, io.circe.parser.*
import scala.util.matching.Regex

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

object Read:
  def apply[T <: Product : ReadProduct](regex: Regex): Read[T] = new Read[T]:
    def read(input: String): T =
      regex.unapplySeq(input) match
        case Some(fields) => summon[ReadProduct[T]].readProduct(fields.toArray)
        case None         => throw new Exception(s"Regex '${regex.regex}' did not match '$input'")
