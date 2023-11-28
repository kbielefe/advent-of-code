package day15
import parse.{*, given}

case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int):
  def score(quantity: Int): Int =
    quantity * (capacity + durability + flavor + texture)

type I = Vector[Ingredient ~ """(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)"""] - "\n"

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    val scores = for
      a <- (0 to 100).iterator
      b <- (0 to (100 - a)).iterator
      c <- (0 to (100 - a - b)).iterator
      d <- (0 to (100 - a - b - c)).iterator
    yield
      val capacity   = Math.max(0, input(0).capacity * a   + input(1).capacity * b   + input(2).capacity * c   + input(3).capacity * d)
      val durability = Math.max(0, input(0).durability * a + input(1).durability * b + input(2).durability * c + input(3).durability * d)
      val flavor     = Math.max(0, input(0).flavor * a     + input(1).flavor * b     + input(2).flavor * c     + input(3).flavor * d)
      val texture    = Math.max(0, input(0).texture * a    + input(1).texture * b    + input(2).texture * c    + input(3).texture * d)
      capacity * durability * flavor * texture
    scores.max

  def part2(input: I): Int =
    val scores = for
      a <- (0 to 100).iterator
      b <- (0 to (100 - a)).iterator
      c <- (0 to (100 - a - b)).iterator
      d <- (0 to (100 - a - b - c)).iterator
      if (a * input(0).calories + b * input(1).calories + c * input(2).calories + d * input(3).calories) == 500
    yield
      val capacity   = Math.max(0, input(0).capacity * a   + input(1).capacity * b   + input(2).capacity * c   + input(3).capacity * d)
      val durability = Math.max(0, input(0).durability * a + input(1).durability * b + input(2).durability * c + input(3).durability * d)
      val flavor     = Math.max(0, input(0).flavor * a     + input(1).flavor * b     + input(2).flavor * c     + input(3).flavor * d)
      val texture    = Math.max(0, input(0).texture * a    + input(1).texture * b    + input(2).texture * c    + input(3).texture * d)
      capacity * durability * flavor * texture
    scores.max
