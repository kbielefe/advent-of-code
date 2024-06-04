package day15
import parse.{*, given}

type I = List[String]
given Read[List[String]] = Read(",")

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    println(hash("HASH"))
    input.map(hash).sum

  def part2(input: I): Int =
    val boxes = Vector.fill(256)(Vector.empty[Lens])
    val filled = input.foldLeft(boxes){(boxes, op) =>
      if op.endsWith("-") then
        val box = boxes(hash(op.init))
        val removed = box.filterNot(_.label == op.init)
        boxes.updated(hash(op.init), removed)
      else
        val Array(label, focalLength) = op.split("="): @unchecked
        val lens = Lens(label, focalLength.toInt)
        val box = boxes(hash(label))
        box.indexWhere(_.label == label) match
          case -1 => boxes.updated(hash(label), box.appended(lens))
          case i  => boxes.updated(hash(label), box.updated(i, lens))
    }
    focusingPower(filled)

  case class Lens(label: String, focalLength: Int)

  def hash(string: String): Int =
    string.foldLeft(0)((accum, char: Char) => (accum + char.toInt) * 17 % 256)

  def focusingPower(boxes: Vector[Vector[Lens]]): Int =
    boxes.zipWithIndex.map((box, boxIndex) => box.zipWithIndex.map((lens, slotIndex) => (boxIndex + 1) * (slotIndex + 1) * lens.focalLength).sum).sum
