package day9
import parse.{*, given}

type FileSystem = Vector[Option[Int]]

given Read[FileSystem] = summon[Read[String]].map: input =>
  input.toVector.map(_.asDigit).zipWithIndex.flatMap: (digit, index) =>
    val isFile = index % 2 == 0
    val id = index / 2
    if isFile then List.fill(digit)(Some(id)) else List.fill(digit)(None)

extension (blocks: FileSystem)
  def availableFreeSpace(size: Int, leftOf: Int): Option[Long] =
    blocks.take(leftOf).sliding(size).zipWithIndex.find(_._1.forall(!_.isDefined)).map(_._2)

  def checksum: Long =
    blocks.zipWithIndex.collect{case (Some(id), index) => id.toLong * index}.sum

  def print: FileSystem =
    println(blocks.map(_.map(_.toString).getOrElse(".")).mkString)
    blocks

  def compact: FileSystem =
    blocks.zipWithIndex.reverse.foldLeft(blocks):
      case (blocks, (Some(_), from)) => blocks.availableFreeSpace(1, from) match
        case Some(to) => blocks.patch(to.toInt, blocks.slice(from, from + 1), 1).patch(from.toInt, List(None), 1)
        case None => blocks
      case (blocks, _) => blocks

  def defrag: FileSystem =
    ???

object Puzzle extends runner.Day[FileSystem, Long, Long]:
  def part1(blocks: FileSystem): Long =
    blocks.compact.checksum
    
  def part2(blocks: FileSystem): Long =
    blocks.defrag.checksum
