package day9
import algorithms.PriorityQueue
import parse.{*, given}

case class File(id: Int, position: Int, size: Int)
case class Block(id: Int, position: Int)
case class Free(position: Int, size: Int)

class FileSystem(free: List[Free], blocks: Set[Block], files: Set[File]):
  def reverseFiles: Iterator[File] =
    ???

  def reverseBlocks: Iterator[Block] =
    blocks.toList.sortBy(-_.position).iterator

  def moveToFree(file: File): FileSystem =
    ???

  def moveToFree(block: Block): FileSystem =
    val Free(position, size) = free.head
    if position < block.position then
      val newFree = if size > 1 then Free(position + 1, size - 1) :: free.tail else free.tail
      new FileSystem(newFree, blocks - block + Block(block.id, position), files)
    else
      this

  def checksum: Long =
    blocks.toList.map(block => block.id.toLong * block.position.toLong).sum

  override def toString: String =
    val maxPosition = (free.map(_.position) ++ blocks.map(_.position)).max
    (0 to maxPosition).map(pos => blocks.find(_.position == pos).map(_.id.toString).getOrElse(".")).mkString

  def compact: FileSystem =
    reverseBlocks.foldLeft(this)(_.moveToFree(_))

  def defrag: FileSystem =
    reverseFiles.foldLeft(this)(_.moveToFree(_))

given Read[FileSystem] = summon[Read[String]].map: input =>
  val positions = input.map(_.asDigit).scanLeft(0)(_ + _)
  val free = input.map(_.asDigit).zip(positions).zipWithIndex.filter(_._2 % 2 == 1).flatMap:
    case ((size, position), _) if size > 0 => Some(Free(position, size))
    case _ => None
  val blocks = input.map(_.asDigit).zip(positions).zipWithIndex.filter(_._2 % 2 == 0).flatMap:
    case ((size, position), index) => (position until (position + size)).map(Block(index / 2, _))
  val files = input.map(_.asDigit).zip(positions).zipWithIndex.filter(_._2 % 2 == 0).flatMap:
    case ((size, position), index) if size > 0 => Some(File(index / 2, position, size))
    case _ => None
  new FileSystem(free.toList, blocks.toSet, files.toSet)

object Puzzle extends runner.Day[FileSystem, Long, Long]:
  def part1(fileSystem: FileSystem): Long =
    fileSystem.compact.checksum
    
  def part2(fileSystem: FileSystem): Long =
    fileSystem.defrag.checksum
