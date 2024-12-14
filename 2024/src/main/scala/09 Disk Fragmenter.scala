package day9
import algorithms.PriorityQueue
import parse.{*, given}

case class File(id: Int, position: Int, size: Int)
case class Block(id: Int, position: Int)
case class Free(position: Int, size: Int)

class FileSystem(free: List[Free], blocks: Set[Block], val files: Set[File]):
  def reverseFiles: Iterator[File] =
    files.toList.sortBy(-_.position).iterator

  def reverseBlocks: Iterator[Block] =
    blocks.toList.sortBy(-_.position).iterator

  def moveToFree(file: File): FileSystem =
    free.find(free => free.position < file.position && free.size >= file.size) match
      case Some(Free(position, size)) =>
        val newFree = if size > file.size then
          free.takeWhile(_ != Free(position, size)) ++ (Free(position + file.size, size - file.size) :: free.dropWhile(_ != Free(position, size)).drop(1))
        else
          free.filter(_ != Free(position, size))
        new FileSystem(newFree, blocks, files - file + File(file.id, position, file.size))
      case None => this

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

  def blocksFromFiles: FileSystem =
    val blocks = files.flatMap(file => (file.position until (file.position + file.size)).map(Block(file.id, _)))
    new FileSystem(free, blocks, files)

  def compact: FileSystem =
    reverseBlocks.foldLeft(this)(_.moveToFree(_))

  def defrag: FileSystem =
    reverseFiles.foldLeft(this)(_.moveToFree(_)).blocksFromFiles

given Read[FileSystem] = summon[Read[String]].map: input =>
  val positions = input.map(_.asDigit).scanLeft(0)(_ + _)
  val free = input.map(_.asDigit).zip(positions).zipWithIndex.filter(_._2 % 2 == 1).flatMap:
    case ((size, position), _) if size > 0 => Some(Free(position, size))
    case _ => None
  val files = input.map(_.asDigit).zip(positions).zipWithIndex.filter(_._2 % 2 == 0).flatMap:
    case ((size, position), index) if size > 0 => Some(File(index / 2, position, size))
    case _ => None
  FileSystem(free.toList, Set.empty, files.toSet).blocksFromFiles

object Puzzle extends runner.Day[FileSystem, Long, Long]:
  def part1(fileSystem: FileSystem): Long =
    fileSystem.compact.checksum
    
  def part2(fileSystem: FileSystem): Long =
    fileSystem.defrag.checksum
