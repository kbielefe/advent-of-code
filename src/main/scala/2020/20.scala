package advent2020

import common._

object Day20 extends MultilineSyncStringsDay[Long, Long](2020, 20) {
  override def part1(input: Seq[Seq[String]]): Long = {
    val tiles = input.map(parseTile)
    val borders = for {
      tile   <- tiles
      border <- tile.borders
    } yield (border -> tile)
    val grouped = borders.groupBy(_._1).filter(_._2.size == 2)
    val usedBorders = grouped.keySet
    tiles.filter(tile => (tile.borders & usedBorders).size == 4).map(_.id).product
  }

  override def part2(input: Seq[Seq[String]]): Long = ???

  type Point = (Int, Int)
  private case class Tile(id: Long, points: Set[Point]) {
    def borders: Set[List[Int]] = {
      val top = points.filter(_._2 == 0).map(_._1).toList.sorted
      val bottom = points.filter(_._2 == 9).map(_._1).toList.sorted
      val left = points.filter(_._1 == 0).map(_._2).toList.sorted
      val right = points.filter(_._1 == 9).map(_._2).toList.sorted
      Set(top, bottom, left, right, reverse(top), reverse(bottom), reverse(left), reverse(right))
    }
  }

  private def reverse(border: List[Int]): List[Int] =
    border.map(9 - _).sorted

  private def parseTile(input: Seq[String]): Tile = {
    val s"Tile $id:" = input(0)
    val points = input.drop(1).zipWithIndex.flatMap{case (line, y) =>
      line.zipWithIndex.filter(_._1 == '#').map(_._2).map(x => (x -> y))
    }.toSet
    Tile(id.toInt, points)
  }
}
