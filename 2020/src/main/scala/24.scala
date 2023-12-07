package day24

import algorithms.HexGrid.Pos
import parse.{*, given}

type I = List[String] - "\n"

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    getBlackTiles(input).size

  def part2(input: I): Int =
    val tiles = getBlackTiles(input)
    val finalTiles = Iterator.iterate(tiles)(life).drop(100).next
    finalTiles.size

  def life(blackTiles: Set[Pos]): Set[Pos] =
    val neighbors = blackTiles.flatMap(_.neighbors)
    (neighbors ++ blackTiles).foldLeft(blackTiles){(tiles, pos) =>
      val blackNeighborCount = (pos.neighbors & blackTiles).size
      if blackTiles.contains(pos) && (blackNeighborCount == 0 || blackNeighborCount > 2) then
        tiles - pos
      else if !blackTiles.contains(pos) && blackNeighborCount == 2 then
        tiles + pos
      else
        tiles
    }

  def getBlackTiles(input: List[String]): Set[Pos] =
    val flips = input.map(inputToPos(_, Pos.origin))
    flips.foldLeft(Set.empty[Pos]){(tiles, pos) =>
      if tiles.contains(pos) then tiles - pos else tiles + pos
    }

  def inputToPos(input: String, pos: Pos): Pos =
    if input.isEmpty then
      pos
    else input.head match
      case 'e' => inputToPos(input.drop(1), pos.east)
      case 'w' => inputToPos(input.drop(1), pos.west)
      case 's' => input.drop(1).head match
        case 'e' => inputToPos(input.drop(2), pos.southeast)
        case 'w' => inputToPos(input.drop(2), pos.southwest)
      case 'n' => input.drop(1).head match
        case 'e' => inputToPos(input.drop(2), pos.northeast)
        case 'w' => inputToPos(input.drop(2), pos.northwest)
