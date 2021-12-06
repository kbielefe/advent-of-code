package puzzleparse

case class Pos(row: Int, col: Int)
opaque type Grid[A] <: Map[Pos, A] = Map[Pos, A]
