package puzzleparse

case class Pos(row: Int, col: Int) derives Read, CanEqual
opaque type Grid[+A] <: Map[Pos, A] = Map[Pos, A]
opaque type Digit <: Int = Int
