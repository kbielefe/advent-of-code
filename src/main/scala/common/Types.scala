package puzzleparse

opaque type Nat <: Int = Int
opaque type Binary <: Int = Int
opaque type Letter <: Char = Char
opaque type Letters <: String = String
case class Pos(row: Int, col: Int)
opaque type Grid[A] <: Map[Pos, A] = Map[Pos, A]
opaque type MultiLine[A] <: List[A] = List[A]
opaque type DMap[D <: String & Singleton, K, V] <: Map[K, V] = Map[K, V]
opaque type DList[D <: String & Singleton, A] <: List[A] = List[A]
class Header[A, B](val header: A, val body: B)
