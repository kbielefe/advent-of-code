package puzzleparse

opaque type Nat <: Int = Int
opaque type Binary <: Int = Int
opaque type Letter <: Char = Char
opaque type Letters <: String = String
type Pos = (Int, Int) // Row, Col
opaque type Grid <: Map[Pos, Char] = Map[Pos, Char]
opaque type MultiLine[A] <: List[A] = List[A]
opaque type DMap[D <: String & Singleton, K, V] <: Map[K, V] = Map[K, V]
opaque type DList[D <: String & Singleton, A] <: List[A] = List[A]
