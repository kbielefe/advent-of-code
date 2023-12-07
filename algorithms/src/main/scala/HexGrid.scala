package algorithms

// Implements pointy-top coordinates from https://www.redblobgames.com/grids/hexagons/
object HexGrid:
  opaque type Pos = (Int, Int, Int)
  given CanEqual[Pos, Pos] = CanEqual.derived

  object Pos:
    def origin: Pos = (0, 0, 0)

  extension (p: Pos)
    def neighbors: Set[Pos] = Set(p.east, p.west, p.northeast, p.northwest, p.southeast, p.southwest)
    def q: Int = p._1
    def r: Int = p._2
    def s: Int = p._3
    def east: Pos = (p.q + 1, p.r, p.s - 1)
    def west: Pos = (p.q - 1, p.r, p.s + 1)
    def northeast: Pos = (p.q + 1, p.r - 1, p.s)
    def southwest: Pos = (p.q - 1, p.r + 1, p.s)
    def northwest: Pos = (p.q, p.r - 1, p.s + 1)
    def southeast: Pos = (p.q, p.r + 1, p.s - 1)
