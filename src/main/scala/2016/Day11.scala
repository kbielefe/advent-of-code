package advent2016
import algorithms.AStar

object Day11:
  val initialPosition = Position(
    Vector(
      Set("SG", "SM", "PG", "PM"),
      Set("TG", "RG", "RM", "CG", "CM"),
      Set("TM"),
      Set.empty
    ), 0)

  val part2initialPosition = Position(
    Vector(
      Set("SG", "SM", "PG", "PM", "EG", "EM", "DG", "DM"),
      Set("TG", "RG", "RM", "CG", "CM"),
      Set("TM"),
      Set.empty
    ), 0)

  val goal = Position(Vector(Set.empty, Set.empty, Set.empty, Set("SG", "SM", "PG", "PM", "TG", "TM", "RG", "RM", "CG", "CM")), 3)
  val part2Goal = Position(Vector(Set.empty, Set.empty, Set.empty, Set("SG", "SM", "PG", "PM", "TG", "TM", "RG", "RM", "CG", "CM", "EG", "EM", "DG", "DM")), 3)

  case class Position(floors: Vector[Set[String]], floor: Int):
    def move(item: String): Set[Position] =
      val up   = if floor == 3 then Set.empty else Set(Position(floors.updated(floor, floors(floor) - item).updated(floor + 1, floors(floor + 1) + item), floor + 1))
      val down = if floor == 0 then Set.empty else Set(Position(floors.updated(floor, floors(floor) - item).updated(floor - 1, floors(floor - 1) + item), floor - 1))
      up ++ down

    def move(items: Set[String]): Set[Position] =
      val up   = if floor == 3 then Set.empty else Set(Position(floors.updated(floor, floors(floor) -- items).updated(floor + 1, floors(floor + 1) ++ items), floor + 1))
      val down = if floor == 0 then Set.empty else Set(Position(floors.updated(floor, floors(floor) -- items).updated(floor - 1, floors(floor - 1) ++ items), floor - 1))
      up ++ down

  given CanEqual[Position, Position] = CanEqual.derived

  private def heuristic(start: Position, end: Position): Int =
    (start.floors(0).size * 3 + start.floors(1).size * 2 + start.floors(2).size) / 2

  private def getNeighbors(start: Position): Set[Position] =
    val moveOne = start.floors(start.floor).flatMap(start.move)
    val moveTwo = start.floors(start.floor).toSeq.combinations(2).flatMap(items => start.move(items.toSet))
    (moveOne ++ moveTwo).filter(safePosition)

  private def edgeWeight(start: Position, end: Position): Int = 1

  private def safePosition(pos: Position): Boolean =
    pos.floors.forall(safeFloor)

  private def safeFloor(floor: Set[String]): Boolean =
    !floor.exists(_.endsWith("G")) || floor.filter(_.endsWith("M")).forall(chip => floor.contains(s"${chip.head}G"))

  val astar = new AStar(heuristic, edgeWeight, 0, getNeighbors)

  def part1(input: List[String]): Int =
    astar.getMinCost(initialPosition, goal).get

  def part2(input: List[String]): Int =
    astar.getMinCost(part2initialPosition, part2Goal).get
