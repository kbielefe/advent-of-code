package advent2021
import puzzleparse.{*, given}
import algorithms.AStar

object Day23:
  def part1(burrow: Burrow): Int =
    AStar(heuristic, edgeWeight, 0, _.neighbors).getMinCost(burrow, goal).get

  def part2(burrow: Burrow): Int =
    AStar(heuristic, edgeWeight, 0, _.neighbors).getMinCost(burrow, goal).get

  type Pod = Char

  case class Hallway(spaces: Map[Int, Pod] = Map.empty):
    override def toString: String =
      (0 to 10).map(col => spaces.get(col).getOrElse('.')).mkString

    def availableMovesIn(fromRoom: Room): Set[Hallway] =
      val disallowed = Set(2, 4, 6, 8)
      val spacesToLeft = ((fromRoom.col - 1) to 0 by -1).filterNot(disallowed.contains(_)).takeWhile(!spaces.contains(_)).toSet
      val spacesToRight = ((fromRoom.col + 1) to 10).filterNot(disallowed.contains(_)).takeWhile(!spaces.contains(_)).toSet
      (spacesToLeft ++ spacesToRight).map(space => Hallway(spaces + (space -> fromRoom.availablePod)))

    def availableMovesOut(toRoom: Room): Set[Hallway] =
      val sameSpaces = spaces.filter(_._2 == toRoom.owner).toSet
      val clearSpaces = sameSpaces.filter((space, pod) => (Math.min(space + 1, toRoom.col) to Math.max(space - 1, toRoom.col)).forall(space => !spaces.contains(space)))
      clearSpaces.map((space, pod) => Hallway(spaces - space))

    def heuristic: Int =
      spaces.map((space, pod) => ((space - pod.col).abs + 1) * pod.energy).sum

    def pods: Set[(Pos, Pod)] =
      spaces.map((col, pod) => (Pos(0, col) -> pod)).toSet

  case class Room(col: Int, owner: Pod, podList: Vector[Option[Pod]]):
    def apply(index: Int): Char =
      podList(index).getOrElse('.')

    def availableMovesOut: Set[Room] =
      val topIndex = podList.indexWhere(_.isDefined)
      if topIndex == -1 then
        Set.empty
      else
        val newPodList = podList.updated(topIndex, None)
        Set(Room(col, owner, newPodList))

    def availableMovesIn: Set[Room] =
      val lowestEmpty = podList.lastIndexWhere(!_.isDefined)
      val eligible = podList.flatten.forall(_ == owner)
      if eligible && lowestEmpty != -1 then
        val newPodList = podList.updated(lowestEmpty, Some(owner))
        Set(Room(col, owner, newPodList))
      else
        Set.empty

    // Throws if room is empty, should only be called in contexts where that
    // has already been checked with availableMovesOut.
    def availablePod: Pod = podList.flatten.head

    def pods: Set[(Pos, Pod)] =
      podList.zipWithIndex.map((podOpt, index) => podOpt.map(pod => (Pos(index + 1, col) -> pod))).flatten.toSet

    def heuristic: Int =
      podList.zipWithIndex.map((podOpt, index) => podOpt.filter(_ != owner).map(pod => (2 + index + (pod.col - col).abs) * pod.energy)).flatten.sum

  case class Burrow(hallway: Hallway, rooms: Map[Pod, Room]) derives CanEqual:
    override def toString: String =
      "\n#############\n" +
      "#" + hallway.toString + "#\n" +
      s"###${rooms('A')(0)}#${rooms('B')(0)}#${rooms('C')(0)}#${rooms('D')(0)}###\n" +
      s"  #${rooms('A')(1)}#${rooms('B')(1)}#${rooms('C')(1)}#${rooms('D')(1)}#\n" +
      "  #########\n"

    def heuristic: Int =
      rooms.values.toList.map(_.heuristic).sum + hallway.heuristic

    def edgeWeight(end: Burrow): Int =
      val startRoomPods = rooms.flatMap(_._2.pods).toSet
      val endRoomPods = end.rooms.flatMap(_._2.pods).toSet
      if startRoomPods.size > endRoomPods.size then // Moved up to hallway
        val (fromPos, pod) = (startRoomPods -- endRoomPods).head
        val (toPos, _) = (end.hallway.pods -- hallway.pods).head
        ((fromPos.row - toPos.row).abs + (fromPos.col - toPos.col).abs) * pod.energy
      else // Moved down to a room
        val (fromPos, pod) = (endRoomPods -- startRoomPods).head
        val (toPos, _) = (hallway.pods -- end.hallway.pods).head
        ((fromPos.row - toPos.row).abs + (fromPos.col - toPos.col).abs) * pod.energy

    def neighbors: Set[Burrow] = upNeighbors ++ downNeighbors

    private def upNeighbors: Set[Burrow] =
      for
        podRooms <- rooms.toSet
        (pod, room) = podRooms
        moveOut <- room.availableMovesOut
        moveIn  <- hallway.availableMovesIn(room)
      yield Burrow(moveIn, rooms + (pod -> moveOut))

    private def downNeighbors: Set[Burrow] =
      for
        podRooms <- rooms.toSet
        (pod, room) = podRooms
        moveIn  <- room.availableMovesIn
        moveOut <- hallway.availableMovesOut(room)
      yield Burrow(moveOut, rooms + (pod -> moveIn))

  def edgeWeight(start: Burrow, end: Burrow): Int = start.edgeWeight(end)
  def heuristic(start: Burrow, end: Burrow): Int = start.heuristic
  val goal: Burrow = Burrow(Hallway(), "ABCD".zip(Seq(2, 4, 6, 8)).map{case (pod, col) => pod -> Room(col, pod, Vector(Some(pod), Some(pod)))}.toMap)

  given Read[Burrow] with
    def read(input: String): Burrow =
      val grid = summon[Read[Grid[Char]]].read(input)
      val rooms = Map(
        'A' -> Room(2, 'A', Vector(Some(grid(Pos(2, 3))), Some(grid(Pos(3, 3))))),
        'B' -> Room(4, 'B', Vector(Some(grid(Pos(2, 5))), Some(grid(Pos(3, 5))))),
        'C' -> Room(6, 'C', Vector(Some(grid(Pos(2, 7))), Some(grid(Pos(3, 7))))),
        'D' -> Room(8, 'D', Vector(Some(grid(Pos(2, 9))), Some(grid(Pos(3, 9)))))
      )
      Burrow(Hallway(), rooms)

  extension(p: Pod)
    def energy: Int = p match
      case 'A' => 1
      case 'B' => 10
      case 'C' => 100
      case 'D' => 1000

    def col: Int = p match
      case 'A' => 2
      case 'B' => 4
      case 'C' => 6
      case 'D' => 8
