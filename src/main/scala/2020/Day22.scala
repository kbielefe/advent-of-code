package advent2020
import puzzleparse.MultiLine

object Day22:
  def part1(input: MultiLine[List[String]]): Int =
    val deck1 = input(0).drop(1).map(_.toInt).toVector
    val deck2 = input(1).drop(1).map(_.toInt).toVector
    score(playGame(deck1, deck2))

  def part2(input: MultiLine[List[String]]): Int =
    val deck1 = input(0).drop(1).map(_.toInt).toVector
    val deck2 = input(1).drop(1).map(_.toInt).toVector
    score(playRecursive(deck1, deck2, Set.empty)._2)

  @scala.annotation.tailrec
  private def playGame(deck1: Vector[Int], deck2: Vector[Int]): Vector[Int] =
    if deck1.isEmpty then
      deck2
    else if deck2.isEmpty then
      deck1
    else if deck1.head > deck2.head then
      playGame(deck1.tail ++ Vector(deck1.head, deck2.head), deck2.tail)
    else
      playGame(deck1.tail, deck2.tail ++ Vector(deck2.head, deck1.head))

  private def playRecursive(deck1: Vector[Int], deck2: Vector[Int], prev: Set[(Vector[Int], Vector[Int])]): (Int, Vector[Int]) =
    if prev.contains(deck1 -> deck2) then
      (1, deck1)
    else if deck1.isEmpty then
      (2, deck2)
    else if deck2.isEmpty then
      (1, deck1)
    else if deck1.head <= (deck1.size - 1) && deck2.head <= (deck2.size - 1) then
      val (winner, _) = playRecursive(deck1.drop(1).take(deck1.head), deck2.drop(1).take(deck2.head), Set.empty)
      if winner == 1 then
        playRecursive(deck1.tail ++ Vector(deck1.head, deck2.head), deck2.tail, prev + (deck1 -> deck2))
      else
        playRecursive(deck1.tail, deck2.tail ++ Vector(deck2.head, deck1.head), prev + (deck1 -> deck2))
    else if deck1.head > deck2.head then
      playRecursive(deck1.tail ++ Vector(deck1.head, deck2.head), deck2.tail, prev + (deck1 -> deck2))
    else
      playRecursive(deck1.tail, deck2.tail ++ Vector(deck2.head, deck1.head), prev + (deck1 -> deck2))

  private def score(deck: Vector[Int]): Int =
    deck.reverse.zipWithIndex.map((card, index) => card * (index + 1)).sum
