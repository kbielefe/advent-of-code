package advent2022
import io.circe.Json
import io.circe.parser.parse
import puzzleparse.MultiLine

object Day13:
  def part1(input: MultiLine[List[Json]]): Int =
    input.zipWithIndex.filter{case (List(left, right), index) => inOrder(left, right).get}.map(_._2 + 1).sum

  def part2(input: MultiLine[List[Json]]): Int =
    val dividerPackets = List("[[2]]", "[[6]]").map(parse).flatMap(_.toOption)
    val sorted = (dividerPackets ++ input.flatten).sortWith((left, right) => inOrder(left, right).get)
    sorted.zipWithIndex.filter(x => dividerPackets.contains(x._1)).map(_._2 + 1).product

  private def inOrder(left: Json, right: Json): Option[Boolean] = (left.isNumber, right.isNumber) match
    case (true, true)   => numbersInOrder(left, right)
    case (false, false) => arraysInOrder(left, right)
    case (false, true)  => arraysInOrder(left, Json.fromValues(List(right)))
    case (true, false)  => arraysInOrder(Json.fromValues(List(left)), right)

  private def numbersInOrder(left: Json, right: Json): Option[Boolean] =
    val leftInt = left.asNumber.flatMap(_.toInt).get
    val rightInt = right.asNumber.flatMap(_.toInt).get
    if leftInt < rightInt then Some(true) else if rightInt < leftInt then Some(false) else None

  private def arraysInOrder(left: Json, right: Json): Option[Boolean] =
    val leftArray = left.asArray.get
    val rightArray = right.asArray.get
    (leftArray.headOption, rightArray.headOption) match
      case (Some(left), Some(right)) => inOrder(left, right).orElse(inOrder(Json.fromValues(leftArray.tail), Json.fromValues(rightArray.tail)))
      case (Some(left), None)        => Some(false)
      case (None, Some(left))        => Some(true)
      case (None, None)              => None
