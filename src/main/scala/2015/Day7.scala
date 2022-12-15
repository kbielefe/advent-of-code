package advent2015
import scala.util.Try

object Day7:
  def part1(input: List[String]): Int =
    evaluate(circuits(input), "a", Map.empty[String, Int])._1

  def part2(input: List[String]): Int =
    evaluate(circuits(input), "a", Map("b" -> 956))._1

  private def circuits(input: List[String]): Map[String, String] =
    input.map(_.split(" -> ")).map{case Array(circuit, output) => output -> circuit}.toMap

  private def evaluate(circuits: Map[String, String], wire: String, wires: Map[String, Int]): (Int, Map[String, Int]) =
    def binary(x: String, y: String, op: (Int, Int) => Int): (Int, Map[String, Int]) =
      val (xResult, xWires) = evaluate(circuits, x, wires)
      val (yResult, yWires) = evaluate(circuits, y, xWires)
      val result = op(xResult, yResult) & 0x0ffff
      (result, yWires + (wire -> result))

    def unary(x: String, op: Int => Int): (Int, Map[String, Int]) =
      val (xResult, xWires) = evaluate(circuits, x, wires)
      val result = op(xResult) & 0x0ffff
      (result, xWires + (wire -> result))

    wires.get(wire).map(_ -> wires).getOrElse{
      Try(wire.toInt).map(_ -> wires).getOrElse{
        circuits(wire) match
          case s"$x AND $y"    => binary(x, y, _ & _)
          case s"$x OR $y"     => binary(x, y, _ | _)
          case s"$x LSHIFT $y" => binary(x, y, _ << _)
          case s"$x RSHIFT $y" => binary(x, y, _ >> _)
          case s"NOT $x"       => unary(x, ~ _)
          case x               => unary(x, identity)
      }
    }
