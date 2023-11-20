package parse

trait Show[A]:
  def show(output: A): String

given Show[Int] with
  def show(output: Int): String =
    output.toString

given Show[String] with
  def show(output: String): String =
    output

given Show[Long] with
  def show(output: Long): String =
    output.toString

