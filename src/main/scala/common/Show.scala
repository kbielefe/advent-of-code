trait Show[A]:
  def show(output: A): String

given Show[Int] with
  def show(output: Int): String =
    output.toString
