package puzzleparse

trait Show[A]:
  def show(output: A): String
  def list(output: A): List[String] = List(show(output))

given Show[Int] with
  def show(output: Int): String =
    output.toString

given Show[String] with
  def show(output: String): String =
    output

given Show[EmptyTuple] with
  def show(output: EmptyTuple): String =
    "()"

  override def list(output: EmptyTuple): List[String] =
    Nil

given [H : Show, T <: Tuple : Show]: Show[H *: T] with
  def show(output: H *: T): String =
    list(output).mkString("(", ", ", ")")

  override def list(output: H *: T): List[String] =
    summon[Show[H]].show(output.head) :: summon[Show[T]].list(output.tail)

given Show[Letter] with
  def show(output: Letter): String =
    output.toString

given Show[Letters] with
  def show(output: Letters): String =
    output
