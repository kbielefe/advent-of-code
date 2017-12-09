import scala.io.Source

val input = Source.fromFile("input9.txt")

val (_, _, score, _, nonCanceled) = input.foldLeft((false, false, 0, 0, 0)){case ((cancel, garbage, score, nesting, nonCanceled), char) =>
  if (cancel) {
    (false, garbage, score, nesting, nonCanceled)
  } else if (garbage) {
    char match {
      case '>' => (false, false, score, nesting, nonCanceled)
      case '!' => (true,  true,  score, nesting, nonCanceled)
      case _   => (false, true,  score, nesting, nonCanceled + 1)
    }
  } else {
    char match {
      case '{' => (false, false, score + nesting + 1, nesting + 1, nonCanceled)
      case '}' => (false, false, score,               nesting - 1, nonCanceled)
      case '<' => (false, true,  score,               nesting,     nonCanceled)
      case _   => (false, false, score,               nesting,     nonCanceled)
    }
  }
}

val answer1 = score
println(answer1)

val answer2 = nonCanceled
println(answer2)
