package algorithms

import parse.{Read, Show}
import spire.math.Rational
import io.circe.Encoder

object Spire:
  given Show[Rational] with
    def show(output: Rational): String =
      output.toString

  given Read[Rational] with
    def read(input: String): Rational =
      Rational(input)

  given Encoder[Rational] =
    summon[Encoder[BigInt]].contramap(_.toBigInt)
