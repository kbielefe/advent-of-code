package common

object Numeric {
  implicit class EnhancedNumeric(n: Int) {
    def digits: Seq[Int] = n.toString.map{_.asDigit}
  }
}
