package common

object Numeric {
  implicit class EnhancedInt(n: Int) {
    def digits: Seq[Int] = n.toString.map{_.asDigit}
  }

  implicit class EnhancedLong(n: Long) {
    def digits: Seq[Int] = n.toString.map{_.asDigit}
  }
}
