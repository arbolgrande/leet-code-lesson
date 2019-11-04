object DivideTwoIntegers {
  def divide(dividend: Int, divisor: Int): Int = {
    val division = dividend.toLong / divisor
    division match {
      case v if v > Int.MaxValue  => Int.MaxValue
      case v if v < Int.MinValue  => Int.MinValue
      case v if v == Int.MaxValue => Int.MaxValue
      case v if v == Int.MinValue => Int.MinValue
      case _ => division.toInt
    }
  }
}