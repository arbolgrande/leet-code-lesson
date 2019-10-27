object PalindromeNumber {
  def isPalindrome(x: Int): Boolean = {
    val str = x.toString
    val (left, right) = str.length match {
      case v if v % 2 == 0 => {
        val center = str.length / 2
        (str.take(center), str.takeRight(center))
      }
      case _ => {
        val center = (str.length.toDouble / 2).floor.toInt
        (str.take(center), str.takeRight(center))
      }
    }
    left == right.reverse
  }
  def main(args: Array[String]): Unit = {
    println(isPalindrome(121))
  }
}