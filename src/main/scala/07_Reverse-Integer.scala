object ReverseInteger {
  def reverse(x: Int): Int = {
    val num = if(x < 0) {
      s"-${x.toString.tail.reverse}"
    } else {
      x.toString.reverse
    }
    if(num.toLong < Int.MinValue || num.toLong > Int.MaxValue) 0 else num.toInt
  }
  def main(args: Array[String]): Unit = {
    println(reverse(1000000))
  }
}