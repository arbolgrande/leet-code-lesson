object IntegerToRoman {
  def intToRoman(num: Int): String = {
    val map = Map(
      0 -> "", 1 -> "I", 2 -> "II", 3 -> "III", 4 -> "IV", 5 -> "V", 6 -> "VI", 7 -> "VII", 8 -> "VIII", 9 -> "IX",
      10 -> "X", 20 -> "XX", 30 -> "XXX", 40 -> "XL", 50 -> "L", 60 -> "LX", 70 -> "LXX", 80 -> "LXXX", 90 -> "XC",
      100 -> "C", 200 -> "CC", 300 -> "CCC", 400-> "CD", 500 -> "D", 600 -> "DC", 700 -> "DCC", 800 -> "DCCC", 900 -> "CM",
      1000 -> "M", 2000 -> "MM", 3000 -> "MMM"
    )
    var resArray = Array.empty[String]
    num.toString.reverse.zipWithIndex.foreach { n =>
      n._2 match {
        case 0 =>
          val number = n._1.asDigit
          if (number != 0) resArray = resArray.:+(map(number))
        case 1 =>
          val number = s"${n._1}0".toInt
          if (number != 0) resArray = resArray.:+(map(number))

        case 2 =>
          val number = s"${n._1}00".toInt
          if (number != 0) resArray = resArray.:+(map(number))
        case 3 =>
          val number = s"${n._1}000".toInt
          if (number != 0) resArray = resArray
      }
    }
    resArray.mkString
  }
}