object RomanToInteger {
  def romanToInt(s: String): Int = {
    val map: Map[String, Int] = Map(
      "I" -> 1, "V" -> 5, "X" -> 10, "L" -> 50,
      "C" -> 100, "D" -> 500, "M" -> 1000
    )
    var sum  = 0
    var prev = ""
    def loop(str: String):Int = {
      str.headOption match {
        case Some(c) =>
          c.toString match {
            case "V" => if(prev == "I") sum = sum + 3 else sum = sum + map("V")
            case "X" => if(prev == "I") sum = sum + 8 else sum = sum + map("X")
            case "L" => if(prev == "X") sum = sum + 30 else sum = sum + map("L")
            case "C" => if(prev == "X") sum = sum + 80 else sum = sum + map("C")
            case "D" => if(prev == "C") sum = sum + 300 else sum = sum + map("D")
            case "M" => if(prev == "C") sum = sum + 800 else sum = sum + map("M")
            case _   => sum = sum + map(c.toString)
          }
          prev = c.toString
          loop(str.tail)
        case None    =>
          sum
      }
    }
    loop(s)
  }
}