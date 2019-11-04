object LetterCombinationsOfAPhoneNumber {
  def letterCombinations(digits: String): List[String] = {
    val map = Map(
      ("2", List("a","b","c")), ("3", List("d","e","f")), ("4", List("g","h","i")), ("5", List("j","k","l")),
      ("6", List("m","n","o")), ("7", List("p","q","r","s")), ("8", List("t","u","v")), ("9", List("w","x","y","z"))
    )
    def loop(list: List[String], string: String): List[String] = {
      string.headOption match {
        case Some(c) =>
          val tmp = map(c.toString)
          val res = tmp.flatMap { t =>
            list.map(_ + t)
          }
          loop(res, string.tail)
        case None    =>
          list
      }
    }
    if(digits.nonEmpty) loop(List(""), digits) else List.empty[String]
  }
}