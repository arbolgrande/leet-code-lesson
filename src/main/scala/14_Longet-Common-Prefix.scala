object LongestCommonPrefix {
  def longestCommonPrefix(strs: Array[String]): String = {
    val sorted = strs.map(v => (v, v.length)).sortWith((v1, v2) => v1._2 < v2._2)
    def loop(s1: Seq[(Char, Char)], res: String): String = {
      if(s1.nonEmpty) {
        val head = s1.head
        (head._1 == head._2) match {
          case true =>
            loop(s1.tail, res + head._1.toString)
          case false =>
            res
        }
      } else {
        res
      }
    }
    var res  = sorted.headOption.map(_._1).getOrElse("")
    (0 until strs.length).foreach { s =>
      val tmp =  loop(res.zip(sorted(s)._1), "")
      res = tmp
    }
    res
  }
}