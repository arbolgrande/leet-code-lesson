import scala.collection.mutable.HashMap

object Solution03 {
  def lengthOfLongestSubstring(s: String): Int = {
    var map = HashMap[Char, Int]()
    var resArray = Array.empty[Int]
    var last  = 0
    s.zipWithIndex.foreach { c =>
      map.get(c._1) match {
        case Some(v) => {
          if(last <= v) last = v + 1
        }
        case None => { }
      }
      resArray = resArray.:+(c._2 + 1 - last)
      map.update(c._1, c._2)
    }
    resArray.reduceOption(_ max _).getOrElse(0)
  }
}