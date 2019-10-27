import scala.collection.mutable.HashMap
object ZigZagConversion {
  def convert(s: String, numRows: Int): String = {
    var map = HashMap[Int, Array[Char]]()
    0 until numRows foreach(n => map.update(n, Array.empty[Char]))
    val top = if(numRows != 1)(numRows * 2) - 2 else 1
    val bottom = top / 2
    s.zipWithIndex.foreach { c =>
      val loc = c._2 % top match {
        case v if v > bottom => top - v
        case v if v <= bottom => v
      }
      map.update(loc, map(loc).:+(c._1))
    }
    map.toSeq.sortWith((v1, v2) => v1._1 < v2._1).flatMap(_._2).mkString
  }
  def main(args: Array[String]): Unit = {
    println(convert("PAYPALISHIRING", 3))
    println(convert("PAYPALISHIRING",4))
    println(convert("A", 1))
  }
}