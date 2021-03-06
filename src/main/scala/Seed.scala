object Main {
  def rotate(matrix: Array[Array[Int]]): Unit = {
    var map     = scala.collection.mutable.Map[Int, Int]()
    val n       = matrix.head.length
    matrix.zipWithIndex foreach { m =>
      m._1.zipWithIndex foreach { v =>
        map update((m._2 * n) + v._2 + 1, v._1)
      }
    }
    def replaceNumber(row: Int, col: Int,num: Int): Unit = {
      matrix(row)(col) = num
    }
    (1 to n).foreach { v =>
      (0 until n).foreach { m =>
        replaceNumber(v-1, m, map((n * n) - (n - v) - (n * m)))
      }
    }
  }
  def main(args: Array[String]): Unit = {
    println(rotate(Array(
      Array(1,2,3),
      Array(4,5,6),
      Array(7,8,9)
    )))
  }
}