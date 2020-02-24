object GenerateParethesis {
  def generateParenthesis(n: Int): List[String] = {
    var queue = scala.collection.mutable.Queue[String]()
    def loop(left: Int, right: Int, str: String): Unit = {
      if(str.length == n * 2) {
        queue.enqueue(str)
      } else {
        if(left < n) {
          loop(left + 1, right, str + "(")
        } else if(right < left) {
          loop(left, right + 1, str + ")")
        }
      }
    }
    loop(0, 0, "")
    queue.toList
  }
}