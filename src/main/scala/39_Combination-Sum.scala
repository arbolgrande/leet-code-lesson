object CombinationSum {
  def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
    val listWithPossibleTimes = candidates map { n =>
      val times = target / n
      (n, times)
    } toSeq
    def createList(num: Int, n: Int, list: List[Int]): List[Int] = {
      if(n > 0) {
        val newList = list.+:(num)
        createList(num, n - 1, newList)
      } else list
    }
    val hoge = listWithPossibleTimes.map { v =>
      (0 to v._2).map(n => createList(v._1, n, List[Int]())).toSeq
    }.toList
    def loop(array: List[Seq[List[Int]]], res: List[List[Int]]): List[List[Int]] = {
      array.headOption match {
        case Some(v) =>
          val newList = res.flatMap { x =>
            v.map { n =>
              x.++(n)
            }
          } filter(_.sum <= target)
          loop(array.tail, newList)
        case None    =>
          res
      }
    }
    loop(hoge.tail, hoge.head.toList).filter(_.sum == target).distinct
  }
}