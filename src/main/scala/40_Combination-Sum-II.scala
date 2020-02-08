object CombinationSumII {
  def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
    def loop(tmp: Array[Int], res: List[List[Int]]): List[List[Int]] = {
      tmp.headOption match {
        case Some(v) =>
          val newTmp = List(List.empty, List(v))
          val newList = res.flatMap { x =>
            newTmp.map { n =>
              n ++ x
            }
          } filter(_.sum <= target)
          loop(tmp.tail, newList)
        case None =>
          res
      }
    }
    val newCandidate = candidates.sorted
    loop(newCandidate.tail, List(List.empty[Int],List(newCandidate.head))).filter(_.sum == target).distinct
  }
}