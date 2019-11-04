object ThreeSumClosest {
  def threeSumClosest(nums: Array[Int], target: Int): Int = {
    val length = nums.length
    var res = nums.head + nums(1) + nums(2)
    val map = nums.sorted.zipWithIndex.map(v => (v._2, v._1)).toMap
    def replaceIfDiffSmall(sum: Int) = {
      val diff    = if(sum > target) sum - target else target - sum
      val resDiff = if(res > target) res - target else target - res
      if(diff < resDiff) res = sum
    }
    def loop(index: Int, left: Int, right: Int): Int = {
      if(index <= length - 2) {
        val sum = map(index) + map(left) + map(right)
        if(sum == target) {
          res = target
          res
        } else {
          (left, right) match {
            case (l,r) if l == 0 && r == length - 1 =>
              replaceIfDiffSmall(sum)
              loop(index + 1, index, index + 2)
            case (l,r) if l == 0 =>
              replaceIfDiffSmall(sum)
              if(sum > target) loop(index + 1, index, index + 2) else loop(index, left, right + 1)
            case (l,r) if r == length - 1 =>
              replaceIfDiffSmall(sum)
              if(sum < target) loop(index + 1, index, index + 2) else loop(index, left -1, right)
            case (l,r) =>
              replaceIfDiffSmall(sum)
              if(sum > target) loop(index, left - 1, right) else loop(index, left, right + 1)
          }
        }
      } else res
    }
    if(length > 2) loop(1, 0, 2)
    res
  }
}