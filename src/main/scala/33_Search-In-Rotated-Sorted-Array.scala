object SearchInRotatedSortedArray {
  def search(nums: Array[Int], target: Int): Int = {
    val map = nums.zipWithIndex.map((v) => (v._2, v._1)).toMap

    def loop(start: Int, end: Int): Int = {
      val startNum = map(start)
      val endNum = map(end)
      if (startNum == target || endNum == target) {
        if (startNum == target) start else end
      } else if (target < startNum && target > endNum) {
        -1
      } else {
        (start, end) match {
          case (x, y) if x == y => -1
          case (x, y) =>
            val index = ((y - x) / 2) + x
            val indexNum = map(index)
            (startNum, indexNum, endNum) match {
              case (_, b, _) if target == b => index
              case (a, b, _) if target > a && target < b => loop(start, index - 1)
              case (_, b, c) if target < c && target > b => loop(index + 1, end)
              case (a, b, _) if target < a =>
                if(a <= b) loop(index + 1, end) else loop(start, index - 1)
              case (a, b, c) if target > c =>
                if(a <= b) loop(index + 1, end) else loop(start, index - 1)
            }
        }
      }
    }
    if (nums.isEmpty) -1 else loop(0, nums.length - 1)
  }
}