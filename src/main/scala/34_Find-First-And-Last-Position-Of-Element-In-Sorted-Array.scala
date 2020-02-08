object FindFirstAndLastPositionOfElementInSortedArray {
  def searchRange(nums: Array[Int], target: Int): Array[Int] = {
    val map = nums.zipWithIndex.map(v => (v._2, v._1)).toMap
    def search(start: Int, end: Int): Int = {
      if(start > end) { - 1 } else {
        val startNum = map(start)
        val endNum = map(end)
        if (startNum == target || endNum == target) {
          if (map(start) == target) start else end
        } else if (target < startNum && target > endNum) {
          -1
        } else {
          val index = ((end - start) / 2) + start
          val indexNum = map(index)
          if (indexNum == target) {
            index
          } else if (indexNum < target) {
            search(index + 1, end)
          } else {
            search(start, index - 1)
          }
        }
      }
    }
    val res = if(nums.nonEmpty) search(0, nums.length - 1) else - 1
    def searchWithNum(start: Int, end: Int): Array[Int] = {
      if(map.get(start - 1).nonEmpty && map(start -1) == map(res)) {
        searchWithNum(start - 1, end)
      } else if(map.get(end + 1).nonEmpty && map(end + 1) == map(res)) {
        searchWithNum(start, end + 1)
      } else {
        Array(start, end)
      }
    }
    if(res != -1) {
      searchWithNum(res, res)
    } else {
      Array(-1, -1)
    }
  }

}