object ThreeSum {
  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val length = nums.length
    var res = Set.empty[List[Int]]
    val map = nums.sorted.zipWithIndex.map(v => (v._2, v._1)).toMap
    def addIfZero(index: Int, left: Int, right:Int) = {
      if(map(index) + map(left) + map(right) == 0)
        res = res.+(List(map(index), map(left), map(right)))
    }
    def loop(index: Int, left: Int, right: Int): Unit = {
      if(index <= length - 2) {
        (left, right) match {
          case (l, r) if l == 0 && r == length - 1 =>
            addIfZero(index, l, r)
            loop(index + 1, index, index + 2)
          case (l,r) if l == 0 =>
            addIfZero(index, l, r)
            val sum = map(index) + map(left) + map(right)
            if(sum == 0) loop(index + 1, index, index + 2) else loop(index, l, right + 1)
          case (l, r) if right == length - 1 =>
            addIfZero(index, l,r)
            val sum = map(index) + map(left) + map(right)
            if(sum == 0) loop(index + 1, index, index + 2) else loop(index, l -1, r)
          case (l,r) =>
            val sum = map(left) + map(right) + map(index)
            if(sum == 0) {
              res = res.+(List(map(index), map(left), map(right)))
              loop(index, left -1, right + 1)
            } else if(sum > 0) {
              loop(index, left -1, right)
            } else {
              loop(index, left, right + 1)
            }
        }
      } else {
        Unit
      }
    }
    if(length > 2) loop(1, 0, 2)
    res.toList
  }
  def main(args: Array[String]): Unit = {
    println(threeSum(Array(-1, 2, 1, -4)))
  }
}