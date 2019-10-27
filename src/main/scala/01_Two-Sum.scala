package main

object Solution01 {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val map = nums.zipWithIndex.toMap
    var x, y = -1
    nums.zipWithIndex.foreach { v =>
      val res = map.get(target - v._1) match {
        case Some(r) if r != v._2 => r
        case Some(_)              => -1
        case None                 => -1
      }
      if(res != -1) {
        x = res
        y = v._2
      }
    }
    Array(x, y)
  }
}