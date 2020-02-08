object Permutations {
  def permute(nums: Array[Int]): List[List[Int]] = {
    nums.permutations.toList.map(_.toList)
  }
}