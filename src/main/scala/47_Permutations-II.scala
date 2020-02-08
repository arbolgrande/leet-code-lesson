object PermutationsII {
  def permuteUnique(nums: Array[Int]): List[List[Int]] = {
    nums.permutations.toList.map(_.toList)
  }
}