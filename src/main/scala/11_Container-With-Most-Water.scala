object ContainerWithMostWater {
  def maxArea(height: Array[Int]): Int = {
    val map = height.zipWithIndex.map(v => (v._2, v._1)).toMap
    var maxSize = Math.min(map(0), map(height.length - 1)) * height.length -  1
    def loop(startIndex: Int, endIndex: Int): Int = {
      if(startIndex < endIndex) {
        val area = (endIndex - startIndex) * Math.min(map(startIndex), map(endIndex))
        maxSize = Math.max(area, maxSize)
        if(map(startIndex) > map(endIndex)) {
          loop(startIndex, endIndex - 1)
        } else {
          loop(startIndex + 1, endIndex)
        }
      } else maxSize
    }
    loop(0, height.length - 1)
  }
}