object GroupAnagrams {
  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    var map = scala.collection.mutable.Map[String, List[String]]()
    def checkAnagram(str: Array[String]): Unit = {
      val string = str.sorted.mkString("")
      val exists = map.get(string).nonEmpty
      if(exists) {
        val head = map(string)
        map = map updated (string, head.+:(str.mkString("")))
      } else {
        map = map updated (string, List(str.mkString("")))
      }
    }
    strs foreach { s =>
      checkAnagram(s.split(""))
    }
    map.values.toList
  }
}