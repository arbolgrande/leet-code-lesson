object ValidParenthneses {
  def isValid(s: String): Boolean = {
    val map = Map(")" -> "(", "]" -> "[", "}" -> "{")
    var stack = scala.collection.mutable.Stack[String]()
    var bool = true
    def loop(str: String): Unit = {
      str.headOption.map(_.toString) match {
        case Some(c) if c == "(" || c == "[" || c == "{" =>
          stack = stack.push(c)
          loop(str.tail)
        case Some(c) =>
          if(stack.isEmpty || stack.pop() != map(c)) {
            bool = false
          }
          loop(str.tail)
        case None =>
          Unit
      }
    }
    loop(s)
    if(stack.nonEmpty) bool = false
    bool
  }
}