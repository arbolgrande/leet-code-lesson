object MergeTwoSortedLists {
  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
    var stack = scala.collection.mutable.Stack[Int]()
    def pushToStack(node: ListNode): Unit = {
      node match {
        case v: ListNode =>
          stack = stack.push(node.x)
          if(node.next == null) Unit else pushToStack(node.next)
        case _ =>
          Unit
      }
    }
    pushToStack(l1)
    pushToStack(l2)
    val sorted = stack.sortWith((v1, v2) => v1 > v2)
    if(sorted.isEmpty) {
      null: ListNode
    } else {
      sorted.tail.foldLeft(new ListNode(sorted.head)){ (acc, v) =>
        var newNode = new ListNode(v)
        newNode.next = acc
        newNode
      }
    }
  }
}