object RemoveNthNodeFromEndOfList {
  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    var stack = scala.collection.mutable.Stack[Int]()
    def pushStack(count: Int, node: ListNode): Int = {
      stack.push(node.x)
      if(node.next == null) count else pushStack(count + 1, node.next)
    }
    val count = pushStack(1,head )
    def createNewNode(node: ListNode, c: Int):ListNode = {
      if(c > count) {
        node
      } else {
        if (c == n) {
          stack.pop()
          createNewNode(node, c + 1)
        } else {
          val newNode = new ListNode(stack.pop())
          newNode.next = node
          createNewNode(newNode, c + 1)
        }
      }
    }
    if(n != 1) {
      createNewNode(new ListNode(stack.pop()), 2)
    } else if(n == 1 && stack.length <= 2) {
      stack.pop()
      createNewNode(null, 2)
    } else {
      stack.pop()
      createNewNode(new ListNode(stack.pop()), 3)
    }
  }
}