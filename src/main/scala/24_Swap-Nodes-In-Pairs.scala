object SwapNodesInPairs {
  def swapPairs(head: ListNode): ListNode = {
    def createNode(node: ListNode): ListNode = {
      if (node == null || node.next == null) {
        node
      } else {
        val next = createNode(node.next.next)
        var newNode = node.next
        newNode.next = node
        newNode.next.next = next
        newNode
      }
    }
    createNode(head)
  }
}