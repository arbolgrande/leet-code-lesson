/**
  * Definition for singly-linked list.
  * class ListNode(var _x: Int = 0) {
  *   var next: ListNode = null
  *   var x: Int = _x
  * }
  */

class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
}

object Solution02 {
  def createNode(str: String, node: ListNode): ListNode = {
    str.headOption match {
      case Some(c) =>
        val newLi = new ListNode(c.asDigit)
        newLi.next = node
        createNode(str.tail, newLi)
      case None => node
    }
  }
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    var l1Num, l2Num: Array[Int] = Array()
    var resArray = Array.empty[String]
    def createString(l1Str: String, l2Str: String, hasCarryUp: Boolean): Array[String] = {
      (l1Str.lastOption, l2Str.lastOption) match {
        case (Some(x), Some(y)) => {
          val num = if(hasCarryUp) {
            x.asDigit + y.asDigit + 1
          } else {
            x.asDigit + y.asDigit
          }
          resArray = resArray.+:(if(num > 9) (num - 10).toString else num.toString)
          createString(l1Str.init, l2Str.init, num > 9)
        }
        case (Some(x), None) =>
          val num = if(hasCarryUp) {
            x.asDigit + 0 + 1
          } else {
            x.asDigit + 0
          }
          resArray = resArray.+:(if(num > 9) (num - 10).toString else num.toString)
          createString(l1Str.init, l2Str, num > 9)
        case (None, Some(y)) =>
          val num = if(hasCarryUp) {
            0+ y.asDigit + 1
          } else {
            0 + y.asDigit
          }
          resArray = resArray.+:(if(num > 9) (num - 10).toString else num.toString)
          createString(l1Str, l2Str.init, num > 9)
        case (None, None) =>
          if(hasCarryUp) {
            resArray = resArray.+:("1")
            resArray
          } else {
            resArray
          }
      }
    }
    def loop(li1: ListNode,li2: ListNode): Unit = {
      (li1, li2) match {
        case (_: ListNode, _: ListNode) => {
          l1Num = l1Num.+:(li1.x)
          l2Num = l2Num.+:(li2.x)
          loop(li1.next, li2.next)
        }
        case (x: ListNode, null) => {
          l1Num = l1Num.+:(x.x)
          loop(li1.next, li2)
        }
        case (null, y: ListNode) => {

          l2Num = l2Num.+:(y.x)
          loop(li1, li2.next)
        }
        case (null , null) => {
        }
      }
    }
    loop(l1, l2)
    val l1Str = l1Num.mkString
    val l2Str = l2Num.mkString
    val str = createString(l1Str, l2Str,false).mkString
    val node = createNode(str.tail, new ListNode(str.head.asDigit))
    node
  }
  def main(args: Array[String]): Unit = {
    addTwoNumbers(new ListNode(0), new ListNode(0))
  }
}