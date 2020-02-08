object Main {
  def solveNQueens(n: Int): List[List[String]] = {
    val range = (0 until n).toList
    // n番目にQを仕込む
    def createString(target: Int, current: Int, str: String): String = {
      if(current < n) {
        val newStr = if(current == target) {
          str.concat("Q")
        } else {
          str.concat(".")
        }
        createString(target, current + 1, newStr)
      } else {
        str
      }
    }
    // バックトラックで配列を生成
    def loop(list: List[List[String]], current: Int): List[List[String]] = {
      if(current < n) {
        None
        val newList = list.flatMap { v =>
          val vs = v.map(_.indexOf("Q"))
          range.flatMap { n =>
            if(vs.contains(n)) None else Some(v.:+(createString(n, 0, "")))
          }
        }
        loop(newList, current + 1)
      } else {
        list
      }
    }
    val map = scala.collection.mutable.HashMap[Int, Int]()
    def checkMap(row: Int, diff: Int, currentRow: Int, index: Int): Boolean = {
      if(row < 0) {
        true
      } else {
        if(map.get(index - diff).nonEmpty && map(index - diff) == currentRow - diff) {
          false
        } else if(map.get(index + diff).nonEmpty && map(index + diff) == currentRow - diff) {
          false
        } else {
          checkMap(row - 1, diff + 1, currentRow, index)
        }
      }
    }
    def check(current: Int,list: List[String]): Boolean = {
      list.headOption match {
        case Some(v) =>
          val index = v.indexOf("Q")
          if(map.get(index).nonEmpty || !checkMap(current -1, 1, current, index)) {
            false
          } else {
            map.update(index, current)
            check(current + 1,list.tail)
          }
        case None =>
          true
      }
    }
    (0 until n).flatMap { v =>
      loop(List(List(createString(v, 0, ""))), 1)
    }.flatMap { v =>
      val opt = if(!check(1, v))
        None
      else
        Some(v)
      map.clear()
      opt
    } .toList
  }

  def main(args: Array[String]): Unit = {
    println(solveNQueens(8))
  }


}