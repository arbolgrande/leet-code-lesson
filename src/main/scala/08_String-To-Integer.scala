object StringToInterger {
  def myAtoi(str: String): Int = {
    val regex = "[0-9]"
    def firstChar(strArray: Array[String]): (String, Array[String]) = {
      strArray.headOption match {
        case Some(s) if s.toString == " " => firstChar(strArray.tail)
        case Some(s) => (s.toString, strArray.tail)
        case None    => ("", strArray)
      }
    }
    var num = ""
    def convert(strArray: Array[String]): String = {
      strArray.headOption match {
        case Some(v) => {
          v match {
            case v1 if v.matches(regex) => {
              if(num.isEmpty && v == "0") {} else num = num.concat(v)
              convert(strArray.tail)
            }
            case v2 if v == " " => {
              num
            }
            case v3 if v == "+" || v == "-" => {
              num
            }
            case _ => num
          }
        }
        case None => num
      }
    }
    val first = firstChar(str.split(""))
    val res = first._1 match {
      case ""                        => "0"
      case "-"                       => s"-${convert(first._2)}"
      case "+"                       => convert(first._2)
      case v if v.matches(regex)     => s"${convert(first._2.+:(v))}"
      case _                         => "0"
    }
    res match {
      case v if v.isEmpty               => 0
      case v if v.length > 11           => if(v.startsWith("-")) Int.MinValue else Int.MaxValue
      case v if v == "+" || v == "-"    => 0
      case v if v.toLong < Int.MinValue => Int.MinValue
      case v if v.toLong > Int.MaxValue => Int.MaxValue
      case _                            => res.toInt
    }
  }
  def main(args: Array[String]): Unit = {
    println(myAtoi("20000000000000000000"))
  }
}