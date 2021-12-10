import scala.io.StdIn.readLine

object util:

  def readAll(): List[String] =
    var lines = List[String]()
    var l = readLine()
    while(l != "") {
      lines = lines.appended(l)
      l = readLine()
    }
    lines