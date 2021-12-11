import scala.io.Source
import scala.io.StdIn.readLine

object util:

  def readAllFrom(path: String): List[String] =
    val source = Source.fromFile("input/" + path)
    val lines = source.getLines.toList
    source.close()
    lines