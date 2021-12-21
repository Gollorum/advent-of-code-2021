import util.ReadMode._

import scala.io.Source
import scala.io.StdIn.readLine

object util:

  enum ReadMode:
    case Release, Example

  def readAllFrom(path: String, mode: ReadMode = Release): List[String] =
    val source = Source.fromFile("input/" + path + (if(mode == Example) "_example" else ""))
    val lines = source.getLines.toList
    source.close()
    lines

  extension [A](a: A)
    def let[B](f: A => B): B = f(a)