import scala.collection.mutable
import scala.io.StdIn.readLine

object d9:

  case class Grid(width: Int, height: Int, values: List[Int]) {

    def apply(x: Int, y: Int): Int = values(x + width * y)

    def isInBounds(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height

    def isLowPoint(x: Int, y: Int): Boolean = lowestNeighbor(x, y).isEmpty

    def lowestNeighbor(x: Int, y: Int): Option[(Int, Int)] =
      val value = this(x, y)
      Seq((x-1, y), (x, y-1), (x+1, y), (x, y+1))
        .filter(t => isInBounds(t._1,t._2) && this(t._1, t._2) < value)
        .minByOption(t => this(t._1, t._2))

    private val sinkFor: mutable.Map[(Int, Int), Option[(Int, Int)]] = mutable.Map()

    def findSink(x: Int, y: Int): Option[(Int, Int)] =
      if(!sinkFor.contains((x, y)))
        sinkFor.addOne((x, y),
          if(this(x, y) == 9) None
          else lowestNeighbor(x, y)
            .map(t => findSink(t._1, t._2))
            .getOrElse(Some((x, y)))
        )
      sinkFor((x, y))

    def print(): Unit = println((0 until width).map(x => (0 until height).map(y => this(x, y)).mkString).mkString("\n"))

    lazy val lowPoints: IndexedSeq[(Int, Int)] = for {
      x <- 0 until width
      y <- 0 until height
      if isLowPoint(x, y)
    } yield (x, y)

    lazy val allBasinSizes: Iterable[Int] = (for {
      x <- 0 until width
      y <- 0 until height
    } yield (x, y)).collect(t => findSink(t._1, t._2) match {
      case Some(sink) => sink
    }).groupBy(sink => sink).values.map(_.length).toList.sortBy(size => -size)

  }

  def main(args: Array[String]): Unit =
    var lines = List[String]()
    var l = readLine()
    while(l != "") {
      lines = lines.appended(l)
      l = readLine()
    }

    val grid = Grid(lines.head.length, lines.length, lines.flatMap(s => s.map(_.toString.toInt)))

    firstAssignment(grid)
    secondAssignment(grid)

  def firstAssignment(grid: Grid): Unit = println("Risk level: " +
    grid.lowPoints.map(t => grid(t._1, t._2) + 1).sum
  )

  def secondAssignment(grid: Grid): Unit = println("BasinSizes: " +
    grid.allBasinSizes.take(3).product
  )
