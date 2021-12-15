import scala.collection.mutable

object d15:

  case class Cave(private val riskLevels: Array[Int]):
    lazy val size: Int = Math.sqrt(riskLevels.length).toInt
    lazy val destination: (Int, Int) = (size - 1, size - 1)
    def riskAt(c: (Int, Int)): Int = riskLevels(c._1 + (c._2 * size))
    def contains(c: (Int, Int)): Boolean = c._1 >= 0 && c._1 < size && c._2 >= 0 && c._2 < size

    def enlarge(count: Int): Cave =
      Cave((for {
        y <- 0 until size * count
        x <- 0 until size * count
      } yield (riskAt((x % size, y % size)) + x / size + y / size - 1) % 9 + 1).toArray)

  def main(args: Array[String]): Unit =
    val lines = util.readAllFrom("d15")
    val cave = Cave(lines.flatMap(_.map(_.asDigit)).toArray)
    println(findBestPathCosts(cave))
    println(findBestPathCosts(cave.enlarge(5)))

  def findBestPathCosts(cave: Cave): Int =
    val knownCosts = mutable.Map[(Int, Int), Int]()
    def costsToReach(c: (Int, Int)): Int =
      neighborsOf(c, cave).filter(knownCosts.contains).map(knownCosts).min
        + cave.riskAt(c)
    var coordsToExpand = List(((0,0), 0))
    while(coordsToExpand.nonEmpty) {
      val (c, costsToReachC) = coordsToExpand.head
      if (c == cave.destination) return costsToReachC
      knownCosts(c) = costsToReachC
      coordsToExpand = (coordsToExpand.tail ++ neighborsOf(c, cave).map(c => (c, costsToReachC + cave.riskAt(c))))
        .filter(c => !knownCosts.contains(c._1))
        .groupBy(_._1).map(t => (t._1, t._2.map(_._2).min)).toList.sortBy(_._2)
    }
    throw new Exception("No path found")

  def neighborsOf(c: (Int, Int), cave: Cave): Seq[(Int, Int)] = Seq(
    (c._1 + 1, c._2),
    (c._1, c._2 + 1),
    (c._1 - 1, c._2),
    (c._1, c._2 - 1)
  ).filter(cave.contains)