object d11:

  case class Octopus(energy: Int, hasFlashed: Boolean):
    def progress: Octopus = Octopus(energy + 1, false)
    def resetIfFlashed: Octopus = Octopus(if(hasFlashed) 0 else energy, false)

  case class OctoGrid(octopuses: Seq[Octopus], flashesSinceStart: Int, flashedLastStep: Int = 0):
    val width = 10
    val height = 10
    def apply(x: Int, y: Int): Octopus = octopuses(x + y * width)

    def map(f: Octopus => Octopus): OctoGrid = OctoGrid(octopuses.map(f), flashesSinceStart)

    def progress: OctoGrid =
//      print
      OctoGrid(octopuses.map(_.progress), flashesSinceStart)
        .handleFlashes()
        .resetAllFlashed

    def resetAllFlashed: OctoGrid =
      val count = octopuses.count(_.hasFlashed)
      OctoGrid(octopuses.map(_.resetIfFlashed), flashesSinceStart, count)

    def print: Unit =
      println((0 until height).map(y => (0 until width).map(x => this(x,y).energy).mkString).mkString("\n") + "\n")

    val allIndices: Seq[(Int, Int)] = for {
      y <- 0 until height
      x <- 0 until width
    } yield (x, y)

    def handleFlashes(lookAt: Seq[(Int, Int)] = allIndices): OctoGrid =
      val flashing = for {
        (x, y) <- lookAt
        octo = this (x, y)
        if !octo.hasFlashed && octo.energy > 9
      } yield (x, y)
      if(flashing.isEmpty) this
      else {
        val neighbors = for {
          (x, y) <- flashing
          xNeigh <- (x-1) to (x+1)
          if xNeigh >= 0 && xNeigh < width
          yNeigh <- (y-1) to (y+1)
          if yNeigh >= 0 && yNeigh < height
        } yield (xNeigh, yNeigh)
        OctoGrid(
          for {
            y <- 0 until height
            x <- 0 until width
            octo = this(x, y)
          } yield Octopus(octo.energy + neighbors.count(_ == (x,y)), octo.hasFlashed || flashing.contains((x,y))),
          flashesSinceStart = flashesSinceStart + flashing.size
        ).handleFlashes(neighbors.distinct)
      }


  def main(args: Array[String]): Unit =
    val lines = util.readAllFrom("d11")
    val grid = OctoGrid(lines.flatMap(_.map(c => Octopus(c.asDigit, false))), flashesSinceStart = 0)
    firstAssignment(grid)
    secondAssignment(grid)

  def firstAssignment(octoGrid: OctoGrid): Unit =
    println((0 until 100).foldLeft(octoGrid)((grid, _) => grid.progress).flashesSinceStart)

  def secondAssignment(octoGrid: OctoGrid): Unit =
    println((0 until 1000).foldLeft(octoGrid +: Nil)((grids, i) => grids :+ grids.last.progress).indexWhere(_.flashedLastStep == 100))
