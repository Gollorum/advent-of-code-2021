import util.ReadMode.Example

object d25:

  enum Seegurke: // https://www.youtube.com/watch?v=hBtKWIljLUY
    case None, Right, Down

  case class OceanFloor(values: Array[Seegurke], width: Int, height: Int):
    def apply(x: Int, y: Int): Seegurke = values(x + width * y)
    def update(x: Int, y: Int, gurke: Seegurke): Unit = values(x % width + width * (y % height)) = gurke

    def progress(seegurke: Seegurke, move: (Int, Int) => (Int, Int)): Int = {
      val toMove = for {
        y <- 0 until height
        x <- 0 until width
        if values(x + width * y) == seegurke
        (x2, y2) = move(x, y)
        if this(x2, y2) == Seegurke.None
      } yield (x, y, x2, y2)
      for((x, y, x2, y2) <- toMove) {
        this(x, y) = Seegurke.None
        this(x2, y2) = seegurke
      }
      toMove.length
    }

    def progressRight(): Int = progress(Seegurke.Right, (x, y) => ((x+1) % width, y))
    def progressDown(): Int = progress(Seegurke.Down, (x, y) => (x, (y+1) % height))
    def progress(): Int = progressRight() + progressDown()

    private def moveToStop(): Int =
      if(progress() == 0) 1 else moveToStop() + 1

    def movesToStop: Int = OceanFloor(values.clone(), width, height).moveToStop()


  def main(args: Array[String]): Unit =
    val lines = util.readAllFrom("d25")
    val oceanFloor = OceanFloor(lines.flatMap(_.map{
      case '>' => Seegurke.Right
      case 'v' => Seegurke.Down
      case '.' => Seegurke.None
    }).toArray, lines.head.length, lines.length)
    println(oceanFloor.movesToStop)