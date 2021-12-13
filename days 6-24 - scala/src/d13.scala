import d13.FoldInstruction.*

import scala.math.max

object d13:

  enum FoldInstruction(along: Int):
    case Vertical(x: Int) extends FoldInstruction(x)
    case Horizontal(y: Int) extends FoldInstruction(y)

  case class PaperState(width: Int, height: Int, dots: (Int, Int) => Boolean):
    def fold(instruction: FoldInstruction): PaperState = instruction match {
      case Vertical(x) => PaperState(max(x, width - x - 1), height, (x, y) => dots(x, y) || dots(width - x - 1, y))
      case Horizontal(y) => PaperState(width, max(y, height - y - 1), (x, y) => dots(x, y) || dots(x, height - y - 1))
    }
    lazy val dotCount: Int = (for {
      x <- 0 until width
      y <- 0 until height
    } yield (x,y)).count(t => dots(t._1, t._2))
    def print(): Unit = println((0 until height).map(y => (0 until width).map(x => if(dots(x, y)) '#' else '.').mkString).mkString("\n"))

  def main(args: Array[String]): Unit =
    val lines = util.readAllFrom("d13")
    val idx = lines.indexOf("")
    val dotCodes = lines.take(idx)
    val dotCoords = dotCodes.map { case s"$x,$y" => (x.toInt, y.toInt) }
    val foldCodes = lines.drop(idx + 1)
    val foldInstructions = foldCodes.map {
      case s"fold along x=$x" => FoldInstruction.Vertical(x.toInt)
      case s"fold along y=$y" => FoldInstruction.Horizontal(y.toInt)
    }
    val width = dotCoords.map(_._1).max + 1
    val height = dotCoords.map(_._2).max + 1
    val toMap = dotCoords.groupBy(_._1).map(t => (t._1, t._2.map(_._2)))
    val initialState = PaperState(
      width, height,
      (x, y) => toMap.contains(x) && toMap(x).contains(y)
    )
    initialState.print()
    println()
    val secondState = initialState.fold(foldInstructions.head)
    val finalState = foldInstructions.foldLeft(initialState)((state, instr) => state.fold(instr))
    finalState.print()
    println(secondState.dotCount)