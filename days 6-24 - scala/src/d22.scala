import util.ReadMode.Example
import scala.math.max
import scala.math.min

object d22:

  case class Position(x: Int, y: Int, z: Int)

  case class Instruction(x: Range, y: Range, z: Range, on: Boolean):
    def tryGetState(pos: Position): Option[Boolean] =
      if(x.contains(pos.x) && y.contains(pos.y) && z.contains(pos.z)) Some(on) else None
    def asCube: Cube = Cube(x, y, z, if(on) 1 else -1)

  object Instruction:
    def unapply(s: String): Option[(Range, Range, Range, Boolean)] = s match {
      case s"on x=$xMin..$xMax,y=$yMin..$yMax,z=$zMin..$zMax" => Some(xMin.toInt to xMax.toInt, yMin.toInt to yMax.toInt, zMin.toInt to zMax.toInt, true)
      case s"off x=$xMin..$xMax,y=$yMin..$yMax,z=$zMin..$zMax" => Some(xMin.toInt to xMax.toInt, yMin.toInt to yMax.toInt, zMin.toInt to zMax.toInt, false)
      case _ => None
    }

  case class Cube(x: Range, y: Range, z: Range, value: Int):
    def total: Long = x.size.toLong * y.size * z.size * value
    def differenceWith(other: Cube): Option[Cube] =
      val xMin = max(x.start, other.x.start)
      val yMin = max(y.start, other.y.start)
      val zMin = max(z.start, other.z.start)
      val xMax = min(x.end, other.x.end)
      val yMax = min(y.end, other.y.end)
      val zMax = min(z.end, other.z.end)
      if(xMin > xMax || yMin > yMax || zMin > zMax) None
      else Some(Cube(xMin to xMax, yMin to yMax, zMin to zMax, -value))

  case class ReactorState(instructions: Seq[Instruction]):
    def activeCubesIn(xR: Range, yR: Range, zR: Range): Int =
      val cubeStates: Seq[Boolean] = for {
        x <- xR
        y <- yR
        z <- zR
      } yield instructions.map[Option[Boolean]](i => i.tryGetState(Position(x, y, z))).collectFirst[Boolean]{case Some(state) => state}.getOrElse[Boolean](false)
      cubeStates.count(bool => bool)

  case class ReactorStateV2(cubes: Seq[Cube]):
    def +(instruction: Instruction): ReactorStateV2 =
      ReactorStateV2(cubes ++ cubes.flatMap(_.differenceWith(instruction.asCube)) ++ (if(instruction.on) Seq(instruction.asCube) else Nil))
    def total: Long = cubes.map(_.total).sum

  def main(args: Array[String]): Unit =
    val lines = util.readAllFrom("d22")
    val instructions = lines.map{ case Instruction(x, y, z, isOn) => Instruction(x, y, z, isOn) }

    println(ReactorState(instructions.reverse).activeCubesIn(-50 to 50, -50 to 50, -50 to 50))

    // Only valid for example
    println(instructions.take(10).foldLeft(ReactorStateV2(Nil))((state, i) => state + i).total)

    println(instructions.foldLeft(ReactorStateV2(Nil))((state, i) => state + i).total)