import util.ReadMode.Example
import scala.math.max
import scala.math.min

object d22:

  object Instruction:
    def unapply(s: String): Option[(Range, Range, Range, Boolean)] = s match {
      case s"$turnOn x=$xMin..$xMax,y=$yMin..$yMax,z=$zMin..$zMax" =>
        Some(xMin.toInt to xMax.toInt, yMin.toInt to yMax.toInt, zMin.toInt to zMax.toInt, turnOn == "on")
      case _ => None
    }

  case class Cube(x: Range, y: Range, z: Range, value: Int):
    def total: Long = x.size.toLong * y.size * z.size * value
    def intersectionWith(other: Cube, newValue: Int = value): Option[Cube] =
      val xMin = max(x.start, other.x.start)
      val yMin = max(y.start, other.y.start)
      val zMin = max(z.start, other.z.start)
      val xMax = min(x.end, other.x.end)
      val yMax = min(y.end, other.y.end)
      val zMax = min(z.end, other.z.end)
      if(xMin > xMax || yMin > yMax || zMin > zMax) None
      else Some(Cube(xMin to xMax, yMin to yMax, zMin to zMax, newValue))
    def differenceWith(other: Cube): Option[Cube] = intersectionWith(other, -value)

  case class ReactorState(cubes: Seq[Cube]):
    def +(instruction: Cube): ReactorState =
      ReactorState(cubes ++ cubes.flatMap(_.differenceWith(instruction)) ++ (if(instruction.value != 0) Seq(instruction) else Nil))
    def total: Long = cubes.map(_.total).sum

  def main(args: Array[String]): Unit =
    val lines = util.readAllFrom("d22")
    val instructions = lines.map{ case Instruction(x, y, z, isOn) => Cube(x, y, z, if(isOn) 1 else 0) }

    println(instructions.flatMap(_.intersectionWith(Cube(-50 to 50, -50 to 50, -50 to 50 , 0)))
      .foldLeft(ReactorState(Nil))((state, i) => state + i).total)

    println(instructions.foldLeft(ReactorState(Nil))((state, i) => state + i).total)