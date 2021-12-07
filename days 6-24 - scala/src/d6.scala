import scala.io.StdIn.readLine
import scala.language.postfixOps

object d6:

  case class FishPopulation(c: List[Long]):

    def progress(): FishPopulation = FishPopulation(c.drop(1).updated(6, c(7) + c.head).appended(c.head))

    def +(i: Int): FishPopulation = FishPopulation(c.updated(i, c(i) + 1))

    def sum: Long = c.sum

  object FishPopulation:
    val zero: FishPopulation = FishPopulation(List.fill(9)(0))

  val days = 256

  def main(args: Array[String]): Unit =
    val initialPopulation = readLine().split(',').map(_.toInt).foldLeft(FishPopulation.zero)((pop, i) => pop + i)
    val fishCount = (0 until days).foldLeft(initialPopulation)((pop, _) => pop.progress())
    println(fishCount.sum)
