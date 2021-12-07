import scala.io.StdIn.readLine

object d7:

  def main(args: Array[String]): Unit =
    val allCrabs = readLine().split(',').map(_.toInt).sorted
    println(secondAssignment(allCrabs))

  def firstAssignment(allCrabs: Array[Int]): Int =
    (allCrabs.head to allCrabs.last).map(p => allCrabs.map(c => (p - c).abs).sum).min

  def secondAssignment(allCrabs: Array[Int]): Int =
    (allCrabs.head to allCrabs.last).map(p => allCrabs.map(c => (p - c).abs).map(p => p * (p+1) / 2).sum).min