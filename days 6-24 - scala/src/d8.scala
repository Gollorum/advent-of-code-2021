import scala.collection.mutable
import scala.io.StdIn.readLine

object d8:

  val numberMapping: Map[Int, Set[CharMapping]] = Map[Int, Set[CharMapping]](
    (0, Set('a', 'b', 'c', 'e', 'f', 'g')),
    (1, Set('c', 'f')),
    (2, Set('a', 'c', 'd', 'e', 'g')),
    (3, Set('a', 'c', 'd', 'f', 'g')),
    (4, Set('b', 'c', 'd', 'f')),
    (5, Set('a', 'b', 'd', 'f', 'g')),
    (6, Set('a', 'b', 'd', 'e', 'f', 'g')),
    (7, Set('a', 'c', 'f')),
    (8, Set('a', 'b', 'c', 'd', 'e', 'f', 'g')),
    (9, Set('a', 'b', 'c', 'd', 'f', 'g')),
  )

  implicit class CharMapping(val c: Char) {
    override def toString: String = c.toString

    override def equals(obj: Any): Boolean = obj match {
      case otherC: CharMapping => c == otherC.c
      case _ => false
    }

    override def hashCode(): Int = c.hashCode()
  }

  val uniqueNumberLengths: Seq[Int] =
    (0 to 7).filter(c => numberMapping.count(_._2.size == c) == 1)

  def main(args: Array[String]): Unit =
    var lines = List[String]()
    var l = readLine()
    while(l != "") {
      lines = lines.appended(l)
      l = readLine()
    }
    val entries = lines.map(l => l.splitAt(l.indexOf(" | ")))
      .map(t => (
        t._1.split(' '),
        t._2.drop(" | ".length).split(' ')
      ))
    for(l <- entries) println(l._1.mkString("(", ", ", ") |") + l._2.mkString(" (", ", ", ")"))
    firstAssignment(entries)
    secondAssignment(entries)

  def firstAssignment(allEntries: List[(Array[String], Array[String])]): Unit =
    println(allEntries.map(_._2.count(s => uniqueNumberLengths.contains(s.length))).sum)


  def secondAssignment(allEntries: List[(Array[String], Array[String])]): Unit =
    println(allEntries.map(t => {
      val mapping = decodeMapping(t._1)
      t._2.foldLeft(0)((sum, s) => sum * 10 + mapping(s))
    }).sum)

  def decodeMapping(readings: Array[String]): String => Int =
    val mappings = readings.foldLeft(
      mutable.Set.from(('a' to 'g').permutations
        .map(chars => Map.from(chars.zip(('a' to 'g').map(CharMapping))))))(
      (possibilities, reading) => possibilities.filter(p => isPossibilityValidFor(p, reading)))

    if(mappings.size > 1) println("Multiple possible mappings found: " + mappings)
    val mapping = mappings.head
    s => numberMapping.find(t => t._2 == s.map(mapping).toSet).get._1

  def isPossibilityValidFor(p: Map[Char, CharMapping], s: String): Boolean = numberMapping.exists(_._2 == s.map(p).toSet)