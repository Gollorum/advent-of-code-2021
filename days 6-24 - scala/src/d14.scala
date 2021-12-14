object d14:

  extension (s: String)
    def asChar: Char = s.head

  case class Mapping(l: Char, r: Char, insert: Char):
    implicit def asTuple: ((Char, Char), Seq[(Char, Char)]) = (l, r) -> Seq((l, insert), (insert, r))
  object Mapping:
    def apply(s: String): Mapping = s match {
      case s"$from -> $to" => Mapping(from.head, from.last, to.head)
    }

  def main(args: Array[String]): Unit =
    val lines = util.readAllFrom("d14")
    val startingPattern = lines.head
    val mappings = lines.drop(2).map(Mapping.apply(_).asTuple).toMap
    applyMappingsAndPrint(10, startingPattern, mappings)
    applyMappingsAndPrint(40, startingPattern, mappings)

  extension [T](i: Iterable[(T, Long)])
    def collectCounts: Map[T, Long] = i.groupBy(_._1).map(t => (t._1, t._2.map(_._2).sum))

  def applyMappingsAndPrint(times: Int, startingPattern: String, mappings: Map[(Char, Char), Seq[(Char, Char)]]): Unit =
    val pairCounts = startingPattern.sliding(size = 2, step = 1).map(s => (s.head, s.last)).toList.groupBy(identity).map(t => (t._1, t._2.size.toLong))
    val pairCountsAfterApplication = (0 until times).foldLeft(pairCounts)((counts, _) => counts.flatMap[((Char, Char), Long)] { case (chars, count) =>
          mappings(chars)
            .map(target => (target, count))
        }.collectCounts)
    val counts = (pairCountsAfterApplication.flatMap[(Char, Long)]{ case ((l, r), i) => List((l, i), (r, i))}
      ++ Seq((startingPattern.head, 1l), (startingPattern.last, 1l)))
      .collectCounts.map(t => (t._1, t._2 / 2))
    println(counts.mkString(", "))
    println(counts.values.max - counts.values.min)