object d14:

  type ElementPair = (Char, Char)
  type Mapping = (ElementPair, Seq[ElementPair])
  object Mapping:
    def apply(s: String): Mapping =
      val (l, r, insert) = s match { case s"$from -> $to" => (from.head, from.last, to.head) }
      (l, r) -> Seq((l, insert), (insert, r))

  def main(args: Array[String]): Unit =
    val lines = util.readAllFrom("d14")
    val startingPattern = lines.head
    val mappings = lines.drop(2).map(Mapping.apply).toMap
    applyMappingsAndPrint(10, startingPattern, mappings)
    applyMappingsAndPrint(40, startingPattern, mappings)

  extension [T](i: Iterable[(T, Long)])
    def collectCounts: Map[T, Long] = i.groupBy(_._1).map(t => (t._1, t._2.map(_._2).sum))

  def applyMappingsAndPrint(times: Int, startingPattern: String, mappings: Map[ElementPair, Seq[ElementPair]]): Unit =
    val pairCounts: Iterable[(ElementPair, Long)] = startingPattern.sliding(size = 2, step = 1).map(s => (s.head, s.last)).toList
      .groupBy(identity).map(t => (t._1, t._2.size.toLong))
    val pairCountsAfterApplication = (0 until times).foldLeft(pairCounts)((counts, _) => (for {
      (chars, count) <- counts
      target <- mappings(chars)
    } yield (target, count)).collectCounts)
    val counts = (pairCountsAfterApplication.flatMap{ case ((l, r), i) => List((l, i), (r, i))}
        ++ Seq((startingPattern.head, 1l), (startingPattern.last, 1l))
      ).collectCounts.map(t => (t._1, t._2 / 2))
    println(pairCountsAfterApplication.mkString(", "))
    println(counts.values.max - counts.values.min)