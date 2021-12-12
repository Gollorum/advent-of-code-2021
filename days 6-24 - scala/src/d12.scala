object d12:

  case class Cave(id: String):
    lazy val isBig: Boolean = id.toUpperCase == id
    override def hashCode(): Int = id.hashCode()
    override def equals(obj: Any): Boolean = obj match {
      case Cave(otherId) if otherId == id => true
      case _ => false
    }
    override def toString: String = id

  def main(args: Array[String]): Unit =
    val lines = util.readAllFrom("d12")
    val connectionsOneDir = lines.map(s => s.splitAt(s.indexOf('-'))).map(t => Cave(t._1) -> Cave(t._2.drop(1)))
    val graph = (connectionsOneDir ++ connectionsOneDir.map(t => (t._2, t._1))).filter(t => t._2 != Cave("start") && t._1 != Cave("end"))
    firstAssignment(graph)
    secondAssignment(graph)

  def firstAssignment(graph: List[(Cave, Cave)]): Unit =
    println(allSimplePathsToEnd(graph, Cave("start"), Set(Cave("start"))).size)

  def secondAssignment(graph: List[(Cave, Cave)]): Unit =
    println(allComplexPathsToEnd(graph, Cave("start"), Set(Cave("start")), None).size)

  def allSimplePathsToEnd(graph: List[(Cave, Cave)], start: Cave, forbidden: Set[Cave]): Iterable[Seq[Cave]] = for {
    next <- graph.filter(_._1 == start).map(_._2)
    if !forbidden.contains(next)
    trail <- if (next.id == "end")
      Seq(Nil)
    else
      allSimplePathsToEnd(graph, next, if(next.isBig) forbidden else forbidden + next)
  } yield next +: trail

  def allComplexPathsToEnd(graph: List[(Cave, Cave)], start: Cave, forbidden: Set[Cave], doublyVisited: Option[Cave]): Iterable[Seq[Cave]] = for {
    next <- graph.filter(_._1 == start).map(_._2)
    if !forbidden.contains(next) || doublyVisited.isEmpty
    double = if(doublyVisited.isEmpty && forbidden.contains(next)) Some(next) else doublyVisited
    trail <- if (next.id == "end")
      Seq(Nil)
    else
      allComplexPathsToEnd(graph, next, if(next.isBig) forbidden else forbidden + next, double)
  } yield next +: trail