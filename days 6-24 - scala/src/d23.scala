import java.util.Objects
import scala.math.{abs, cbrt, cos}
import scala.collection.mutable

object d23:

  private val roomFor = Map.from(('A' to 'D').zipWithIndex)
  private val xIndexFor = roomFor.map(t => (t._1, t._2 * 2 + 2))
  private val moveCostFor = Map.from(('A' to 'D').zip(Seq(1, 10, 100, 1000)))

  extension (i: Int)
    def rangeTo(j: Int): Range = if(i <= j) i to j else i to j by -1

  case class RoomState(
    hallway: IndexedSeq[Char], // size: 11
    rooms: IndexedSeq[IndexedSeq[Char]] // size: 4x2
  ):
    override lazy val toString: String =
      Seq.fill(13)('#').mkString +
        "\n#" + hallway.mkString + "#\n" + rooms(0).indices.map(i =>
        "  #" + rooms.map(_(i)).mkString("#") + "#  ").mkString("\n")
    def allWithPosition: Seq[(Char, (Int, Int))] =
      hallway.zip(hallway.indices.map((_, 0))).filter(_._1 != '.') ++ (
        for {
          (room, roomIndex) <- rooms.zipWithIndex
          (element, indexInRoom) <- room.zipWithIndex
          if element != '.'
        } yield (element, (2 + 2 * roomIndex, 1 + indexInRoom)))

    lazy val distanceFromGoal: Int = allWithPosition.map{
      case (c, (x, y)) => moveCostFor(c) * (if(x != xIndexFor(c)) abs(x - xIndexFor(c)) + y + 1 else 0)
    }.sum

    def possibleNextSteps: Seq[(RoomState, Int)] = allWithPosition.flatMap{
      case (c, (x, y)) => tryEnterHome(c, x, y) ++ waysToEnterHallway(c, x, y)
    }

    def tryEnterHome(c: Char, x: Int, y: Int): Seq[(RoomState, Int)] =
      if(canEnterHome(c, x, y))
        val home = roomFor(c)
        val yDest = rooms(home).indices.takeWhile(i => rooms(home)(i) == '.').last + 1
        val xRange = x.rangeTo(home * 2 + 2)
        val costs = moveCostFor(c) * (y + yDest + xRange.size - 1)
        val resultingRoom = RoomState(
          if(y == 0) hallway.zipWithIndex.map(t => if(t._2 == x) '.' else t._1) else hallway,
          rooms.zipWithIndex.map{
            case(curr, i) if i * 2 + 2 == x => (curr.take(y-1) :+ '.') ++ curr.drop(y)
            case(curr, i) if i == home => (curr.take(yDest-1) :+ c) ++ curr.drop(yDest)
            case(curr, _) => curr
          }
        )
        (resultingRoom, costs) +: Nil
      else Nil

    def canEnterHome(c: Char, x: Int, y: Int): Boolean =
      val home = roomFor(c)
      val homeIdx = home * 2 + 2
      homeIdx != x // not yet home
        && !(y > 0 && (hallway(x) != '.' || rooms(x / 2 - 1).take(y - 1).exists(_!='.'))) // can leave room
        && rooms(home)(0) == '.' // home entry free
        && rooms(home).forall(curr => curr == '.' || curr == c) // home not blocked by stranger
        && !x.rangeTo(homeIdx).drop(1).exists(x => hallway(x) != '.') // hallway free

    def waysToEnterHallway(c: Char, x: Int, y: Int): Seq[(RoomState, Int)] =
      val currentRoom = x / 2 - 1
      if(
        y == 0 // already in hallway
        || (xIndexFor(c) == x && rooms(currentRoom).drop(y).forall(curr => curr == '.' || curr == c)) // already home
        || rooms(currentRoom).take(y-1).exists(curr => curr != '.') // top of room blocked
      ) Nil
      else for {
        xDest <- x.rangeTo(0).takeWhile(x => hallway(x) == '.') ++ x.rangeTo(10).takeWhile(x => hallway(x) == '.')
        if xDest == 0 || xDest == 10 || xDest % 2 == 1
        costs = moveCostFor(c) * (y + x.rangeTo(xDest).size - 1)
      } yield (
        RoomState(
          hallway.zipWithIndex.map(t => if(t._2 == xDest) c else t._1),
          rooms.zipWithIndex.map{
            case(curr, i) if i * 2 + 2 == x => (curr.take(y-1) :+ '.') ++ curr.drop(y)
            case(curr, _) => curr
          }
        ),
        costs
      )

  object RoomList:
    def unapply(s: List[String]): Option[List[(Char, Char, Char, Char)]] = s match {
      case Nil => Some(Nil)
      case s"###$a#$b#$c#$d###" +: RoomList(tail) => Some((a.head, b.head, c.head, d.head) +: tail)
      case s"  #$a#$b#$c#$d#" +: RoomList(tail) => Some((a.head, b.head, c.head, d.head) +: tail)
      case _ => None
    }

  def main(args: Array[String]): Unit =
    val lines = util.readAllFrom("d23")
    val initialState = lines.drop(2).dropRight(1) match {
      case RoomList(roomLayers) => RoomState(IndexedSeq.fill(11)('.'), IndexedSeq(
        roomLayers.map(_._1).toIndexedSeq,
        roomLayers.map(_._2).toIndexedSeq,
        roomLayers.map(_._3).toIndexedSeq,
        roomLayers.map(_._4).toIndexedSeq
      ))
    }
    println(initialState)
    case class PossibleStep(newState: RoomState, costsToReach: Int, history: Seq[(RoomState, Int)]) extends Ordered[PossibleStep]:
      import scala.math.Ordered.orderingToOrdered
      def compare(that: PossibleStep): Int = ((this.costsToReach + this.newState.distanceFromGoal) compare (that.costsToReach + that.newState.distanceFromGoal)) match {
        case 0 => this.newState.toString.hashCode() compare that.newState.toString.hashCode()
        case a => a
      }
    val nextSteps = scala.collection.mutable.SortedSet.from[PossibleStep](
      initialState.possibleNextSteps.map(t => PossibleStep(t._1, t._2, Seq((initialState, 0))))
    )
    var isDone = false
    while(!isDone) {
      print(s"\rTo Expand: ${nextSteps.size}")
      val toExpand = nextSteps.head
      if(toExpand.newState.distanceFromGoal == 0)
        isDone = true
        println()
        type FoldType = (Int, Seq[(RoomState, Int)])
        val foldSeed: FoldType = (0, Nil)
        def fold(curr: FoldType, next: (RoomState, Int)): FoldType = (next._2, curr._2 :+ (next._1, next._2 - curr._1))
        println(toExpand.history.foldLeft[FoldType](foldSeed)(fold)._2.mkString("\n\n"))
        println()
        println(toExpand.newState.toString + "\n\n" + toExpand.costsToReach)
      else
        val newNextSteps = toExpand.newState.possibleNextSteps.map(t => PossibleStep(t._1, t._2 + toExpand.costsToReach, toExpand.history :+ (toExpand.newState, toExpand.costsToReach)))
        nextSteps.remove(nextSteps.head)
        for(s <- newNextSteps)
          nextSteps.find(_.newState == s.newState) match {
            case Some(oldStep) => if(s.costsToReach < oldStep.costsToReach)
              nextSteps.remove(oldStep)
              nextSteps.addOne(s)
            case None =>
              nextSteps.addOne(s)
          }
    }