import util.ReadMode._
import scala.collection.mutable

object d21:

  val allQuantumDeltas: Map[Int, Map[Int, Int]] = (0 until 10).map(pos => (pos,
    (for {
      firstRoll <- 1 to 3
      secondRoll <- 1 to 3
      thirdRoll <- 1 to 3
      newPos = (pos + firstRoll + secondRoll + thirdRoll) % 10
    } yield newPos).groupBy(s => s).map(t => (t._1, t._2.length))
  )).toMap

  object QuantumCache:
    private val cache = mutable.Map[(Int, Int, Int, Int), (Long, Long)]()

    def resultOf(player1Pos: Int, player2Pos: Int, player1Score: Int, player2Score: Int): (Long, Long) =
      if(player2Score >= 21) (0l, 1l)
      else cache.getOrElseUpdate((player1Pos, player2Pos, player1Score, player2Score),
        allQuantumDeltas(player1Pos).foldLeft((0l, 0l))((sum, newPosition) =>
          val (player2Wins, player1Wins) = resultOf(player2Pos, newPosition._1, player2Score, player1Score + newPosition._1 + 1)
          (sum._1 + player1Wins * newPosition._2, sum._2 + player2Wins * newPosition._2)
        ))

  case class GameSate(
    player1Pos: Int,
    player2Pos: Int,
    player1Score: Int,
    player2Score: Int,
    dieRolled: Int
  ):
    def advance(newPlayer1Pos: Int): GameSate =
      GameSate(
        player2Pos,
        newPlayer1Pos,
        player2Score,
        player1Score + newPlayer1Pos + 1,
        dieRolled + 3
      )

    def advancedDeterministic: GameSate = advance((player1Pos + dieRolled * 3 + 6) % 10)

    def finishDeterministic: GameSate = advancedDeterministic match {
      case state: GameSate if state.player2Score >= 1000 => state
      case state: GameSate => state.finishDeterministic
    }

    def finishQuantum: (Long, Long) = QuantumCache.resultOf(player1Pos, player2Pos, player1Score, player2Score)

  def main(args: Array[String]): Unit =
    val lines = util.readAllFrom("d21")//, Example)
    val player1Start = lines.head match {
      case s"Player 1 starting position: $p" => p.toInt
    }
    val player2Start = lines.tail.head match {
      case s"Player 2 starting position: $p" => p.toInt
    }
    val initialState = GameSate(player1Start-1, player2Start-1, 0, 0, 0)
    val finalState = initialState.finishDeterministic
    println(finalState.player1Score * finalState.dieRolled)
    println(initialState.finishQuantum)