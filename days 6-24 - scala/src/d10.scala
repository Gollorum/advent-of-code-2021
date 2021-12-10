object d10:

  val corruptionScore = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val completionScore = Map(
    ')' -> 1l,
    ']' -> 2l,
    '}' -> 3l,
    '>' -> 4l
  )

  val openers = List('(', '[', '{', '<')

  def closerFor(opener: Char): Char = opener match {
    case '(' => ')'
    case '[' => ']'
    case '{' => '}'
    case '<' => '>'
  }

  sealed trait ParsingResult
  object Success extends ParsingResult
  case class Incomplete(state: LineState) extends ParsingResult
  case class Corrupted(breakingChar: Char) extends ParsingResult

  case class LineState(openerStack: List[Char]):
    def TryAppend(c: Char): Either[Char, LineState] =
      if(openers.contains(c)) Right(LineState(c +: openerStack))
      else if(openerStack.nonEmpty && c == closerFor(openerStack.head)) Right(LineState(openerStack.tail))
      else Left(c)

  object LineState:

    def TryParse(s: String): ParsingResult = s.foldLeft[Either[Char, LineState]](Right(LineState(Nil)))((state, char) => state.flatMap(s => s.TryAppend(char))) match {
      case Left(closingChar) => Corrupted(closingChar)
      case Right(LineState(Nil)) => Success
      case Right(lineState) => Incomplete(lineState)
    }

  def main(args: Array[String]): Unit =
    val allLines = util.readAll()
    firstAssignment(allLines)
    secondAssignment(allLines)

  def firstAssignment(allLines: List[String]): Unit =
    println(allLines.map(s => LineState.TryParse(s) match {
      case Corrupted(breakingChar) => corruptionScore(breakingChar)
      case _ => 0
    }).sum)

  extension [T](t: T)
    def let[S](f: T => S): S = f(t)

  def secondAssignment(allLines: List[String]): Unit =
    val incompletionScores = allLines.collect(s => LineState.TryParse(s) match {
      case Incomplete(lineState) => lineState.openerStack.foldLeft(0l)((score, char) => score * 5l + completionScore(closerFor(char)))
    }).sorted
    println("Incompletion scores: \n" + incompletionScores.mkString("\n"))
    println(incompletionScores(incompletionScores.length / 2))