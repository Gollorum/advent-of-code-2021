import scala.language.implicitConversions
import scala.collection.mutable
import util.let

object d24:

  type Num = Long

  // One segment works like this:
  // input: d, a, b, digit
  // x = (z % 26 + a) != digit
  // if(d == 26) z /= 26
  // if(x) z = z * 26 + digit + b
  //
  // For last step: z < 26 && !x

  case class Step(d: Boolean, a: Int, b: Int)
  object Step:
    def parse(s: List[String]): List[Step] = s match {
      case Nil => Nil
      case "26" +: a +: b +: tail => Step(true, a.toInt, b.toInt) +: parse(tail)
      case "1" +: a +: b +: tail => Step(false, a.toInt, b.toInt) +: parse(tail)
    }

  private val stateResultCache = mutable.Map[(Num, Int), Seq[Seq[Int]]]()

  case class State(z: Num):
    def advance(s: Step, digit: Int): State = advance(s.d, s.a, s.b, digit)
    def advance(d: Boolean, a: Int, b: Int, digit: Int): State =
      val x = ((z % 26) + a) != digit
      val z1 = if(d) z / 26 else z
      val z2 = if(x) z1 * 26 + digit + b else z1
      State(z2)
    def allValidNumbers(steps: List[Step], prevDigits: Int, expandBy: Seq[Int]): Seq[Seq[Int]] = steps match {
      case step +: Nil =>
        if (z >= 26) Nil else expandBy.filter(i => advance(step, i).z == 0).map(Seq(_))
      case step +: tail => stateResultCache.getOrElseUpdate((z, prevDigits), calcVal(step, tail, prevDigits + 1, expandBy))
    }
    private def calcVal(step: Step, tail: List[Step], digits: Int, expandBy: Seq[Int]) =
      if (tail == Nil)
        val digit = z.toInt + step.a
        if(digit > 0 && digit < 10) advance(step, digit).allValidNumbers(Nil, digits, expandBy).map(digit +: _) else Nil
      else
        expandBy.flatMap(i => advance(step, i).allValidNumbers(tail, digits, expandBy).map(i +: _))
    def maxValidNumber(steps: List[Step]): Seq[Int] = allValidNumbers(steps, 0, expandBy = 9 to 1 by -1).head
    def minValidNumber(steps: List[Step]): Seq[Int] = allValidNumbers(steps, 0, expandBy = 1 to 9).head

  extension (steps: List[Step])
    def eval(number: Seq[Int]): Num = steps.zip(number).foldLeft(State(0l))((state, step) => state.advance(step._1, step._2)).z

  def main(args: Array[String]): Unit =
    val steps = Step.parse(util.readAllFrom("d24_decoded"))
    val minNumber = State(0l).minValidNumber(steps)
    println(s"Min number: ${minNumber.mkString} => ${steps.eval(minNumber)}")
    stateResultCache.clear()
    val maxNumber = State(0l).maxValidNumber(steps)
    println(s"Max number: ${maxNumber.mkString} => ${steps.eval(maxNumber)}")