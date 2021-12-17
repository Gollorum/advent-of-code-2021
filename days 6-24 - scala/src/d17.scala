object d17:

  def harmonicSumTo(i: Int): Int = if(i > 0) (i + 1) * i / 2 else 0

  case class HorizontalTrajectory(initialVelocity: Int):
    def reachedAfter(steps: Int): Int = initialVelocity.sign *
      (harmonicSumTo(initialVelocity.abs) - harmonicSumTo(initialVelocity.abs - steps))
    lazy val allReachedDistances: IndexedSeq[(Int, Int)] =
      (0 to initialVelocity.abs).map(steps => (reachedAfter(steps), steps))

  def toInfinity(min: Int): LazyList[Int] = LazyList.cons(min, toInfinity(min + 1))

  case class VerticalTrajectory(initialVelocity: Int):
    lazy val maxReachedHeight: Int = if(initialVelocity > 0) harmonicSumTo(initialVelocity.abs) else 0
    def reachedAfter(steps: Int): Int =
        if(initialVelocity <= 0) harmonicSumTo(-initialVelocity-1) - harmonicSumTo(steps - initialVelocity-1)
        else maxReachedHeight - harmonicSumTo(
          if(steps < initialVelocity) initialVelocity.abs - steps
          else steps - initialVelocity - 1
        )
    def validBeyond(steps: Int, min: Int, max: Int): Boolean =
      val isStillAscending = steps <= initialVelocity.abs
      if(!isStillAscending && reachedAfter(steps) < min) return false
      toInfinity(steps).map(reachedAfter).dropWhile(_ > max).takeWhile(_ >= min).nonEmpty


  def main(args: Array[String]): Unit =
    val lines = util.readAllFrom("d17")
    val (xMin, xMax, yMin, yMax) = lines.head match {
      case s"target area: x=$xMin..$xMax, y=$yMin..$yMax" => (xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt)
    }
    val possibleHorizontals = (0 to xMax)
      .map(HorizontalTrajectory.apply)
      .map(tr => (tr, tr.allReachedDistances.filter(d => d._1 >= xMin && d._1 <= xMax).map(_._2)))
      .filter(_._2.nonEmpty)
    val validVelocities = for {
      initialVerticalVelocity <- yMin until -yMin
      vt = VerticalTrajectory(initialVerticalVelocity)
      (ht, possibleStepsCounts) <- possibleHorizontals
      if possibleStepsCounts.map(c => vt.reachedAfter(c)).exists(y => y >= yMin && y <= yMax)
        || possibleStepsCounts.last == ht.initialVelocity.abs && vt.validBeyond(possibleStepsCounts.last, yMin, yMax)
    } yield (ht, vt)

    println(validVelocities.map(_._2.maxReachedHeight).max)
    println(validVelocities.size)