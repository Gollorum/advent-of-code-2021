object d18:

  enum TupleDir:
    case Left, Right

  import TupleDir._

  extension (path: Seq[TupleDir])
    def left: Option[Seq[TupleDir]] =
      if(path.isEmpty) None
      else path.tail.left.map(path.head +: _).orElse(path.head match {
        case Left => None
        case Right => Some(Left +: path.tail.map(_ => Right))
      })
    def right: Option[Seq[TupleDir]] =
      if(path.isEmpty) None
      else path.tail.right.map(path.head +: _).orElse(path.head match {
        case Right => None
        case Left => Some(Right +: path.tail.map(_ => Left))
      })
    def fitOn(num: ShellfishNumber, dir: TupleDir): Seq[TupleDir] = num match {
      case _: Literal => Nil
      case t: Tuple =>
        if(path.isEmpty) dir +: path.fitOn(t(dir), dir)
        else path.head +: path.tail.fitOn(t(path.head), dir)
    }

  sealed trait ShellfishNumber:
    def magnitude: Int
    def toExplodeCoords(path: Seq[TupleDir] = Nil): Option[(Seq[TupleDir], (Literal, Literal))]
    def toSplitCoords(path: Seq[TupleDir] = Nil): Option[(Seq[TupleDir], Literal)]
    def +(other: ShellfishNumber): ShellfishNumber = Tuple(this, other).reduce
    def apply(path: Seq[TupleDir]): ShellfishNumber
    def where(path: Seq[TupleDir], num: ShellfishNumber => ShellfishNumber): ShellfishNumber
    def where(path: Option[Seq[TupleDir]], num: ShellfishNumber => ShellfishNumber): ShellfishNumber = path match {
      case Some(value) => this.where(value, num)
      case None => this
    }

    def reduce: ShellfishNumber = toExplodeCoords() match {
      case Some((explodeAt, (l, r))) =>
        val leftPath = explodeAt.left.map(_.fitOn(this, Right))
        val rightPath = explodeAt.right.map(_.fitOn(this, Left))
        this.where(leftPath, _.asInstanceOf[Literal] + l.n)
          .where(explodeAt, _ => Literal(0))
          .where(rightPath, _.asInstanceOf[Literal] + r.n)
          .reduce
      case None => toSplitCoords() match {
        case Some((splitAt, lit)) =>
          this.where(splitAt, _ => Tuple(Literal(lit.n / 2), Literal(lit.n - lit.n / 2)))
            .reduce
        case None => this
      }
    }

  case class Literal(n: Int) extends ShellfishNumber:
    override def magnitude: Int = n
    override def toExplodeCoords(path: Seq[TupleDir]): Option[(Seq[TupleDir], (Literal, Literal))] = None
    override def toSplitCoords(path: Seq[TupleDir]): Option[(Seq[TupleDir], Literal)] =
      if(n >= 10) Some((path, this)) else None
    override def apply(path: Seq[TupleDir]): ShellfishNumber =
      this
    def where(path: Seq[TupleDir], num: ShellfishNumber => ShellfishNumber): ShellfishNumber =
      num(this)
    def +(m: Int): Literal = Literal(n + m)
    override def toString: String = n.toString

  case class Tuple(left: ShellfishNumber, right: ShellfishNumber) extends ShellfishNumber:
    override def magnitude: Int = 3 * left.magnitude + 2 * right.magnitude
    override def toExplodeCoords(path: Seq[TupleDir]): Option[(Seq[TupleDir], (Literal, Literal))] =
      if(path.length < 4) left.toExplodeCoords(path :+ Left) orElse right.toExplodeCoords(path :+ Right)
      else Some((path, (left.asInstanceOf[Literal], right.asInstanceOf[Literal])))
    override def toSplitCoords(path: Seq[TupleDir]): Option[(Seq[TupleDir], Literal)] =
      left.toSplitCoords(path :+ Left) orElse right.toSplitCoords(path :+ Right)
    def apply(dir: TupleDir): ShellfishNumber = dir match {
      case Left => left
      case Right => right
    }
    override def apply(path: Seq[TupleDir]): ShellfishNumber =
      if(path.isEmpty) this else this(path.head)(path.tail)
    def where(path: Seq[TupleDir], num: ShellfishNumber => ShellfishNumber): ShellfishNumber =
      if(path.isEmpty) num(this) else path.head match {
        case Left => Tuple(left.where(path.tail, num), right)
        case Right => Tuple(left, right.where(path.tail, num))
      }
    override def toString: String = s"[$left,$right]"

  object Literal:
    def unapply(s: String): Option[Int] = s.toIntOption

  object Tuple:
    def unapply(s: String): Option[(ShellfishNumber, ShellfishNumber)] =
      if(!s.startsWith("[")) None
      else
        val countList = s.foldLeft(Seq(0))((d, c) => c match {
          case '[' => d :+ (d.last + 1)
          case ']' => d :+ (d.last - 1)
          case _ => d :+ d.last
        })
        val splitAtIdx = countList.drop(2).indexOf(1) + 2
        val lStr = s.substring(1, splitAtIdx)
        val rStr = s.substring(splitAtIdx + 1, s.length - 1)
        (lStr, rStr) match {
          case (ShellfishNumber(l), ShellfishNumber(r)) => Some(l, r)
          case _ => None
        }

  object ShellfishNumber:

    def unapply(s: String): Option[ShellfishNumber] = s match {
      case Literal(n) => Some(Literal(n))
      case Tuple(l, r) => Some(Tuple(l, r))
      case _ => None
    }

    def parse(s: String): ShellfishNumber = unapply(s).get

  def main(args: Array[String]): Unit =
    val lines = util.readAllFrom("d18")
    val summands = lines.map(ShellfishNumber.parse)
    val result = summands.tail.foldLeft(summands.head)((s1, s2) => s1 + s2)
    println(result.magnitude)
    println((for {
      l <- summands
      r <- summands
    } yield (l + r).magnitude).max)