object d16:

  sealed abstract class DecodedPacket(version: Int)
  case class Literal(version: Int, n: Long) extends DecodedPacket(version)
  case class Operator(version: Int, t: Int, subPackets: Seq[DecodedPacket]) extends DecodedPacket(version)

  object Literal:
    def parse(s: String, version: Int): (Literal, Int) =
      val (n, digits, _) = s.sliding(5, 5).foldLeft((0l, 0, false))((t, s) =>
        if(t._3) (t._1, t._2, t._3)
        else ((t._1 << 4) + s.drop(1).parseBinary, t._2 + 5, t._3 || s.startsWith("0"))
      )
      (Literal(version, n), digits)

  extension (s: String)
    def parseBinary: Int = Integer.parseInt(s, 2)
    def parseHex: Int = Integer.parseInt(s, 16)
    def hexToBinary: String =
      val raw = s.parseHex.toBinaryString
      Seq.fill(4 - raw.length)("0").mkString ++ raw

  extension (packet: DecodedPacket)
    def traverse[A](onOperator: (Operator, Seq[A]) => A, onLiteral: Literal => A): A = packet match {
      case l: Literal => onLiteral(l)
      case o: Operator => onOperator(o, o.subPackets.map(_.traverse(onOperator, onLiteral)))
    }

  def parse(s: String): (DecodedPacket, String) =
    val version = s.substring(0, 3).parseBinary
    val t = s.substring(3, 6).parseBinary
    t match {
      case 4 =>
        val (l, digits) = Literal.parse(s.substring(6), version)
        (l, s.substring(6 + digits))
      case _ if s.charAt(6) == '0' =>
        val totalSubBits = s.substring(7, 22).parseBinary
        val subPackets = parseAllPackets(s.substring(22, 22 + totalSubBits))
        (Operator(version, t, subPackets), s.substring(22 + totalSubBits))
      case _ =>
        val subPacketCount = s.substring(7, 18).parseBinary
        val (subPackets, rest) = (0 until subPacketCount).foldLeft((Seq[DecodedPacket](), s.substring(18)))((t, _) =>
          val (p, r) = parse(t._2)
          (t._1 :+ p, r)
        )
        (Operator(version, t, subPackets), rest)
    }

  def parseAllPackets(s: String): Seq[DecodedPacket] =
    if(s.isEmpty) return Nil
    val (head, rest) = parse(s)
    head +: parseAllPackets(rest)

  def main(args: Array[String]): Unit =
    for(input <- util.readAllFrom("d16")) {
      val values = input.map(_.toString.hexToBinary)
      val root = parse(values.mkString)._1

      println("Version sum: " + root.traverse[Int]((o, lits) => o.version + lits.sum, l => l.version))
      println("Evaluation: " + root.traverse[Long](
        (o, lits) => o.t match {
          case 0 => lits.sum
          case 1 => lits.product
          case 2 => lits.min
          case 3 => lits.max
          case 5 => if(lits.head > lits(1)) 1 else 0
          case 6 => if(lits.head < lits(1)) 1 else 0
          case 7 => if(lits.head == lits(1)) 1 else 0
        },
        l => l.n
      ))
    }