object d20:

  case class Image(width: Int, height: Int, offPixel: Int, values: Array[Int]):
    def apply(x: Int, y: Int): Int = if(x >= 0 && x < width && y >= 0 && y < height) values(x + y * width) else offPixel
    def applyMapping(mapping: Seq[Int]): Image = Image(
      width + 2, height + 2,
      mapping(List.fill(9)(offPixel).foldLeft(0)((res, b) => (res << 1) + b)),
      (for {
        y <- -1 to height
        x <- -1 to width
        readValues = for {
          y2 <- (y - 1) to (y + 1)
          x2 <- (x - 1) to (x + 1)
        } yield this(x2, y2)
        index = readValues.foldLeft(0)((res, b) => (res << 1) + b)
      } yield mapping(index)).toArray
    )
    def print(): Unit = println(
      (0 until height)
        .map(y => (0 until width)
          .map(x => if(this(x, y) == 0) ' ' else '.').mkString
        ).mkString("\n")
    )
    def onCount: Int = values.sum

  object Image:
    def apply(seq: Seq[Seq[Int]]): Image =
      val height = seq.length
      val width = seq.map(_.length).max
      val array = seq.flatten.toArray
      Image(width, height, 0, array)

  def main(args: Array[String]): Unit =
    val lines = util.readAllFrom("d20")
    val mapping = lines.head.map{case '.' => 0; case '#' => 1}
    val image = Image(lines.drop(2).map(_.map{case '.' => 0; case '#' => 1}))
    println((0 until 2).foldLeft(image)((img, _) => img.applyMapping(mapping)).onCount)
    println((0 until 50).foldLeft(image)((img, _) => img.applyMapping(mapping)).onCount)
