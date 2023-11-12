object task_4_6 extends App {
  def generateTriangle(levels: Int): List[List[Int]] = {
    def generateRow(row: List[Int]): List[Int] =
      (0 +: row :+ 0).sliding(2).map { case List(a, b) => a + b }.toList

    (1 to levels).foldLeft(List(List(1))) { (triangle, _) =>
      triangle :+ generateRow(triangle.last)
    }
  }

  def printTriangle(triangle: List[List[Int]]): Unit = {
    val maxDigits = triangle.last.last.toString.length

    triangle.zipWithIndex.foreach { case (row, rowIndex) =>
      val padding = " " * ((triangle.length - rowIndex - 1) * (maxDigits + 1) / 2)
      val formattedRow = row.map(num => String.format(s"%${2 * maxDigits}d", num)).mkString("  ")
      println(padding + formattedRow)
    }
  }

  def getElement(triangle: List[List[Int]], row: Int, col: Int): Option[Int] = {
    if (row >= 0 && col >= 0 && row < triangle.length && col < triangle(row).length)
      Some(triangle(row)(col))
    else
      None
  }

  def sumRow(triangle: List[List[Int]], row: Int): Option[Int] = {
    getElement(triangle, row, 0).map(_ => triangle(row).sum)
  }

  def isSymmetric(triangle: List[List[Int]]): Boolean = {
    triangle.forall(row => row == row.reverse)
  }


  val levels = 7
  val pascalTriangle = generateTriangle(levels)

  println("Pascal's Triangle:")
  printTriangle(pascalTriangle)

  val row = 4
  val col = 2
  println(s"Element at row $row, column $col: ${getElement(pascalTriangle, row, col)}")

  val sumRowNumber = 4
  println(s"Sum of elements in row $sumRowNumber: ${sumRow(pascalTriangle, sumRowNumber)}")

  println(s"Is Pascal's Triangle symmetric? ${isSymmetric(pascalTriangle)}")


}