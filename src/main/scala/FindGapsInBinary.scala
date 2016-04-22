import scala.annotation.tailrec

object FindGapsInBinary {

  val input = 1041

  def log2(x: Double) = scala.math.log(x)/scala.math.log(2)

  var powers: Array[Int] = Array.empty

  @tailrec
  def intToBinary (input: Int): Array[Int] = input  match {
    case 0 => powers
    case _ =>
      val powerOf2 = log2(input).toInt
      powers = powers :+ powerOf2
      intToBinary(input - scala.math.pow(2, powerOf2).toInt)
  }

  var gap = 0

  @tailrec
  def findGap(input: Array[Int]): Int = input.length match {
    case 1 => gap
    case _ =>
      val a = input(0)
      val b = input(1)
      if ((a - b) > gap) gap = a - b - 1
      findGap(input.slice(1, input.length))
  }

  intToBinary(input)
  println(findGap(powers))

}
