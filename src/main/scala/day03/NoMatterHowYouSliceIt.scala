package day03

import common.puzzleInput

object NoMatterHowYouSliceIt extends App {

  val input: List[String] = puzzleInput("day03.txt")

  def regexFromInput(claim: String): List[Int] = {
    val regex = raw"""#\d{1,4} @ (\d{1,3}),(\d{1,3}): (\d{1,3})x(\d{1,3})""" r


    claim match {
      case regex(left, top, width, height) => {
        println(s"$claim ... @ $left,$top: ${width}x$height")
        List(left, top, width, height).map(_.toInt)
      }
      case _ => throw new Exception(s"Regex matching failed")
    }
  }

  def claimIndices(left: Int, top: Int, width: Int, height: Int): Seq[(Int, Int)] = {
    (left to left + width)
      .flatMap(x =>
        (top to top + height).map(y => (x, y))
      )
  }

  // Part 1
  def overlappingClaims(input: List[String]): Map[(Int, Int), Int] = {
    input
      .map(regexFromInput)
      .flatMap {
        case List(left, top, width, height) =>
          claimIndices(left, top, width, height)
      }
      .groupBy(identity)
      .mapValues(_.size)
      .filter { case (_, v) => v > 1 }
  }

  val res0 = overlappingClaims(input)
  println(res0)
  println(res0.size)

}
