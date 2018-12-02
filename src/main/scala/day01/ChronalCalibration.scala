package day01

import common.puzzleInput

object ChronalCalibration extends App {

  val frequencies: List[Int] = puzzleInput("day01.txt").map(_.toInt)

  // Part 1
  def resultingFrequency(input: List[Int]): Int = {
    input.sum
  }

  // Part 2
  def repeatingFrequency(input: List[Int]): Int = {
    val cycleInput = Iterator.continually(input).flatten
    val accumulator: Iterator[Int] = cycleInput
      .scanLeft(0)(_ + _)
      .drop(1)

    def firstRepeatingFrequency(accumulatedFreqs: Iterator[Int], seen: List[Int]): Int = {
      val head: Int = accumulatedFreqs.next()
      if (seen contains head) return head
      firstRepeatingFrequency(accumulatedFreqs, head :: seen)
    }

    firstRepeatingFrequency(accumulator, List.empty[Int])
  }


  println(s"Resulting frequency: ${resultingFrequency(frequencies)}")
  println(repeatingFrequency(frequencies))
}
