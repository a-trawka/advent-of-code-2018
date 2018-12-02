package day01

import scala.io.Source

object ChronalCalibration extends App {

  val bufferedSource = Source.fromResource("day01.txt")
  val iter: Iterator[String] = bufferedSource.getLines
  val frequencies: Iterator[Int] = iter.map(_.toInt)

  // Part 1
  def resultingFrequency(input: Iterator[Int]): Int = {
    input.sum
  }

  // Part 2
  def repeatingFrequency(input: Iterator[Int]): Int = {

    val cycleInput = Iterator.continually(input.toList).flatten
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
  bufferedSource.close()
}
