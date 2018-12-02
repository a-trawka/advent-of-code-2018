package day02

import common.puzzleInput

object InventoryManagementSystem extends App {

  val input = puzzleInput("day02.txt")

  // Part 1
  def checksum(input: List[String]): Int = {
    val charCounter: List[Map[Char, Int]] =
      input.map {
        boxID => boxID.groupBy(char => char).mapValues(_.length)
      }

    val filteredOtherThanTwoThree: List[Map[Char, Int]] =
      charCounter.map {
        charMap => charMap.filter { case (_, v) => Seq(2, 3).contains(v) }
      }

    val iterTwosAndThrees: List[Int] =
    // List(Map(char -> 2), Map(char -> 2, char -> 3)) -> List(2, 2, 2, 3, 3, 2, 3, ...)
      filteredOtherThanTwoThree.flatMap {
        charMap => charMap.values.toSet
      }

    val countTwosAndThrees: Iterable[Int] = iterTwosAndThrees
      .groupBy(i => i)
      .values
      .map(_.size)

    countTwosAndThrees.product
  }

  // Part 2
  def differentChars(input: List[String]): String = {

    def commonLetters(input: List[String]): String = {
      val firstBoxID: String = input.head
      val differences: List[IndexedSeq[((Char, Char), Int)]] =
        input.map {
          boxID =>
            (firstBoxID zip boxID)
              .zipWithIndex
              .filter { case ((x, y), _) => x != y }
        }

      val singleCharDifference =
        differences.filter(difference => difference.size == 1)
      if (singleCharDifference.isEmpty) return commonLetters(input.tail)

      val index = singleCharDifference  //: List[IndexedSeq[((Char, Char), Int)]]
        .head                           //: IndexedSeq[((Char, Char), Int)]
        .last                           //: ((Char, Char), Int)
        ._2                             //: Int

      firstBoxID.patch(index, "", 1)
    }

    commonLetters(input)
  }

  println(checksum(input))
  println(differentChars(input))

}
