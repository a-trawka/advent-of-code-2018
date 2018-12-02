import scala.io.Source

package object common {

  def puzzleInput(resourceName: String): List[String] = {
    val bufferedSource = Source.fromResource("day01.txt")
    val linesList = bufferedSource.getLines.toList
    bufferedSource.close
    linesList
  }

}
