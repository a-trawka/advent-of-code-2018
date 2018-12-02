import scala.io.Source

package object common {

  def puzzleInput(resourceName: String): List[String] = {
    val bufferedSource = Source.fromResource(resourceName)
    val linesList = bufferedSource.getLines.toList
    bufferedSource.close
    linesList
  }

}
