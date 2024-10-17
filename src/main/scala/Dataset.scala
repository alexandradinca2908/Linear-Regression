import scala.annotation.tailrec
import scala.io.Source
import helpers._

class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m

  //  Write lists as "a, b, c" instead of List(a, b, c)
  //  Lists of lists become one big string where
  //  the initial lists are strings separated by newline
  override def toString: String = {
    listToString(getHeader, ',') + '\n'
      + listToString(getRows.map(listToString(_, ',')), '\n')
  }

  def selectColumn(col: String): Dataset = {
    //  Return column as a row then transpose by making every element a list
    val wantedCol = iterateCols(getHeader, getRows.transpose)(col).map(x => List(x))

    new Dataset(wantedCol)
  }
  def selectColumns(cols: List[String]): Dataset = {
    //  Searches each column separately
    //  Concatenates all found columns into one List[List[String]]
    def searchColumns(cols: List[String]): List[List[String]] = {
      cols match {
        case head :: tail =>
          val result = iterateCols(getHeader, getRows.transpose)(head)

          if (result == Nil) searchColumns(tail)
          else result :: searchColumns(tail)

        case Nil => Nil
      }
    }

    val wantedCols = searchColumns(cols).transpose

    new Dataset(wantedCols)
  }

  def split(percentage: Double): (Dataset, Dataset) = {
    //  Parse every entry and add it to l1/l2 depending on acc
    //  First k elems to l1, k + 1 elem to l2
    @tailrec
    def parseSet(set: List[List[String]], acc: Double)
                (l1: List[List[String]])
                (l2: List[List[String]]): (List[List[String]], List[List[String]]) = {
      set match {
        case Nil => (l1, l2)
        case head :: tail =>
          if (acc < (1 / percentage - 1)) parseSet(tail, acc + 1)(head :: l1)(l2)
          else parseSet(tail, 0)(l1)(head :: l2)
      }
    }

    val sortedSet = getRows.sortBy(_.head)
    val (l1, l2) = parseSet(sortedSet, 0)(Nil)(Nil)

    //  Reverse lists
    val revL1 = l1.foldLeft(Nil: List[List[String]])((xs, x) => x :: xs)
    val revL2 = l2.foldLeft(Nil: List[List[String]])((xs, x) => x :: xs)

    (new Dataset(getHeader :: revL1), new Dataset(getHeader :: revL2))
  }

  def size: Int = data.length
  def getRows: List[List[String]] = data.tail
  def getHeader: List[String] = data.head
}

object Dataset {
  def apply(csv_filename: String): Dataset = {
    //  Create a source val and read the lines from file
    val source = Source.fromFile(csv_filename)

    //  source.getLines returns an Iterator; must be converted
    val m = source.getLines.toList.map(_.split(",").toList)

    //  Create dataset object
    val newDataset : Dataset = new Dataset(m)

    source.close()

    newDataset
  }
  def apply(ds: List[List[String]]): Dataset = new Dataset(ds)
}
