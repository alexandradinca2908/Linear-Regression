import helpers._

type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {

  def transpose: Matrix = {
    //  Transpose function from Interactive Course 6
    def transposeHelper(mat: Mat): Mat = {
      mat match {
        case Nil :: _ => Nil
        case _ => mat.map(_.head) :: transposeHelper(mat.map(_.tail))
      }
    }

    data match {
      case None => new Matrix(None)
      case Some(mat) => new Matrix(Some(transposeHelper(mat)))
    }
  }

  def map(f: Double => Double): Matrix = {
    data match {
      case None => new Matrix(None)
      case Some(mat) =>
        val newMat = mat.foldRight(Nil: Mat)((x, xs) => x.map(f(_)) :: xs)
        new Matrix(Some(newMat))
    }
  }

  def *(other: Matrix): Matrix = {
    if (this.width != other.height) new Matrix(None)
    else {
      (this.data, other.data) match {
        case (None, _) => new Matrix(None)
        case (_, None) => new Matrix(None)
        case (Some(mat1), Some(mat2)) =>
          //  Multiplication formula from course 6
          //  Zip every line from m1 with all columns from m2
          //  Then multiply the tuples and add the results
          val result = mat1.map(line => mat2.transpose.map(col => line.zip(col).map(p =>
                p._1 * p._2).sum))
          new Matrix(Some(result))
      }
    }
  }

  def ++(x: Double): Matrix =
    this.data match {
      case None => new Matrix(None)
      //  Wrap x as list and use list concatenation properties
      //  since :: is for prepending
      case Some(mat) => new Matrix(Some(mat.map(_ ::: List(x))))
    }

  def -(other: Matrix): Matrix =
    if (this.height != other.height || this.width != other.width) new Matrix(None)
    else {
      (this.data, other.data) match {
        case (None, _) => new Matrix(None)
        case (_, None) => new Matrix(None)
        case (Some(mat1), Some(mat2)) =>
          //  We start with 2 List[List[Double]]
          //  Create a list of tuples containing 2 lists => List[(List, List)]
          //  Then merge each tuple resulting in => List[List(Double, Double)]
          //  Lastly get a result from tuples => Double - Double => List[List(Double)]
          val result = mat1.zip(mat2).map((l1, l2) => l1.zip(l2).map((d1, d2) => d1 - d2))
          new Matrix(Some(result))
      }
    }

  def data: Option[Mat] = m
  def height: Option[Int] =
    data match
      case None => None
      case Some(mat) => Some(mat.length)
  def width: Option[Int] =
    data match
      case None => None
      case Some(mat) => Some(mat.head.length)

  override def toString: String = {
    data match {
      case None => ""
      case Some(mat) =>
        val stringL = mat.map(_.map(_.toString()))
        listToString(stringL.map(listToString(_, ' ')), '\n')
    }
  }
}

object Matrix {
  def apply(data: Mat): Matrix =
    data match {
      case Nil => new Matrix(None)
      case _ => new Matrix(Some(data))
    }
  def apply(data: Option[Mat]): Matrix = new Matrix(data)
  def apply(dataset: Dataset): Matrix =
    dataset.data match {
      case Nil => new Matrix(None)
      case head :: tail =>
        val doubleTail = tail.foldLeft(Nil: List[List[Double]])((xs, x) => x.map(_.toDouble) :: xs)
        new Matrix(Some(doubleTail))
    }
}
