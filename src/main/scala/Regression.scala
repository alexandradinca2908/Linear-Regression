import scala.annotation.tailrec
import scala.language.postfixOps

object Regression {

  def regression(dataset_file: String,
                attribute_columns: List[String],
                value_column: String,
                test_percentage: Double,
                alpha: Double,
                gradient_descent_steps: Int): (Matrix, Double) = {

    //  Initialize evaluation data
    val dataset: Dataset = Dataset.apply(dataset_file)

    //  Split dataset then select wanted columns
    val (train, validate): (Dataset, Dataset) =
      dataset.split(test_percentage)
    val filteredTrain = train.selectColumns(attribute_columns)
    val filteredValidate = validate.selectColumns(attribute_columns)

    //  Wrap training dataset info as Matrix
    val X = Matrix.apply(filteredTrain) ++ 1.0

    //  Init W as Matrix
    val wRow = X.width match
      case None => List.empty[List[Double]]
      case Some(x) => List.fill(x)(List(0.0))
    val W = Matrix.apply(wRow)

    @tailrec
    def gradientDescent(W: Matrix,
                        acc: Int): Matrix = {
      if (acc <= 0) W
      else {
        //  Estimation calculus
        val estimate = X * W

        //  Error calculus
        val Y = Matrix.apply(train.selectColumn(value_column))
        val err = estimate - Y

        //  Gradient calculus
        val m = err.height match {
          case None => -1
          case Some(x) => x
        }
        val gradient = (X.transpose * err).map(_ / m)

        //  New W
        val newW = W - gradient.map(_ * alpha)
        gradientDescent(newW, acc - 1)
      }
    }

    //  Calculate W after gradient_steps
    val finalW = gradientDescent(W, gradient_descent_steps)

    //  Calculate average error for validation dataset
    val V = Matrix.apply(filteredValidate) ++ 1.0

    //  First get all the errors
    val estimate = V * finalW
    val Y = Matrix.apply(validate.selectColumn(value_column))
    val err = estimate - Y

    //  Get nr of entries
    val entries = err.height match {
      case None => -1
      case Some(x) => x
    }

    //  Get the values from the matrix
    val values = err.data match {
      case None => List.empty[List[Double]]
      case Some(mat) => mat
    }

    //  Calculate the average
    val avg_err = values.foldLeft(0.0)((acc, list) => acc + list.head) / entries

    (finalW, avg_err)
  }

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}