/**
  * Created by Jesús Martínez on 10/25/16.
  */
case class Line(normalVector: NumericVector = NumericVector(0,0), constantTerm: Double = 0) {
  require(normalVector.dimension == 2, "A line is bi-dimensional")

  private val tolerance = 1e-10
  private val dimension = 2

  private def getFirstNonZeroIndex = normalVector.coordinates.indexWhere(_ != 0)

  val basePoint: NumericVector = {
    val firstNonZeroIndex = getFirstNonZeroIndex
    val coordinates: Seq[Double] =
      Seq.fill(dimension)(0.0).
        updated(firstNonZeroIndex, constantTerm / normalVector.coordinates(firstNonZeroIndex))
    NumericVector(coordinates: _*)
  }

  /**
    * @return String representation of `this` line with the form Ax1 + Bx2 = k
    */
  override def toString: String = {
    def standardize(term: Double) = s"${if (term % 1 == 1) term.toInt else term}"
    def writeCoefficient(coef: Double, isInitialTerm: Boolean = false) = {
        s"${if (coef < 0) "-" else if (coef > 0 && !isInitialTerm) "+" else ""}" +
          s"${if (!isInitialTerm) " " else ""}" +
            s"${if (math.abs(coef) != 1) standardize(math.abs(coef)) else ""}"
    }

    val initialIndex = getFirstNonZeroIndex

    val leftHandSide = for {
      i <- this.normalVector.coordinates.indices
      isInitialIndex = i == initialIndex
      coef = this.normalVector(i)
      if coef != 0
    } yield s"${writeCoefficient(coef, isInitialIndex)}x_${i + 1}"

    leftHandSide.mkString(" ") + s" = ${standardize(constantTerm)}"
  }

  /**
    * Checks if two lines are parallel to each other.
    * @return True if `this` and `that` are parallel.
    */
  def isParallelTo(that: Line) = normalVector.isParallelTo(that.normalVector)

  /**
    * Checks if two lines are the same.
    * @return True if `this` and `that` are the same line.
    */
  def isEqualsTo(that: Line) = isParallelTo(that) && (this.basePoint - that.basePoint).isOrthogonalTo(normalVector)

  /**
    * Calculates the intersection between two lines.
    * @return None if `this` and `that` are parallel. `this` if the lines are equal and Some(point) otherwise, where
    *         point is the vector that represents the intersection between `this` and `that`.
    */
  def intersectionWith(that: Line): Option[NumericVector] =
    if (isParallelTo(that)) None
    else if (isEqualsTo(that)) Some(this.normalVector)
    else Some {
      val A = normalVector(0)
      val B = normalVector(1)
      val C = that.normalVector(0)
      val D = that.normalVector(1)
      val k1 = constantTerm
      val k2 = that.constantTerm

      NumericVector(D * k1 - B * k2, A * k2 - C * k1) * (1 / (A * D  - B * C))
    }
}
