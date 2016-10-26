/**
  * Created by jesusmartinez on 10/25/16.
  */
case class Plane(normalVector: NumericVector = NumericVector(0d, 0d, 0d), constantTerm: Double = 0) {
  require(normalVector.dimension == 3, "Planes are tri-dimensional")
  private val tolerance = 1e-10
  val dimension = 3

  private def getFirstNonZeroIndex = normalVector.coordinates.indexWhere(_ != 0)

  val basePoint: NumericVector = {
    val firstNonZeroIndex = getFirstNonZeroIndex
    val coordinates: Seq[Double] =
      Seq.fill(dimension)(0.0).
        updated(firstNonZeroIndex, constantTerm / normalVector.coordinates(firstNonZeroIndex))
    NumericVector(coordinates: _*)
  }

  /**
    * @return String representation of `this` plane with the form Ax1 + Bx2 + Cx3 = k
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
    * Checks if two planes are parallel.
    * @return True if `this` and `that` are parallel.
    */
  def isParallelTo(that: Plane) = normalVector.isParallelTo(that.normalVector)

  /**
    * Checks if two planes are the same.
    * @return True if `this` and `that` are the same plane.
    */
  def isEqualsTo(that: Plane) = isParallelTo(that) && (this.basePoint - that.basePoint).isOrthogonalTo(normalVector)
}
