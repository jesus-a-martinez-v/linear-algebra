/**
  * Created by Jesús Martínez on 10/24/16.
  */
case class NumericVector(coordinates: Double*) {
  require(coordinates.nonEmpty, "Empty coordinates")

  private def tolerance = 1e-10

  def apply(i: Int): Double = coordinates(i)

  /**
    * @return Number of elements of this vector.
    */
  def dimension = coordinates.length

  /**
    * @return String representation of the vector.
    */
  override def toString: String = s"Vector(${coordinates.mkString(", ")})"

  /**
    * Sums two vectors.
    * @param that Vector to be summed with `this`.
    * @return A new Vector that contains the sum, coordinate by coordinate, of `this` and `that`
    */
  def + (that: NumericVector): NumericVector = {
    assert(that.dimension != dimension, "Dimensions don't match")

    NumericVector(coordinates zip that.coordinates map { case (a, b) => a + b }: _*)
  }

  /**
    * Subtracts two vectors.
    * @param that Vector to be subtrated to `this`.
    * @return A new Vector that contains the subtraction, coordinate by coordinate, of `this` and `that`
    */
  def - (that: NumericVector): NumericVector = {
    assert(that.dimension != dimension, "Dimensions don't match")

    NumericVector(coordinates zip that.coordinates map { case (a, b) => a - b }: _*)
  }

  /**
    * Scalar product.
    * @param scalar Scalar to be multiplied with `this`
    * @return A new Vector that contains the scalar product, coordinate by coordinate, of `this` and `scalar
    */
  def * (scalar: Double): NumericVector = NumericVector(coordinates map (_ * scalar): _*)

  /**
    * Scalar product.
    * @param scalar Scalar to be multiplied with `this`
    * @return A new Vector that contains the scalar product, coordinate by coordinate, of `this` and `scalar
    */
  def * (scalar: Float): NumericVector = NumericVector(coordinates map (_ * scalar): _*)

  /**
    * Scalar product.
    * @param scalar Scalar to be multiplied with `this`
    * @return A new Vector that contains the scalar product, coordinate by coordinate, of `this` and `scalar
    */
  def * (scalar: Int): NumericVector = NumericVector(coordinates map (_ * scalar): _*)

  /**
    * Scalar product.
    * @param scalar Scalar to be multiplied with `this`
    * @return A new Vector that contains the scalar product, coordinate by coordinate, of `this` and `scalar
    */
  def * (scalar: Long): NumericVector = NumericVector(coordinates map (_ * scalar): _*)

  /**
    * Dot product of two vectors
    * @param that Vector to be dot multiplied with `this`
    * @return Quantity equals to the dot product of `this` and `that`
    */
  def * (that: NumericVector): Double = coordinates.zip(that.coordinates).foldLeft(0.0)((a, pair) => a + pair._1 * pair._2 )

  /**
    * Calculates the angle of between two vectors.
    * @param that Vector that forms an angle with `this`
    * @param radians Flag used to return the result in radians (true) or degrees (false). Defaults to true.
    * @return The angle between `this` and `that` in the units specified by `radians` flag.
    */
  def angleWith(that: NumericVector, radians: Boolean = true): Double = {
    assert(!this.isZero && !that.isZero, "Can't calculate angle with zero vector")

    val inRadians = math.acos(this.normalized * that.normalized)
    if (radians) inRadians else math.toDegrees(inRadians)
  }

  /**
    * @return True if `this` is the zero vector.
    */
  def isZero = magnitude < tolerance

  /**
    * @return Quantity equals to the magnitude of `this`
    */
  def magnitude = math.sqrt(coordinates.foldLeft(0.0)((a, c) => a + math.pow(c, 2)))

  /**
    * Calculates the normalized vector of `this`
    * @return A new vector that is equals to the normalized version of `this`
    */
  def normalized: NumericVector = {
    assert(!isZero, "Zero vector has no normalized form")
    this * (1 / magnitude)
  }

  /**
    * Checks if two vectors are orthogonal to each other
    * @return True if `this` and `that` are orthogonal.
    */
  def isOrthogonalTo(that: NumericVector): Boolean = isZero || (this * that < tolerance)

  /**
    * Checks if two vectors are parallel.
    * @return True if `this` and `that` are parallel
    */
  def isParallelTo(that: NumericVector) = {
    val angle = angleWith(that)
    isZero || that.isZero || angle == 0 || angle == math.Pi
  }

  /**
    * Calculates a parallel component of `this` to `that`
    * @return A new Vector that is parallel to `that`
    */
  def parallelComponentTo(that: NumericVector) =
    if (this.isZero || that.isZero)
      throw new Exception("There's no unique parallel component to the zero vector.")
    else that.normalized * (this * that.normalized)

  /**
    * Calculates an orthogonal component of `this` to `that`
    * @return A new Vector that is orthogonal to `that`
    */
  def orthogonalComponentTo(that: NumericVector) = this - this.parallelComponentTo(that)

  /**
    * Calculates de cross product of two vectors.
    * @param that Vector to be cross multiplied with `this`
    * @return A new Vector that is orthogonal to both `this` and `that `
    */
  def x (that: NumericVector) = {
    assert(this.dimension == 3 && that.dimension == 3, "Cross product is only defined for 3D vectors.")

    NumericVector(
      this.coordinates(1) * that.coordinates(2) - that.coordinates(1) * this.coordinates(2),
      that.coordinates(0) * this.coordinates(2) - this.coordinates(0) * that.coordinates(2),
      this.coordinates(0) * that.coordinates(1) - that.coordinates(0) * this.coordinates(1)
    )
  }

  /**
    * Returns the area of the parallelogram formed by `this` and `that`
    * @return Quantity equals to the area of the parallelogram formed by `this` and `that`
    */
  def areaOfParallelogram(that: NumericVector) = (this x that).magnitude

  /**
    * Returns the area of the triangle formed by `this` and `that`
    * @return Quantity equals to the area of the triangle formed by `this` and `that`
    */
  def areaOfTriangle(that: NumericVector) = areaOfParallelogram(that) / 2
}
