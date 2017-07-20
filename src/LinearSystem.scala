/**
  * Created by jesusmartinez on 10/25/16.
  */
case class LinearSystem(planes: Plane*) {
  require(planes.nonEmpty, "planes empty")

  val dimension = planes.head.dimension
  val length = planes.length

  require(planes.forall(_.dimension == dimension), "All planes must be of the same dimension")

  private def get(i: Int): Plane = this.planes(i)
  private def updated(i: Int, plane: Plane): LinearSystem = LinearSystem(this.planes.updated(i, plane): _*)

  /**
    * @return String representation of this linear system:
    *         Linear System:
    *         Equation 0: A0x1 + B0x2 + C0x3 = k0
    *         Equation 1: A1x1 + B1x2 + C1x3 = k1
    *         .
    *         .
    *         .
    *         Equation n: Anx1 + Bnx2 + Cnx3 = kn
    */
  override def toString: String =
    "Linear System:\n" + planes.zipWithIndex.map { case (p, i) => s"Equation $i: $p" }.mkString("\n")

  // Swaps two rows and returns the new system.
  private def swapRows(firstRowIndex: Int, secondRowIndex: Int): LinearSystem = {
    val rowOne = this.get(firstRowIndex)
    val rowTwo = this.get(secondRowIndex)
    this.updated(firstRowIndex, rowTwo).updated(secondRowIndex, rowOne)
  }

  // Multiplies a row by a scalar value and returns the new system.
  private def multiplyCoefficientAndRow(coefficient: Double, rowIndex: Int): LinearSystem = {
    val oldRow = this.get(rowIndex)
    val newRow = Plane(oldRow.normalVector * coefficient, oldRow.constantTerm * coefficient)
    this.updated(rowIndex, newRow)
  }

  // Adds a `coefficient` times to another and returns the new system.
  private def addMultipleTimesRowToRow(coefficient: Double, rowToAddIndex: Int, rowToBeAddedToIndex: Int): LinearSystem = {
    val row = multiplyCoefficientAndRow(coefficient, rowToAddIndex).get(rowToAddIndex)
    val oldRow = this.get(rowToBeAddedToIndex)
    val newRow = Plane(row.normalVector + oldRow.normalVector, row.constantTerm + oldRow.constantTerm)

    this.updated(rowToBeAddedToIndex, newRow)
  }

  def computeTriangularForm: LinearSystem = ???

}
