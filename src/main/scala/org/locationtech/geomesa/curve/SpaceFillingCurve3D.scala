package org.locationtech.geomesa.curve

import org.locationtech.geomesa.zorder.sfcurve.IndexRange

trait SpaceFillingCurve3D {

  import SpaceFillingCurve3D.FullPrecision

  def index(x: Double, y: Double, z: Double, lenient: Boolean = false): Long
  def invert(i: Long): (Double, Double, Double)

  def ranges(x: (Double, Double), y: (Double, Double), z: (Double, Double)): Seq[IndexRange] =
    ranges(Seq((x._1, y._1, z._1, x._2, y._2, z._2)), FullPrecision, None)

  def ranges(x: (Double, Double), y: (Double, Double), z: (Double, Double), precision: Int): Seq[IndexRange] =
    ranges(Seq((x._1, y._1, z._1, x._2, y._2, z._2)), precision, None)

  def ranges(x: (Double, Double), y: (Double, Double), z: (Double, Double), precision: Int, maxRanges: Option[Int]): Seq[IndexRange] =
    ranges(Seq((x._1, y._1, z._1, x._2, y._2, z._2)), precision, maxRanges)

  /**
   * Gets ranges
   *
   * @param xyz sequence of bounding boxes, in the form of (xmin, ymin, zmin, xmax, ymax, zmax)
   * @param precision precision of the zvalues to consider, up to 64 bits
   * @param maxRanges rough upper bound on the number of ranges to return
   * @return
   */
  def ranges(xyz: Seq[(Double, Double, Double, Double, Double, Double)],
             precision: Int = FullPrecision,
             maxRanges: Option[Int] = None): Seq[IndexRange]
}

object SpaceFillingCurve3D {
  val FullPrecision: Int = 64
}
