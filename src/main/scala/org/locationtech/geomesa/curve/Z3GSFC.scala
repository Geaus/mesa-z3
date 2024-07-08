/***********************************************************************
 * Copyright (c) 2013-2024 Commonwealth Computer Research, Inc.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Apache License, Version 2.0
 * which accompanies this distribution and is available at
 * http://www.opensource.org/licenses/apache2.0.php.
 ***********************************************************************/

package org.locationtech.geomesa.curve

import org.locationtech.geomesa.curve.NormalizedDimension._
import org.locationtech.geomesa.curve.Z3GSFC.{Z3GDimensions, StandardZ3GDimensions}
import org.locationtech.geomesa.zorder.sfcurve.{IndexRange, Z3, ZRange}

/**
 * Z3G space filling curve
 *
 * @param dims curve dimensions
 */
class Z3GSFC(dims: Z3GDimensions) extends SpaceFillingCurve3D {

  val lon: NormalizedDimension  = dims.lon
  val lat: NormalizedDimension  = dims.lat
  val height: NormalizedDimension = dims.height

  /**
   * Alternate constructor
   *
   * @param precision bits used per dimension - note all precisions must sum to less than 64
   */
  def this(precision: Int = 21) = this(StandardZ3GDimensions(precision))

  override def index(x: Double, y: Double, h: Double, lenient: Boolean = false): Long = {
    try {
      require(x >= lon.min && x <= lon.max && y >= lat.min && y <= lat.max && h >= height.min && h <= height.max,
        s"Value(s) out of bounds ([${lon.min},${lon.max}], [${lat.min},${lat.max}], [${height.min},${height.max}]): $x, $y, $h")
      Z3(lon.normalize(x), lat.normalize(y), height.normalize(h)).z
    } catch {
      case _: IllegalArgumentException if lenient => lenientIndex(x, y, h)
    }
  }

  protected def lenientIndex(x: Double, y: Double, h: Double): Long = {
    val bx = if (x < lon.min) { lon.min } else if (x > lon.max) { lon.max } else { x }
    val by = if (y < lat.min) { lat.min } else if (y > lat.max) { lat.max } else { y }
    val bh = if (h < height.min) { height.min } else if (h > height.max) { height.max } else { h }
    Z3(lon.normalize(bx), lat.normalize(by), height.normalize(bh)).z
  }

  override def invert(z: Long): (Double, Double, Double) = {
    val (x, y, h) = Z3(z).decode
    (lon.denormalize(x), lat.denormalize(y), height.denormalize(h))
  }

  override def ranges(xyz: Seq[(Double, Double, Double, Double, Double, Double)],
                      precision: Int,
                      maxRanges: Option[Int]): Seq[IndexRange] = {
    val zbounds = for { (xmin, ymin, zmin, xmax, ymax, zmax) <- xyz } yield {
      ZRange(index(xmin, ymin, zmin), index(xmax, ymax, zmax))
    }
    Z3.zranges(zbounds.toArray, precision, maxRanges, Z3GSFC.MaxRecursion)
  }
}

object Z3GSFC {

  private val MaxRecursion = sys.props.get("geomesa.scan.ranges.recurse").map(_.toInt).orElse(Some(Int.MaxValue))

  trait Z3GDimensions {
    def lon: NormalizedDimension
    def lat: NormalizedDimension
    def height: NormalizedDimension
  }

  case class StandardZ3GDimensions(precision: Int = 21) extends Z3GDimensions {

    require(precision > 0 && precision < 22, "Precision (bits) per dimension must be in [1,21]")

    override val lon: NormalizedDimension = NormalizedLon(precision)
    override val lat: NormalizedDimension = NormalizedLat(precision)
    override val height: NormalizedDimension =NormalizedHeight_unequal(precision)
  }
}

