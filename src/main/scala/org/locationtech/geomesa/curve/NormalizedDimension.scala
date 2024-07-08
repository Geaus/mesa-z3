/***********************************************************************
 * Copyright (c) 2013-2024 Commonwealth Computer Research, Inc.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Apache License, Version 2.0
 * which accompanies this distribution and is available at
 * http://www.opensource.org/licenses/apache2.0.php.
 ***********************************************************************/

package org.locationtech.geomesa.curve

/**
  * Maps a double within a known range to an Int in [0, bins)
  */
trait NormalizedDimension {

  /**
    * Min value considered for normalization range
    *
    * @return
    */
  def min: Double

  /**
    * Max value considered for normalizing
    *
    * @return
    */
  def max: Double

  /**
    * Max value to normalize to
    *
    * @return
    */
  def maxIndex: Int

  /**
    * Normalize the value
    *
    * @param x [min, max]
    * @return [0, maxIndex]
    */
  def normalize(x: Double): Int

  /**
    * Denormalize the value in bin x
    *
    * @param x [0, maxIndex]
    * @return [min, max]
    */
  def denormalize(x: Int): Double
}

object NormalizedDimension {

  // 定义地球半径（以米为单位）
  private val EarthRadius = 6371000.0

  class BitNormalizedDimension(val min: Double, val max: Double, precision: Int) extends NormalizedDimension {

    require(precision > 0 && precision < 32, "Precision (bits) must be in [1,31]")

    // (1L << precision) is equivalent to math.pow(2, precision).toLong
    private val bins = 1L << precision
    private val normalizer = bins / (max - min)
    private val denormalizer = (max - min) / bins

    override val maxIndex: Int = (bins - 1).toInt // note: call .toInt after subtracting 1 to avoid sign issues

    override def normalize(x: Double): Int =
      if (x >= max) {
        maxIndex
      } else {
        math.floor((x - min) * normalizer).toInt
      }

    override def denormalize(x: Int): Double =
      if (x >= maxIndex) {
        min + (maxIndex + 0.5d) * denormalizer
      } else {
        min + (x + 0.5d) * denormalizer
      }
  }

  case class NormalizedLat(precision: Int) extends BitNormalizedDimension(-90d, 90d, precision)

  case class NormalizedLon(precision: Int) extends BitNormalizedDimension(-180d, 180d, precision)

  case class NormalizedTime(precision: Int, override val max: Double) extends BitNormalizedDimension(0d, max, precision)

  case class NormalizedHeight_equal(precision: Int) extends BitNormalizedDimension(0d, 10000d, precision)

  case class NormalizedHeight_unequal(precision: Int) extends NormalizedDimension {

    require(precision > 0 && precision <= 21, "Precision (bits) must be in [1, 21]")

    override val min: Double = 0.0
    override val max: Double = calculateMaxHeight(precision)
    override val maxIndex: Int = (1 << precision) - 1

    // 计算特定高度的弧长
    private def arcLength(height: Double): Double = 2 * math.Pi * (EarthRadius + height) / 360

    // 计算给定精度下的最大高度
    private def calculateMaxHeight(precision: Int): Double = {
      var height = min
      val bins = 1 << precision
      for (i <- 0 until bins) {
        height += arcLength(height)
      }
      println(s"max height(presion=$precision, height=$height")
      height
    }

    // 预计算每个分割点的高度
    private val thresholds: Array[Double] = {
      val bins = 1 << precision
      val thresholds = new Array[Double](bins)
      var height = min


      for (i <- 0 until bins) {
        thresholds(i) = height
        height += arcLength(height)
      }
      thresholds
    }

    override def normalize(x: Double): Int = {
      if (x <= min) return 0
      if (x >= max) return maxIndex
      thresholds.indexWhere(x <= _)
    }

    override def denormalize(x: Int): Double = {
      if (x <= 0) return min
      if (x >= maxIndex) return max
      thresholds(x)
    }
  }
}
